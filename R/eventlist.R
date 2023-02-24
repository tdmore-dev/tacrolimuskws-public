as_administration <- function(x) {
  x$dosis <- list(x$dosis)
  if(!is.null(x$nota)) {
    x$recommendation <- stringr::str_match(x$nota, "Berekende suggestie: ([\\d\\.]+)")[1,2]
    # 11bcdd22-9f14-4947-acfd-ebeaf2943660
    x$recommendationId <- stringr::str_match(x$nota, "id=(.*)&nonce=")[1,2]
  } else {
    x$recommendation <- NA
    x$recommendationId <- NA
  }
  as_tibble(x)
}

#' Convert a JSON request from KWS into a data.frame.
#' 
#' @details
#' No sanity checking is performed. We simply try to capture all information into the output data.frame
#' See [modifyEvents()] for a function to correct small issues in the eventlist before passing it on.
#' See [checkEvents()] for a method that checks the input for content correctness.
#' 
#' @return A data.frame with following columns:
#' * `event` one of ingreep, patient, request, thuistherapie, voorbereiding, voorschrift, uitvoering, labo, zorg
#' * `id` unique identifier for the event. You can assume this is not duplicated (any duplication is an error in the request from KWS side).
#' Beware, for the `patient` row, the id is empty (NA). For `labo` values, the `id` is used as a type identifier, not a unique identifier!
#' * `voorschriftNr` foreign key to link an uitvoering to the corresponding voorschrift. Can be empty in extreme cases.
#' * `time` POSIXCt of the time the event took place. Only `patient` does not have a time. 
#' In rare cases, thuistherapie may also not have a `time`, for drugs the patient is taking 'since forever'
#' * `datumTot` in the case of thuistherapie or infusions
#' * `omschrijving` free-text field; not present for patient, request and zorg
#' 
#' There are specific fields, depending on the type of event:
#' * For `patient`:
#'   * All extra information for the patient is in a named list `attributen` (e.g. `geboorteDatum`, `geslacht`)
#' * For `thuistherapie`, `voorschrift`, `voorbereiding` or `uitvoering`:
#'   * `cnk` pseudo-code called "CNK" describing the drug administered. Note that these CNK codes are internally used within the hospital,
#'   and are often already taken off the market. Use the old BCFI databases to track these codes back to the 
#'   corresponding drug / compound, or use the `omschrijving` field instead.
#'   * `dosis` with a named list (`value`, `ucumUnit`) for the dose itself. Use `tidyr::unnest_wider` to expand this into two columns.
#'   * `frequentie`, `frequentieEenheid` describing how often the drug should be taken, for thuistherapie
#' * For `labo`:
#'   * `attributen` named list with values `numeriek`, `ucumUnit`, `soort`
#' * For `zorg`:
#'   * `definitieCode` with the type of measurement
#'   * `attributen` the specific values of this measurement. Use `tidyr::unnest_wider` or `tidyr::hoist` to convert to columns.
#' 
#' @md
#' @importFrom dplyr select bind_rows mutate mutate_at everything
#' @importFrom tibble as_tibble tibble
#' @export
as.eventlist <- function(x) {
  explicit_na <- function(x) {
    if(is.list(x)) {
      lapply(x, explicit_na)
    } else {
      if(is.null(x)) NA else x
    }
  }
  
  x <- explicit_na(x)
  
  ingreep <- purrr::map_dfr(x$ingreep, . %>% as_tibble)
  x$ingreep <- NULL
  patient <- if(is.null(x$patient)) tibble(attributen=list()) else tibble(attributen=x$patient)
  x$patient <- NULL
  x$request$trigger <- list(x$request$trigger)
  request <- as_tibble(x$request)
  x$request <- NULL
  
  thuistherapie <- purrr::map_dfr(x$thuistherapie, as_administration)
  x$thuistherapie <- NULL
  voorbereiding <- purrr::map_dfr(x$voorbereiding, as_administration)
  x$voorbereiding <- NULL
  voorschrift <- purrr::map_dfr(x$voorschrift, as_administration)
  x$voorschrift <- NULL
  uitvoering <- purrr::map_dfr(x$uitvoering, as_administration)
  x$uitvoering <- NULL
  labo <- purrr::map_dfr(x$labo, function(x) {
    core <- c("id", "datum", "omschrijving")
    noncore <- setdiff(names(x), core)
    
    result <- x[ core ]
    
    # ensure at least 'numeriek' and 'ucumUnit' are present
    defaults <- list(numeriek=numeric(), ucumUnit=character(), soort=character())
    missing <- setdiff(names(defaults), noncore)
    result$attributen <- list( c(x[ noncore ], defaults[missing]) )
    as_tibble(result)
  })
  x$labo <- NULL
  to_zorg <- function(foo){
    names <- purrr::map( foo$attributen, ~.x$attribuutDefinitieCode )
    values <- purrr::map(foo$attributen, ~.x$waarde )
    names(values) <- names
    foo$attributen <- list(values)
    
    as_tibble(foo)
  }
  zorg <- purrr::map_dfr(x$zorg, to_zorg)
  x$zorg <- NULL
  
  ## TODO: include studie details
  x$studie <- NULL
  
  stopifnot( length(x) == 0) #x should be empty now
  
  # include default versions, to ensure all columns exist
  ingreep <- bind_rows(ingreep, tibble(datum=character(), idType=character(), id=character(), omschrijving=character()))
  patient <- bind_rows(patient, tibble(attributen = list()))
  request <- bind_rows(request, tibble(id=character(), time=character()))
  thuistherapie <- bind_rows(thuistherapie, tibble(lijnNr=numeric(), cnk=character(), omschrijving=character(), datumVan=character(), datumTot=character(), frequentie=numeric(), frequentieEenheid=character(), dosis=list())) %>%
    dplyr::mutate_at(dplyr::vars(lijnNr), as.character)
  
  makeCharacter <- function(x, name) {
    name <- rlang::enquo(name)
    name <- rlang::as_label(name)
    if(name %in% colnames(x)) {
      x[[name]] <- as.character(x[[name]])
    }
    x
  }
  
  defaultDose <- tibble(voorschriftNr=character(), cnk=character(), omschrijving=character(), datumVan=character(), datumTot=character(), dosis=list(), nota=character())
  voorbereiding <- voorbereiding %>% 
    makeCharacter(voorschriftNr) %>% bind_rows(defaultDose)
  voorschrift <- voorschrift %>%
    makeCharacter(voorschriftNr) %>% bind_rows(defaultDose)
  uitvoering <- uitvoering %>%
    makeCharacter(uitvoeringNr) %>% makeCharacter(voorschriftNr) %>% bind_rows(cbind(defaultDose, uitvoeringNr=character()))
  labo <- bind_rows(labo, tibble(id=character(), datum=character(), omschrijving=character(), attributen=list()))
  zorg <- zorg %>% 
    makeCharacter(zorgNr) %>% bind_rows(tibble(zorgNr=character(), datum=character()))
  
  events <- bind_rows(
    ingreep=ingreep %>% dplyr::rename(time=.data$datum),
    patient=patient,
    request=request,
    thuistherapie=thuistherapie %>% dplyr::rename(time=.data$datumVan, id=.data$lijnNr),
    voorbereiding=voorbereiding %>% dplyr::rename(time=.data$datumVan, id=.data$voorschriftNr),
    voorschrift=voorschrift %>% dplyr::rename(time=.data$datumVan, id=.data$voorschriftNr),
    uitvoering=uitvoering %>% dplyr::rename(time=.data$datumVan, id=.data$uitvoeringNr),
    labo=labo %>% dplyr::rename(time=.data$datum),
    zorg=zorg %>% dplyr::rename(time=.data$datum, id=.data$zorgNr),
    .id="event") %>% select( -idType ) %>%
    select(event, 
           id, 
           voorschriftNr,
           time, datumTot,
           omschrijving, everything())
  
  anytime <- function(x) {
    if(is.null(x)) return(NA)
    anytime::anytime(x)
  }
  events <- events %>% dplyr::mutate_at(dplyr::vars(time, datumTot, validatieDatum), anytime)
  
  class(events) <- c("eventlist", class(events))
  
  events
}

is.eventlist <- function(x) inherits(x, "eventlist")

eventType <- list(
  ingreep="ingreep",
  patient="patient",
  request="request",
  thuistherapie="thuistherapie",
  voorbereiding="voorbereiding",
  voorschrift="voorschrift",
  uitvoering="uitvoering",
  labo="labo",
  zorg="zorg"
)

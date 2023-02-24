#' Take an event list and convert it to tdmore-compatible data.frame objects
#' @param events an eventlist that has been fully checked for errors
#' @return list with attributes `t0`, `now`, `observed`, `covariates`, `regimen`
#' @importFrom dplyr arrange
#' 
eventlistToTdmore <- function(model, events) {
  stopifnot(is.eventlist(events))
  events <- events %>% filter(event != "thuistherapie")
  
  ## Construct tdmore-compatible data.frames
  result <- list()
  result$t0 <- min(events$time, na.rm=TRUE)
  events$time <- as.numeric( difftime(events$time, result$t0, units="hours") )
  now <- events$time[ events$event == "request" ]
  result$now <- now
  # construct 'observed' data.frame
  labo <- events %>% 
    filter(event=="labo") %>%
    tidyr::unnest_wider(attributen) %>%
    bind_rows(tibble::tibble(numeriek=numeric(), ucumUnit=character(), soort=character())) %>% #ensure right columns, even if no rows exist
    select(time, omschrijving, numeriek) %>%
    tidyr::pivot_wider(names_from=omschrijving, values_from=numeriek) %>%
    bind_rows(tibble::tibble(`Hematocriet (bloed)`=numeric(), `Tacrolimus (FK 506) (bloed)`=numeric())) #ensure right columns present, even if empty
  zorg <- events %>%
    filter(event=="zorg") %>%
    select(time, attributen) %>%
    tidyr::unnest_wider(attributen)
  if("FPgewichtW" %in% colnames(zorg)) zorg[["FPgewichtW"]] <- as.numeric(zorg[["FPgewichtW"]])
  zorg <- zorg %>%
    bind_rows(tibble::tibble(FPgewichtW=numeric())) #ensure right columns, even if no rows exist
  
  result$covariates <- bind_rows(labo, zorg) %>%
    transmute(
      TIME=time,
      HCT=`Hematocriet (bloed)`,
      WT=FPgewichtW
    ) %>% bind_rows(tibble(TIME=0)) %>% #add time '0'
    arrange(TIME) %>%
    tidyr::fill(everything(), .direction="downup")
  result$observed <- labo %>%
    transmute(
      TIME=time,
      CONC=`Tacrolimus (FK 506) (bloed)`
    ) %>% arrange(TIME) %>% filter(! is.na(CONC) )
  
  modelOutput <- tdmore::getMetadataByClass(model, "tdmore_output")$name
  if(!is.null(modelOutput)) colnames(result$observed) <- c("TIME", modelOutput)
  
  result$regimen <- events %>%
    filter( (event == eventType$voorbereiding & time > now) |
             (event == eventType$voorschrift & time > now) |
             event == eventType$uitvoering ) %>%
    tidyr::unnest_wider(dosis) %>%
    bind_rows(tibble::tibble(value=numeric(), ucumUnit=character())) %>% #ensure right columns are present, even if no dose rows
    transmute(
      KWS_ID=id,
      TIME=time,
      AMT=value,
      FORM=omschrijving,
      FIX=fix
    ) %>%
    arrange(TIME)
  result$regimen$OCC <- match(result$regimen$TIME, unique(result$regimen$TIME))
  
  result
}

#' Compute recommendation(s) (core method) for the given KWS patient, model and current time.
#'
#' @md
#' @param patient the patient data as sent by the KWS, as a named list
#' @param modelName the name of the model to be used; should be available in `inst/models/XXXX.R`
#' @return a named list with the following arguments:
#' * `events` list of events from the parsed JSON patient
#' * `model` the tdmore model object
#' * `tdmoreArgs` a named list with arguments for 
#' * `fit` the tdmorefit object
#' * `recommendation` the recommendation object
#' * `updatedPrescriptions` a list of objects list(voorschriftNr=XXXX, dosis=list(value=X, unit=Y))
#' 
#' @param par starting coefficient values
#' @param robust if TRUE, try our very best to return without errors, even in case of unusual circumstances
#' 
#' @importFrom dplyr filter first mutate pull transmute left_join
#' @importFrom lubridate dhours dminutes
#' @export
#'
computeRecommendationCore <- function(patient, modelDir=system.file("models", package="tacrolimuskws"), modelName=tdmore::defaultModel(dir=modelDir), par=NULL, control=NULL, robust=FALSE) {
  r <- list() #the resulting object
  
  r$events <- as.eventlist(patient)
  r$events <- fixEvents(r$events)
  if(!robust) checkEvents(r$events) #check the event list
  
  # Select model
  r$model <- tdmore::getModel(modelName, dir=modelDir)
  r$tdmoreArgs <- eventlistToTdmore(r$model, r$events)
  if(all(r$tdmoreArgs$regimen$FIX)) warning$noPrescriptionToUpdate()

  if (is.null(r$model)) {
    #stop here, building a fit not possible without a model
    if(robust) return(r) else exception$unknownModelException()
  }
  
  r$fit <- tdmore:::estimate(r$model, 
                           observed=r$tdmoreArgs$observed, 
                           regimen=r$tdmoreArgs$regimen, 
                           covariates=r$tdmoreArgs$covariates, 
                           par=par,
                           se.fit=FALSE,
                           control=control)
  r$recommendation <- tdmore::findDoses(r$fit, regimen=r$tdmoreArgs$regimen)
  
  r$updatedPrescriptions <- purrr::pmap(
    list(r$recommendation$regimen$KWS_ID, r$recommendation$regimen$AMT), 
    function(id, amt) {
      event <- r$events[ match(id, r$events$id), ]
      # {
      #   "voorschriftNr": XXXXXXX,
      #   "cnk": "XXXXXX",
      #   "omschrijving": "PROGRAFT",
      #   "datumVan": "2019-10-13T08:00:00+02:00",
      #   "datumTot": "2019-10-13T08:00:00+02:00",
      #   "dosis": {
      #     "value": X,
      #     "ucumUnit": "mg"
      #   }
      # }
      
      list(
        voorschriftNr=as.numeric(event$id), #KWS expects a numeric ID
        dosis=list(
          value=amt,
          ucumUnit="mg"
        ),
        nota = paste0("<html>Berekende suggestie: ", 
                         amt, " dosis_mg ",
                         "<a href=\"$SHINY_URL\">meer info</a></html>")
        )
    })
  
  # Always send all recommendations
  changed <- which(!r$recommendation$regimen$FIX)
  #changed <- which( recommendation$regimen$AMT != tdmore$regimen$AMT )
  r$updatedPrescriptions <- r$updatedPrescriptions[changed]
  
  r <- amendWithAdvagrafAlternative(r, par=par, control=control)
  
  return(r)
}

amendWithAdvagrafAlternative <- function(r, par, control) {
  changedId <- purrr::map_chr(r$updatedPrescriptions, ~.x$voorschriftNr)
  changed <- r$recommendation$regimen$KWS_ID %in% changedId
  if( all( r$recommendation$regimen$FORM[changed] == "PROGRAFT") ) {
    # adapt to a recommendation of ADVAGRAF
    newRegimen <- r$tdmoreArgs$regimen
    i <- max(which(newRegimen$FIX == FALSE))
    if(is.finite(i)) {
      newRegimen <- newRegimen[ seq(1, i), ]
      newRegimen$FORM[i] <- "ADVAGRAF"
    }
    
    advagrafFit <- tdmore:::estimate(r$model, 
                                     observed=r$tdmoreArgs$observed, 
                                     regimen=newRegimen, 
                                     covariates=r$tdmoreArgs$covariates, 
                                     par=par,
                                     se.fit=FALSE,
                                     control=control)
    advagrafRecommendation <- tdmore::findDoses(advagrafFit, regimen=newRegimen)
    
    r$updatedPrescriptions <- lapply(r$updatedPrescriptions, function(x) {
      #if(x$voorschriftNr == newRegimen$KWS_ID[i]) {  ## 29-SEP-2021: add this suggestion to ALL post-it notes
        x$nota <- stringr::str_replace(x$nota, stringr::fixed("</html>"), 
                                       paste0("Indien switch ADVAGRAF: ", advagrafRecommendation$regimen$AMT[i], "mg/dag</html>")
        )
      #}
      x
    })
  }
  return(r)
}
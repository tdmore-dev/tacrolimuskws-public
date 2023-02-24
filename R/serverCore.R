supplementWithDb <- function(events, db) {
  history <- unique(events$recommendationId) %>% na.omit()
  getRecommendation <- function(id) {
    request <- getRequest(id, db)
    if(is.null(request$recommendation$uitvoering)) {
      warning("Voorschrift refers to database ID ", id, " with empty recommendation...")
      return(tibble(voorschriftNr=character(), recommendationId=character()))
    } 
    request$recommendation$uitvoering %>%
      purrr::map_dfr(as_administration) %>%
      transmute(recommendationId=id, 
                voorschriftNr=as.character(voorschriftNr), 
                historyRecommendation = purrr::map_dbl(dosis, . %>% .$value))
  }
  historyDf <- purrr::map_dfr(history, getRecommendation) %>% bind_rows(
    tibble(voorschriftNr=character(), recommendationId=character())
  )
  dplyr::left_join(events, historyDf, by=c(id="voorschriftNr", recommendationId="recommendationId"))
}


#' Translates a KWS request into a tdmore-compatible data structure,
#' asks for a dose recommendation,
#' and then translates the recommendation back into a KWS-compatible data structure.
#' 
#' If an error occurs, this method returns an error data structure instead.
#' 
#' As a side-effect, the tdmore-compatible data structures are persisted in a database
#' for future consultation by a Shiny app.
#'
#' @param requestJson the KWS request as a JSON string
#' @return all tacrolimus prescriptions
#' @export
#'
computeRecommendation <- function(requestJson, db=NULL) {
  stopifnot(is.character(requestJson))
  if(length(requestJson) > 1) requestJson <- paste(requestJson, collapse="\n")
  
  # Prepare JSON validator
  jsonValidator <- jsonvalidate::json_validator(schema=system.file(package="tacrolimuskws", file="kws-validation-schema.json"))

  if(!is.character(requestJson)) stop("Expecting character vector as requestJson")
  
  # Json schema validation
  tryCatch({
    valid <- jsonValidator(json=requestJson, verbose=T)
    if (!isTRUE(valid)) {
      error <- as.character(attributes(valid))
      exception$jsonSchemaValidationException(paste("JSON schema validation error:", error))
    }
  }, error = function(error) {
    exception$jsonSchemaValidationException(paste("JSON schema validation error:", error))
  })
  
  # Parse request
  request <- rjson::fromJSON(requestJson)
  modelName <- request$model %||% "D0_14_Troughs_1cpt_BASE_TSFD_MPC"
  
  # Included in study?
  studie <- request$studie[[1]]
  studie <- as_tibble(studie)
  
  if(isTRUE(any(studie$studieNr == "63463"))) {
    if(stringr::str_detect(studie$subdeel, stringr::regex("TEST", ignore_case=T)) ||
       stringr::str_detect(studie$subdeel, stringr::regex("INTERVENTION", ignore_case=T))
      ) {
      ## ok, do recommendation
      updatedPrescriptions <- list()
      if(!is.null(db)) {
        # clean copy-paste artifacts
        events <- as.eventlist(request)
        events <- supplementWithDb(events, db)
        i <- which(
          purrr::map2_lgl(events$recommendation, events$historyRecommendation, identical)
        )
        updatedPrescriptions <- purrr::map(i, function(i) {
          list(
            voorschriftNr = as.numeric( events$id[ i ] ),
            dosis=list(
              value=NA,
              ucumUnit="mg"
            ),
            nota="Berekende suggestie: nog niet beschikbaar"
          )
        })
      }
      
      trigger <- request$request$trigger
      if(setequal(names(trigger), c("datum", "idType", "id", "numeriek", "soort", "ucumUnit")) && trigger$id == 859) {
        recommendation <- computeRecommendationCore(patient=request, modelName=modelName)
        
        thisPrescriptions <- recommendation$updatedPrescriptions
        names(thisPrescriptions) <- purrr::map_dbl(thisPrescriptions, function(x){x$voorschriftNr})
        names(updatedPrescriptions) <- purrr::map_dbl(updatedPrescriptions, function(x){x$voorschriftNr})
        updatedPrescriptions[ names(thisPrescriptions) ] <- thisPrescriptions
        names(updatedPrescriptions) <- NULL #and then remove the names again
        recommendation$updatedPrescriptions <- updatedPrescriptions #overwrite existing resets where applicable
        
        return(recommendation)
      } else {
        warning$notTacTrigger(trigger)
        return(list(updatedPrescriptions=updatedPrescriptions) )
      }
    } else if (stringr::str_detect(studie$subdeel, stringr::regex("CONTROL", ignore_case=TRUE))) {
      warning$noIntervention(studie)
      return(list()) # do nothing for control arm
    } else {
      exception$invalidSubdeel(studie) ## this is an exception!
    }
  } else {
    exception$notInStudy(studie)
  }
}
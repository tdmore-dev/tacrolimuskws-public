## This file describes all routines used for verifying validity of a KWS request
## It is complemented by the kws-validation-schema.json in inst/, ensuring syntactic correctness of the JSON request
## 

kwsWarning <- function(message, codeName, ..., call=sys.call(-1)) {
  x <- warningCondition(message=message, codeName=codeName, ..., class="kws.warning", call=call)
  warning(x)
}
kwsException <- function(message, codeName, ..., call=sys.call(-1)) {
  x <- errorCondition(message=message, codeName=codeName, ..., class="kws.exception", call=call)
  stop(x)
}

warning <- list(
  oldMeasuresDiscardedWarning=function() {
    kwsWarning("Some measures occur before the first dose and have been discarded", "OLD_MEASURES_DISCARDED")
  },
  oldUnvalidatedDosesDiscardedWarning=function() {
    kwsWarning("Some old unvalidated prescriptions have been discarded", "OLD_UNVALIDATED_DOSES_DISCARDED")
  },
  executionWithoutPrescription=function(events) {
    kwsWarning(paste0("A dose was administered without an associated prescription:\n", str(events)), "EXECUTION_WITHOUT_PRESCRIPTION")
  },
  unknownCNKNumberWarning=function(cnk) {
    kwsWarning(paste("CNK number", cnk, "is unknown"), "UNKNOWN_CNK_NUMBER")
  },
  incorrectDoseUnitWarning=function(events) {
    kwsWarning(paste0("Some doses do not have the right unit (mg)\n",str(events)), "INCORRECT_DOSE_UNIT")
  },
  incorrectMeasureUnitWarning=function(events) {
    kwsWarning(paste0("Some measures do not have the right unit\n", str(events)), "INCORRECT_MEASURE_UNIT")
  },
  movedTroughLaboTime=function(labo) {
    kwsWarning(paste0("Labo within 3 hours post-dose detected, adapting to closest uitvoering time -10min:\n",
                      str(labo)), "MOVED_LABO")
  },
  movedFutureLaboTime=function(labo, newtime) {
    kwsWarning(paste0("Labo result from the future:\n",
                      str(labo),
                      "\n moving to request time ", newtime), "MOVED_LABO")
  },
  notTacrolimusWarning=function(administrations) {
    kwsWarning(paste0("Non-tacrolimus administrations detected, ignoring...\n",
                      str(administrations)
                      ), "NOT_TACROLIMUS")
  },
  ignoringZeroDoseInPast=function(events) {
    kwsWarning(paste0("Dose of 0 in the past, considering as 'uitgevoerd'...\n",
                      str(events)
    ), "ZERO_DOSE_IN_PAST")
  },
  duplicateLabo=function(events) {
    kwsWarning(paste0("Duplicate labo values, ignoring the following (double) values:\n",
                      str(events)
    ), "DUPLICATE_LABO")
  },
  duplicateTreatment=function(events) {
    kwsWarning(paste0("Duplicate treatment values, ignoring the following (double) values:\n",
                      str(events)
    ), "DUPLICATE_TREATMENT")
  },
  noIntervention=function(studie) {
    kwsWarning(paste0("This patient is not in the intervention arm, based on the following study data:\n",
                      str(studie)
    ), "NOT_IN_INTERVENTION_ARM")
  },
  disabled=function(studie) {
    kwsWarning(paste0("This patient would be dose-modified, but the software is temporarily disabbled:\n",
                      str(studie)
    ), "DISABLED")
  },
  notTacTrigger=function(trigger) {
    kwsWarning(paste0("Not returning recommendation. We only perform recommendations when receiving TAC lab result:\n",
                      str(trigger)
    ), "TRIGGER_NOT_TAC")
  },
  recommendationYesterdayNotFollowed=function(doses) {
    kwsWarning(paste0("A recommendation yesterday was not followed:\n",
                      str(doses)
    ), "RECOMMENDATION_YESTERDAY_NOT_FOLLOWED")
  },
  noPrescriptionToUpdate=function() {
    kwsWarning("No validated/unvalidated prescription to update", "NO_PRESCRIPTION_TO_UPDATE")
  }
)

exception <- list(
  outPatientDetected=function() {
    kwsException("Outpatient detected (transplant > 14 days)", "OUTPATIENT_DETECTED")
  },
  noTransplantException=function() {
    kwsException("No 'ingreep' field in the JSON file", "MISSING_INGREEP_FIELD")
  },
  incorrectTransplantDateException=function() {
    kwsException("Transplant date > current date", "INCORRECT_TRANSPLANT_DATE")
  },
  noPrescriptionException=function() {
    kwsException("No prescription at all in request", "NO_PRESCRIPTION")
  },
  nonExecutedvoorschriftInPastException=function(voorschrift=data.frame()) {
    kwsException(paste0(
      "Some voorschrift in the past were not executed:\n",
      paste(capture.output(print(voorschrift)), collapse="\n")
    ),"NON_EXECUTED_VOORSCHRIFT_IN_PAST")
  },
  infusionFoundException=function() {
    kwsException("An infusion was found in the list of prescriptions", "INFUSION_FOUND")
  },
  unknownModelException=function() {
    kwsException("Specified model does not exist on server", "UNKNOWN_MODEL")
  },
  eventInPast=function(events) {
    kwsException(paste0("The specified events should be in the future:\n",
                        paste(capture.output(print(events)), collapse="\n")
                    ), "EVENT_IN_PAST")
  },
  eventInFuture=function(events) {
    kwsException(paste0("The specified events should be in the past:\n",
                        paste(capture.output(print(events)), collapse="\n")
    ), "EVENT_IN_FUTURE")
  },
  invalidEventEnd=function(events){
    kwsException(paste0("The specified event end time is invalid:\n",
                        paste(capture.output(print(events)), collapse="\n")
    ), "EVENT_ENDTIME_INVALID")
  },
  
  jsonSchemaValidationException=function(message) {
    kwsException(message, "JSON_SCHEMA_VALIDATION_ERROR")
  },
  
  tacMeasuredBeforeDose=function(tac, firstDose) {
    kwsException(paste0(
      "Tacrolimus was measured before a first dose was administered at ", firstDose, ":\n",
      str(tac)
      ), "TAC_BEFORE_DOSE")
  },
  invalidSubdeel=function(studie) {
    kwsException(paste0("This patient is included in the study, but we do not know in which study arm!!\n",
                      str(studie)
    ), "INVALID_SUBDEEL")
  },
  notInStudy=function(studie) {
    kwsException(paste0("This patient is not included in study 63463\n",
                        str(studie)
    ), "NOT_IN_STUDY")
  }
)

#' This method modifies the events to correct small mistakes in the data.
#' 
#' @section Labo results:
#' 
#' Labo results are reported using the 'planned sampling time'.
#' Blood is often sampled before that time... This can become a problem if a treatment happend just before.
#' 
#' Example:
#' * Treatment (uitvoering) at 07:48
#' * Labo taken at 07:25, but reported by the KWS as 08:00
#' 
#' We detect this case and move the labo value to 07:48 - 10 minutes
#' 
#' The same thing happens when we receive the lab data before the actual registered sampling time.
#' 
#' Example:
#' * Request at 07:23
#' * Lab taken at 07:12, but reported by the KWS at 08:00 (in the future!)
#' 
#' We detect this case and move the labo value to 07:23 - 10 minutes.
#' 
#' @section Non-Tacrolimus doses:
#' 
#' We silently drop any dose (voorbereiding, voorschrift, uitvoering) that is not tacrolimus. We define this as an
#' omschrijving with the words "ADVAGRAF"or "PROGRAFT"
#' 
#' @param events data.frame as generated by as.eventlist
#' @return events modified as described in this documentation
fixEvents <- function(events) {
  stopifnot(is.eventlist(events))
  # Labo results are reported using the 'planned sampling time'.
  # Blood is often sampled before that time... We first move the blood sampling time to a corresponding treatment, should it exist.
  # This is specific for Tacrolimus, where we know blood is sampled at trough.
  uitvoering <- subset(events, event==eventType$uitvoering)
  newLaboTimes <- rep(as.POSIXct(NA), nrow(events))
  for(i in which(events$event == "labo")) {
    uitvoeringTimes <- subset(uitvoering, time < events$time[i], time, drop=TRUE)
    if(length(uitvoeringTimes) == 0) next #no uitvoeringen before this lab
    closestUitvoering <- max( uitvoeringTimes )
    if(difftime(events$time[i], closestUitvoering, units="hours") < 3) {
      newtime <- closestUitvoering - lubridate::minutes(10) #10 minutes before administration
      newLaboTimes[i] <- newtime
    }
  }
  
  i <- ! is.na(newLaboTimes)
  if(any(i)) {
    warning$movedTroughLaboTime(cbind(newtime=newLaboTimes[i], events[i,]))
    events$time[i] <- newLaboTimes[i]
  }
  
  # if there are labo results still in the future, this is because maybe the uitvoering has not even happened yet. 
  # Move the labo to the request time in this case
  now <- events$time[ events$event == eventType$request ]
  i <- events$event == eventType$labo &
    events$time > now
  if(any(i)) {
    # labo in the future
    newtime <- now - lubridate::minutes(10)
    warning$movedFutureLaboTime(events[i,], newtime)
    events$time[i] <- newtime #move to 'now'
  }
  
  # voorschrift, voorbereiding, uitvoering
  # we are only interested in TACROLIMUS: 
  # PROGRAFT or ADVAGRAF
  administrations <- events$event %in% c(eventType$voorbereiding, eventType$voorschrift, eventType$uitvoering)
  i <- administrations &
    !stringr::str_detect(events$omschrijving, pattern="(ADVAGRAF)|(PROGRAFT)")
  if(any(i)) {
    warning$notTacrolimusWarning(events[i,])
    events <- events[!i, ] #dropping
  }
  
  # voorschrift or voorbereiding of '0' in the past can be considered uitvoering
  eventsWithDose <- events %>% tidyr::unnest_wider(dosis) %>% 
    bind_rows(tibble::tibble(value=numeric(), ucumUnit=character())) #ensure right columns are present, even if no dose rows
  i <- events$event %in% c(eventType$voorbereiding, eventType$voorschrift) &
    eventsWithDose$value == 0 & events$time < now
  if(any(i)) {
    warning$ignoringZeroDoseInPast(events[i,])
    events$event[i] <- eventType$voorschrift #consider validated
    zeroExecution <- events[i,] #and add a corresponding execution
    zeroExecution$event <- eventType$uitvoering
    zeroExecution$voorschriftNr <- zeroExecution$id
    zeroExecution$id <- paste0("DUMMY-",zeroExecution$id)
    events <- bind_rows(
      events,
      zeroExecution
    )
  }
  
  # duplicate lab values
  tmp <- events[, c("time", "omschrijving")]
  tmp[events$event != eventType$labo, ] <- NA #only consider labo
  i <- events$event == eventType$labo & duplicated(tmp)
  if(any(i)) {
    warning$duplicateLabo(events[i, ])
    events <- events[!i, ]
  }
  
  # duplicate treatment values
  tmp <- events$id
  tmp[! events$event %in% c(eventType$voorbereiding, eventType$voorschrift, eventType$uitvoering) ] <- NA
  i <- ! is.na(tmp) & duplicated(tmp)
  if(any(i)) {
    warning$duplicateTreatment(events[i, ])
    events <- events[!i, ]
  }
  
  # mark which treatments are FIXED and which ones can be MODIFIED
  #browser()
  lastLabo <- max(events$time[ events$event == eventType$labo ])
  events$fix <- TRUE
  boolCanModify <- (events$time > (now + lubridate::dhours(3))) &  #we can modify treatments that are far away
      (events$time < (now + lubridate::ddays(1))) &  #only modify treatments in the immediate future
      (is.na(events$validatieDatum) | (events$validatieDatum < lastLabo))   #AND for which the validation was based on old info
  if(all(c("PROGRAFT", "ADVAGRAF") %in% events$omschrijving[boolCanModify]) && 
     "ADVAGRAF" %in% events$omschrijving[ events$event == eventType$uitvoering]) {
   kwsWarning("Both PROGRAFT and ADVAGRAF in future doses, while ADVAGRAF was already administered! Keeping only ADVAGRAF!", "KWS_ADVAGRAF_AND_PROGRAFT_PLANNED")
   boolCanModify <- boolCanModify & events$omschrijving == "ADVAGRAF" #once ADVAGRAF, never go back to PROGRAFT!
  }
  events$fix[ boolCanModify ] <- FALSE
  
  events
}

#' This method checks the events data.frame for any issues
#' 
checkEvents <- function(events) {
  # The events should be correctly related to each other
  now <- events$time[ events$event == eventType$request ]
  
  #ingreep: past
  #voorbereiding: only in the future
  #voorschrift: past or future
  #   every voorschrift in the past should have a corresponding uitvoering
  #uitvoering: past
  #labo: past
  #zorg: past
  
  if(!any(events$event == eventType$ingreep) ) exception$noTransplantException()
  
  transplantDate <- max( subset(events, event==eventType$ingreep)$time )
  if (transplantDate < (now - lubridate::ddays(14 + 1))) exception$outPatientDetected() # Outpatients if more than 14 days (+1 day safety margin)
  
  # check that we are not time-traveling to the future
  badEvents <- events %>% filter(
    event %in% c(eventType$ingreep, eventType$uitvoering, eventType$labo, eventType$zorg) & #should be in the past
      time >= now #but are in the future
  )
  if(nrow(badEvents) > 0) exception$eventInFuture( badEvents )
  
  # check that we did not forget something in the past
  badEvents <- events %>% filter(
    event == eventType$voorbereiding & #voorbereiding should be in the future
      time < now #but is in the past
  )
  if(nrow(badEvents) > 0) exception$eventInPast( badEvents )
  
  # find events with stopTime before startTime (strange!!)
  badEvents <- events %>% filter(
    .data$datumTot < .data$time #but is in the past
  )
  if(nrow(badEvents) > 0) exception$invalidEventEnd( badEvents )
  
  uitvoering <- subset(events, event == eventType$uitvoering)
  badEvents <- events %>% filter(event == eventType$voorschrift) %>% #voorschrift
    filter(
      time < now & #in the past
      ! id %in% uitvoering$voorschriftNr #but no corresponding uitvoering!
  )
  if(nrow(badEvents) > 0) exception$nonExecutedvoorschriftInPastException( badEvents )
  
  if(anyNA(uitvoering$voorschriftNr)) warning$executionWithoutPrescription(uitvoering[ is.na(uitvoering$voorschriftNr), ])
  
  # the date-of-birth and sex of the patient is available
  # TODO
  #if( setequal( names( events$attributen[ events$event == eventType$patient ] ), c("geboorteDatum", ""))
  
  # request is already checked in JSON validation
  
  # thuistherapie is ignored
  
  # voorbereiding, voorschrift and uitvoering should have a valid dose with the right unit
  administrations <- subset(events, event %in% c(eventType$voorbereiding, eventType$voorschrift, eventType$uitvoering)) %>%
    tidyr::unnest_wider(dosis) %>%
    bind_rows(tibble::tibble(value=numeric(), ucumUnit=character())) #ensure right columns are present, even if no dose rows
  i <- administrations$ucumUnit != "mg"
  if(any(i)) warning$incorrectDoseUnitWarning(administrations[i,] )
  
  # labo should be either hematocrit or tacrolimus
  # we warn, but do not refuse dosing advice
  labo <- subset(events, event %in% eventType$labo) %>% tidyr::unnest_wider(attributen) %>%
    bind_rows(tibble::tibble(numeriek=numeric(), ucumUnit=character(), soort=character())) #if no labo, the attributen does not expand into the right columns
  i <- !labo$omschrijving %in% c("Hematocriet (bloed)", "Tacrolimus (FK 506) (bloed)")
  if(any(i)) warning$incorrectMeasureUnitWarning(labo[i,] )
  hct <- labo[ labo$omschrijving == "Hematocriet (bloed)", ]
  i <- dplyr::between(hct$numeriek, 0, 1) & hct$soort=="R" & hct$ucumUnit=="{ratio}"
  if(any(!i)) warning$incorrectMeasureUnitWarning(hct[!i,] )
  tac <- labo[ labo$omschrijving == "Tacrolimus (FK 506) (bloed)", ]
  i <- tac$soort=="R" & tac$ucumUnit=="ug/L"
  if(any(!i)) warning$incorrectMeasureUnitWarning(tac[!i,] )
  
  # if tac was measured, a dose should exist before it!
  if(nrow(tac) > 0) {
    firstDose <- min( events$time[ events$event == eventType$uitvoering ] )
    i <- tac$time < firstDose
    if(any( i ) ) {
      exception$tacMeasuredBeforeDose( tac[i, ], firstDose)
    }
  }
  
  
  ## check for recommendations not applied
  uitvoeringen <- subset(events, event == eventType$uitvoering) %>%
    transmute(time, id, voorschriftNr, dosis=purrr::map_dbl(dosis, function(x)x$value)) %>%
    left_join(
      subset(events, event %in% eventType$voorschrift) %>% select(id, recommendation), 
      by=c(voorschriftNr="id") ) %>%
    filter(!is.na(recommendation) & recommendation != dosis) %>% arrange(time)
  uitvoeringen <- filter(uitvoeringen, time > now - lubridate::ddays(1))
  if(nrow(uitvoeringen) > 0) {
    warning$recommendationYesterdayNotFollowed(uitvoeringen)
  }
  
  ## sufficient number of doses in the future
  ## TODO
}

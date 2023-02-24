#' Easily compose a URL for MongoDB
#' 
#' @param host hostname
#' @param user username, or NULL for no user
#' @param password password, or NULL for no password
#' @return composes a url of mongodb://user:password@host
#' @export
composeUrl <- function(host="localhost", user=NULL, password=NULL) {
  url <- "mongodb://"
  if(!is.null(user) && user != "") {
    if(is.null(password)) stop("User specified, but password was not...")
    url <- paste0(url, utils::URLencode(user), ":", utils::URLencode(password), "@")
  }
  url <- paste0(url, host)
  url
}


#' Save a request and recommendation pair into the database.
#'
#' @param webApiDB web API DB
#' @param request a request from KWS (JSON string)
#' @param recommendation a recommendation (JSON object)
#' @param received_at time at which the request is received, can be NULL
#' @param ipAddress IP address of client who sends the request
#' 
#' @return the pair ID
#' @importFrom rjson toJSON
#' @export
#' 
saveRequest <- function(webApiDB, request, original, recommendation, received_at=NULL, ..., id=NULL) {
  pair <- list(request=request, originalRequest=original, recommendation=recommendation)
  
  pair$received_at <- if(is.null(received_at)) NULL else format(received_at, usetz=TRUE)
  pair$created_at <- format(Sys.time(), usetz=TRUE)
  pair[ names(list(...)) ] <- list(...)

  #bad idea to use sequential numeric IDs if this application
  #will ever scale in a multi-threaded environment...
  #Even though it certainly is useful, it remains a bad idea.
  if(is.null(id)) id <- uuid::UUIDgenerate()
  pair$id <- id
  
  webApiDB$insert(rjson::toJSON(pair))
  
  return(pair$id)
}

#' Get a request from the database. 
#'
#' @param id by ID
#' @param db mongolite object
#' @param oid optional oid of the object
#' @return the requested request
#' @export
#' 
getRequest <- function(id, db, oid) {
  query <- list()
  if(!missing(id)) query$id = id
  if(!missing(oid)) query$`_id` = list(`$oid`=oid)
  queryJson = rjson::toJSON(query)
  
  it <- db$iterate(queryJson, limit = 1)
  return(it$one())
}

getRequestOid <- function(id, db) {
  queryJson = rjson::toJSON(list(id=id))
  it <- db$iterate(queryJson, limit = 1, fields='{"_id": true}')
  return(it$one()$`_id`)
}

#' Remove a request from the database.
#'
#' @param id the request ID
#' @export
#' 
removeRequest <- function(id, db) {
  db$remove(paste0('{"id" : ', id, '}'))
}

#' Get all requests from the database. 
#'
#' @return all the requests
#' @export
#'
getAllRequests <- function(db, offset=0, limit=0, query="{}") {
  retValue <- db$find(fields = '{"id" : true, "created_at" : true}',
                      query=query,
                      skip=offset, limit=limit,
                      sort='{ "$natural" : -1 }') #inverse natural order; most recent first
  return(retValue)
}

#' Get all requests from the database (detailled version). 
#'
#' @return all the requests
#' @importFrom dplyr first select
#' @importFrom plyr laply
#' 
#' @export
#'
getAllRequestsDetailled <- function(db, offset=0, limit=100, query="{}") {
  requests <- db$find(fields = '{"id" : true,
                      "received_at" : true,
                      "created_at" : true,
                      "ip_address" : true,
                      "request": true,
                      "recommendation.uitvoering.dosis.value" : true,
                      "recommendation.shinyURL" : true,
                      "recommendation.errorCodeName" : true,
                      "recommendation.warnings.codeName" : true,
                      "_id": true}',
                      query=query,
                      skip=offset, limit=limit,
                      sort='{ "$natural" : -1 }') #inverse natural order
  nrow <- nrow(requests)
  
  uitvoering <- requests$recommendation$uitvoering
  if (is.null(uitvoering)) {
    requests$doses <- rep(NA, nrow)
  } else {
    requests$doses <- plyr::laply(uitvoering, function (uitvoeringItem) {
      doses <- as.numeric(unlist(uitvoeringItem))
      if(length(doses)==0) {
        NA
      } else {
        doses %>% round(2) %>% paste(., " mg", collapse = ", ")
      }
    })
  }
  
  if (is.null(requests$request)) {
    requests$requestTime <- rep(NA, nrow)
  } else {
    getRequestTime <- function (requestItem) {
      if(length(requestItem)==0 || nchar(requestItem) == 0) return(NA)
      tryCatch({
          request <- jsonlite::fromJSON(requestItem)
          result <- paste("", unlist( request$request$time ))
          if(length(result) > 1) stop("Multiple requestTimes")
          result
        } , error=function(e) e$message )
    }
    requests$requestTime <- purrr::map_chr(requests$request, getRequestTime)
  }
  
  if (is.null(requests$request)) {
    requests$txDate <- rep(NA, nrow)
  } else {
    getTxDate <- function (requestItem) {
      if(length(requestItem)==0 || nchar(requestItem) == 0) return(NA)
      tryCatch({
        request <- jsonlite::fromJSON(requestItem)
        txDate <- unlist( request$ingreep$datum )
        txDate <- max(txDate) # use the last transplant date available
        patient <- request$patient
        if(is.null(txDate)) NA else paste(txDate, collapse=",")
        txDate <- paste(txDate, "-", patient$geslacht, substr(patient$geboorteDatum, start=1, stop=4), sep="", collapse=";")
        txDate
      } , error=function(e) e$message
      )
    }
    requests$txDate <- purrr::map_chr(requests$request, getTxDate)
  }
  
  errorCodeName <- requests$recommendation$errorCodeName
  if (is.null(errorCodeName)) {
    requests$errorCodeName <- rep(NA, nrow)
  } else {
    requests$errorCodeName <- purrr::map_chr(errorCodeName, function (errorItem) {
      item <- unlist(errorItem) %>% dplyr::first()
      item <- ifelse(is.null(item), NA, as.character(item))
      return(item)
    })
  }
  
  warnings <- requests$recommendation$warnings
  if (is.null(warnings)) {
    requests$warnings <- rep(NA, nrow)
  } else {
    requests$warnings <- plyr::laply(warnings, function (warning) {
      items <- unlist(warning)
      items <- ifelse(is.null(items), NA, paste(items, collapse = ", "))
      return(items)
    })
  }
  
  for(i in c("received_at", "created_at", "requestTime"))
    if(i %in% colnames(requests)) {
      requests[i] <- if(nrow(requests) > 0) anytime::anytime(requests[[i]]) else as.POSIXct(character(0))
    }

  if("recommendation" %in% colnames(requests))
    requests <- requests %>% dplyr::select(-c("recommendation"))
  
  return(requests)
}
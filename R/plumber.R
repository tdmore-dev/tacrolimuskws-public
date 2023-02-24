## This file contains the core Plumber API

#' Execute the main plumber API, using environment keys to configure all settings.
#' 
#' Environment keys used:
#' - KWS_CONFIG_MONGODB_ADMINUSERNAME
#' - KWS_CONFIG_MONGODB_ADMINPASSWORD
#' - KWS_CONFIG_MONGODB_SERVER
#' - KWS_CONFIG_API_KEY
#' 
#' The api keys are separated with ';'. If this is unset, no authentication is performed.
#' 
#' @export
runPlumber <- function(port=8080, 
                       debug=FALSE, 
                       shinyAppBaseURL=Sys.getenv("KWS_CONFIG_SHINY_URL", unset="https://localhost")) {
  # Selecting DB's
  dbConfig <- list(
    user = Sys.getenv("KWS_CONFIG_MONGODB_ADMINUSERNAME"),
    password = Sys.getenv("KWS_CONFIG_MONGODB_ADMINPASSWORD"),
    server = Sys.getenv("KWS_CONFIG_MONGODB_SERVER", unset="localhost")
  )
  url <- composeUrl(host=dbConfig$server, user=dbConfig$user, password=dbConfig$password)
  cat("Starting plumber with mongo connection ", url, " and shinyAppBaseURL ", shinyAppBaseURL, "...\n")
  
  webApiDB <- mongolite::mongo(collection="requests", db="tacrolimuskws", url=url)
  configDB <- mongolite::mongo(collection="config", db="tacrolimuskws", url=url)
  
  apiKeys = strsplit(Sys.getenv("KWS_CONFIG_API_KEY", unset=NA), ";")[[1]]
  htpasswd = strsplit(Sys.getenv("KWS_CONFIG_HTTP_AUTH", unset=NA), ";")[[1]]
  cat("API Keys: ", Sys.getenv("KWS_CONFIG_API_KEY", unset=NA), "\n")
  cat("htpasswd: ", Sys.getenv("KWS_CONFIG_HTTP_AUTH", unset=NA), "\n")
  ## To set htpasswd, use 
  ## Sys.setenv(KWS_CONFIG_HTTP_AUTH=paste0("Basic ", jsonlite::base64_enc(paste0("admin", ":", "blabla"))))
  pb <- getPlumber(apiKeys, htpasswd,
                   webApiDB, configDB,
                   shinyAppBaseURL=shinyAppBaseURL)
  pb$run("0.0.0.0", port=port, debug=debug, docs=FALSE)
}

#' Get the main plumber API, programmatically defined.
#' 
#' @export
#' @param apiKeys list of allowed api keys, or NA to disable authentication
#' @param htpasswd list of allowed Basic authorization keys. These are stored as 'Basic XXXXX', with XXXXX the 
#' @param webApiDB mongolite database connection to store requests
#' @param shinyAppBaseUrl base URL for Shiny app
getPlumber <- function(apiKeys=NULL, htpasswd=NULL,
                       webApiDB,
                       configDB,
                       shinyAppBaseURL="https://localhost") {
  pr <- plumber::Plumber$new()
  ## Filters are added in sequence. They are executed first, before any of the endpoints.
  pr$filter("auth", function(req, res) plumber_auth(req, res, apiKeys, htpasswd))
  pr$filter("logger", plumber_logger) #only log auth'd requests
  
  pr$handle("GET", "/heartbeat", handler=plumber_heartbeat, preempt="auth") #preempt both auth and logger
  pr$handle("POST", "/request", 
            handler=function(req, res) plumber_request(req, res, webApiDB, configDB, shinyAppBaseURL), 
            serializer=plumber::serializer_unboxed_json())
  pr$handle("GET", "/request/<id>", 
            function(req, res, id, pretty=FALSE, visualize=FALSE, original=FALSE) plumber_getRequest(req, res, id, pretty, visualize, original, webApiDB))
  pr$handle("GET", "/recommendation/<id>", 
            function(req, res, id, pretty) plumber_getRecommendation(req, res, id, pretty, webApiDB))
  pr$handle("GET", "/requests",
            function(req, res, offset=0, limit=50, query="{}") plumber_admin(req, res, offset, limit, query, webApiDB, shinyAppBaseURL)
  )
  pr$handle("GET", "/dump",
            function(req, res, offset=0, limit=0) plumber_dump(req, res, webApiDB, offset=as.numeric(offset), limit=as.numeric(limit))
  )
  pr$handle("GET", "/version",
            function(req, res) plumber_version(req, res)
  )
  pr$handle("GET", "/regex",
            function(req, res) plumber_configRegex(req, res, configDB)
  )
  pr$handle("POST", "/regex",
            function(req, res, action="add", Regex="", Replacement="", Comment="") plumber_configRegex_post(req, res, configDB, action, Regex, Replacement, Comment)
  )
  pr$handle("GET", "/stop",
            function(req, res) {
              httpuv::interrupt()
              return("interrupt received")
            }
  )
  
  pr
}

plumber_configRegex_post <- function(req, res, configDB, action="add", Regex="", Replacement="", Comment="") {
  config <- configDB$iterate()$one()
  if(action == "add") {
    rex <- stringr::regex(Regex, multiline=TRUE, dotall=TRUE)
    isOK <- try({stringr::str_match("myMatchrex", rex); TRUE}, silent=TRUE)
    if(!isTRUE(isOK)) {
      return(
        plumber_configRegex(req, res, configDB, msg=paste0("Error in regex: ", isOK))
      )
    }
    
    config$regex <- c(
      config$regex,
      list(list(Regex=Regex, Replacement=Replacement, Comment=Comment))
    )
    msg <- "Added new Regex\n"
  } else if (action == "remove") {
    config$regex[[as.numeric(Regex)]] <- NULL
    #config$regex <- purrr::discard(config$regex, function(x){x$Regex == Regex})
    msg <- "Removed path\n"
  }
  
  json <- jsonlite::toJSON(config, auto_unbox = TRUE)
  outMsg <- capture.output(
    {configDB$replace("{}", update=json, upsert=TRUE)}
  )
  msg <- paste0(msg, paste(outMsg, collapse="\n"), "\n", json )
  
  plumber_configRegex(req, res, configDB, msg=msg)
}

plumber_configRegex <- function(req, res, configDB, msg=NULL) {
  config <- configDB$iterate()$one()
  html <- paste0("<pre>", msg, "</pre>")
  html <- paste0(html, '<table border="1"><tr><th>Regex</th><th>Replacement</th><th>Comment</th><td/></tr>\n')
  for(i in seq_along(config$regex)) {
    re <- config$regex[[i]]
    html <- paste0(html, "<tr><td>", re$Regex, "</td><td>", re$Replacement, "</td><td>",re$Comment,"</td>\n")
    #tag <- htmltools::tags$input(type="hidden", name="Regex", value=re$Regex)
    html <- paste0(html, '<td><form method="POST">',
                   '<input type="hidden" name="Regex" value="', i, '">',
                   '<input type="hidden" name="action" value="remove">',
                   '<input type="submit" value="X">',
                   '</form>',
                   "</td>")
    html <- paste0(html, "</tr>\n")
  }
  html <- paste0(html, "</table>\n")
  
  html <- paste0(html, '
  <form method=POST>
  <label for="Regex">Regex:</label><br>
  <input type="text" id="Regex" name="Regex"><br>
  <label for="Replacement">Replacement:</label><br>
  <input type="text" id="Replacement" name="Replacement"><br>
  <label for="Comment">Comment:</label><br>
  <input type="text" id="Comment" name="Comment"><br>
  <input type="submit" value="Add"/>
  </form>')
  
  res$body <- html
  res$setHeader("Content-type", "text/html")
  res
}

to_url <- function(req, url, ...) {
  Rook::Request$new(req)$to_url(url, ...)
}


#* Return an HTML table with all requests and recommendations.
#* @get /requests
plumber_admin <- function(req, res, offset=0, limit=50, query="{}", db, shinyAppBaseURL) {
  offset <- as.numeric(offset)
  limit <- as.numeric(limit)
  if(offset < 0) offset <- 0
  
  # Get all requests from DB
  requests <- getAllRequestsDetailled(db, offset, limit, query)
  
  head <- "<html><head>
    <style>
    table {
      font-family: arial, sans-serif;
      border-collapse: collapse;
      width: 100%;
    }
    
    td, th {
      border: 1px solid #dddddd;
      text-align: left;
      padding: 8px;
    }
    
    tr:nth-child(even) {
      background-color: #dddddd;
    }
    </style>
    </head>"
  
  tableInit <- paste0(head, "<table><tr><th>Request ID</th><th>Request KWS send time</th><th>Request start</th><th>Computation time (in seconds)</th><th>Original request</th><th>Patient</th><th>Recommendation</th><th>Dosing advice</th><th>Error</th><th>Warning(s)</th><th>IP Address</th></tr>")
  
  doses <- ifelse(is.na(requests$doses), "/", requests$doses)
  errors <- ifelse(is.na(requests$errorCodeName), "/", requests$errorCodeName)
  
  start <- requests$received_at
  if(nrow(requests) == 0) {
    diff <- numeric()
  } else {
    stop <- as.POSIXct(requests$created_at)
    diff <- as.numeric(stop - start) # already in seconds
  }
  ip <- ifelse(sapply(requests$ip_address, is.null), "/", requests$ip_address)
  warnings <- ifelse(is.na(requests$warnings), "/", "Yes")
  txDate <- requests$txDate
  if(nrow(requests) > 0) requests$shinyURL <- paste0(shinyAppBaseURL, "?id=", requests$id, "&nonce=", requests$`_id`)
  
  tableContent <- paste0("<tr><td>", requests$id, "</td>",
                         "<td>", requests$requestTime, "</td>",
                         "<td>", start, "</td>",
                         "<td>", diff, "</td>",
                         "<td>", "<a href='", 
                         to_url(req, paste0("/request/", requests$id), pretty=TRUE),"'>Request</a> <small><a href='", 
                         to_url(req, paste0("/request/", requests$id), visualize=TRUE),"'>vis</a></small>", "</td>",
                         "<td>", txDate, "</td>",
                         "<td>", "<a href='", to_url(req, paste0("/recommendation/", requests$id), pretty=TRUE), "'>Recommendation</a>", "</td>",
                         "<td>", paste0("<a href='", requests$shinyURL, "'>", doses,"</a>"), "</td>",
                         "<td>", errors, "</td>",
                         "<td>", warnings, "</td>",
                         "<td>", ip, "</td>",
                         "</tr>", collapse = "\n")
  tableEnd <- paste0("</table>",
    "<a href='?limit=",limit,"&offset=",offset-limit,"&query=", query,"'>older</a>",
    "<a href='?limit=",limit,"&offset=",offset+limit,"&query=", query,"'>newer</a>",
    "</html>"
  )
  html <- paste0(tableInit, tableContent, tableEnd)
  
  res$body <- html
  res$setHeader("Content-type", "text/html")
  res
}

#' Dump all requests from the database in a single zip-file
plumber_dump <- function(req, res, db, offset=0, limit=0, query="{}") {
  # Get all requests from DB
  result <- safeRun({
    requests <- getAllRequests(db, offset=offset, limit=limit, query=query)
    files <- c()
    for(id in requests$id) {
      request <- getRequest(id, db)
      requestJson <- paste(request$request, collapse="\n")
      
      file <- file.path(tempdir(), paste0(id, ".json"))
      write( x=requestJson, file=file)
      date <- request$received_at
      if(is.null(date)) date <- request$created_at
      if(!is.null(date)) fs::file_touch(file, modification_time=anytime::anytime(date))
      files <- c(files, file)
    }
    
    # Put them in a zipfile
    zipFile <- tempfile(fileext=".zip")
    zip(zipfile=zipFile, files=files)
    body <- readBin(zipFile, "raw", n = file.info(zipFile)$size)
    
    #clean up
    unlink(zipFile)
    unlink(files)
    
    body
  })
  
  if(is.null(result$value)) {
    return(stringify(result))
  } else {
    res$body <- result$value
    res$setHeader("Content-type", "application/zip")
    return(res)
  }
  
}

plumber_version <- function(req, res) {
  versions <- list(
    utils::packageDescription("tacrolimuskws"),
    utils::packageDescription("shinytdmore"),
    utils::packageDescription("tdmore")
  ) %>% lapply(unclass)
  
  if(TRUE) { #prefer HTML, but keep code for JSON below
    htmlContent <- htmltools::HTML(
      htmlTable::htmlTable(
        versions %>% purrr::map_dfr(as_tibble)
      )
    )
    res$body <- htmlContent
    res$setHeader("Content-type", "text/html")
    return(res)
  } else {
    res$body <- jsonlite::toJSON(versions)
    res$setHeader("Content-type", "application/json")
    res
  }
}

#* Heartbeat service
#* @get /heartbeat
#* @preempt auth
plumber_heartbeat <- function(req, res) {
  return("OK")
}

#' Create a filter to log all requests
plumber_logger <- function(req) {
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-", 
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

#' Create a plumber filter to authenticate against the given list of api keys
#' The filter verifies whether the requests `X-KWS-Token` header is listed in a set of allowed api keys.
#' 
#' If apiKeys is NA, no authentication is performed
#' @param apiKeys 
plumber_auth <- function(req, res, apiKeys, htpasswd) {
  if(isAllowedApiKey(req, apiKeys) || isAllowedBasicAuth(req, htpasswd)){
    plumber::forward()
  } else {
    res$status <- 401 # Unauthorized
    res$setHeader("WWW-Authenticate", 'Basic realm="TacrolimusKWS"')
    return(list(error="Authentication required, please set request header X-KWS-Token or perform HTTP basic auth"))
  }
}
isAllowedApiKey <- function(req, apiKeys) {
  if(length(apiKeys) == 0 || (length(apiKeys)==1 & is.na(apiKeys))) return(TRUE) #no security configured
  if( is.null(req$HTTP_X_KWS_TOKEN) || length(req$HTTP_X_KWS_TOKEN) == 0)
    return(FALSE) ## token not defined in request
  if( req$HTTP_X_KWS_TOKEN %in% apiKeys )
    return(TRUE)
  return(FALSE)
}

isAllowedBasicAuth <- function(req, htpasswd) {
  if(length(htpasswd) == 0 || (length(htpasswd)==1 & is.na(htpasswd))) return(FALSE)
  token <- req$HTTP_AUTHORIZATION
  if( is.null(token) || length(token) == 0)
    return(FALSE) ## token not defined in request
  if( token %in% htpasswd )
    return(TRUE)
  return(FALSE)
}

#' This is the main function to handle a request from KWS
#' 
#' The function uses plumber_safeRequest to
#' translate the KWS request into a TDMore-compatible format,
#' execute the estimation and dose recommendation,
#' and translate this back into KWS format.
#' The function also provides error and warning handling.
#' 
#' Before returning the request, we store it in a separate database for later consultation.
#' 
#' @export
plumber_request <- function(req, res, webApiDB=NULL, configDB=NULL, shinyAppBaseURL="", received_at=Sys.time()) {
  force(received_at) #ensure it is calculated NOW
  requestJson <- paste( req$postBody, collapse="\n")
  
  config <- configDB$iterate()$one()
  
  additionalWarnings <- c()
  for(i in seq_along(config$regex)) {
    re <- config$regex[[i]]
    matched <- stringr::str_detect(requestJson, stringr::regex(re$Regex, multiline=TRUE, dotall=TRUE))
    if( matched ) {
      requestJson <- stringr::str_replace_all(requestJson, stringr::regex(re$Regex, multiline=TRUE, dotall=TRUE), re$Replacement)
      additionalWarnings <- c(additionalWarnings,
                              list(
                              warningCondition(paste0("Modified the input data using regex #", i, " (", re$Comment, ")"), codeName="KWS_REGEX_APPLIED", class="kws.warning")
                              )
      )
    }
  }
  
  
  # Compute the recommendation
  recommendation <- safeRun({
    computeRecommendation(
      requestJson = requestJson,
      db=webApiDB
    )
  })
  recommendation$warnings <- c(additionalWarnings, recommendation$warnings)
  
  
  retValue <- stringify(recommendation)
  retValue$value <- NULL
  retValue$uitvoering <- recommendation$value$updatedPrescriptions
  retValue <- retValue %>% purrr::discard(is.null) # Filter NULL elements
  
  # Save both the request and the recommendation into the Mongo database
  if (is.null(webApiDB)) {
    requestId <- -1
    requestOid <- -1
  } else {
    retValue$config <- config
    requestId <- saveRequest(webApiDB, 
                             request=requestJson,
                             original=paste( req$postBody, collapse="\n"), #use the original request!
                             retValue, 
                             received_at=received_at, 
                             ip_address=getIpAddress(req),
                             coef=if(is.null(recommendation$value)) NULL else coef(recommendation$value$fit)
                             )
    retValue$config <- NULL #remove again
    requestOid <- getRequestOid(requestId, webApiDB)
  }
  
  retValue$requestId <- requestId
  retValue$requestUrl <- to_url(req, paste0("/request/", requestId))
  retValue$shinyURL <- paste0(shinyAppBaseURL, "?id=", requestId, "&nonce=", requestOid)
  
  retValue$uitvoering <- lapply(retValue$uitvoering,
                                function(x){
                                  x$nota <- stringr::str_replace(x$nota,
                                                                 stringr::fixed("$SHINY_URL"),
                                                                 retValue$shinyURL)
                                  x
                                })
  
  sendAlerts(retValue, config$alertConfig)
  
  # Return the output (Plumber will automatically encode it into JSON)
  return(retValue)
}

sendAlerts <- function(retValue, alertConfig) {
  warningsToSend <- c("KWS_ADVAGRAF_AND_PROGRAFT_PLANNED", "KWS_REGEX_APPLIED", "NO_PRESCRIPTION_TO_UPDATE", "NON_EXECUTED_VOORSCHRIFT_IN_PAST", "INVALID_SUBDEEL", "RECOMMENDATION_YESTERDAY_NOT_FOLLOWED")
  
  errorCode <- retValue$errorCodeName
  warningCodes <- purrr::map_chr(retValue$warnings, function(x) {
    if(is.null(x$codeName)) "INTERNAL" else x$codeName
    })
  body <- NULL
  if(!is.null(errorCode) && errorCode %in% warningsToSend) {
    body <- paste0("Error message: ", retValue$errorCodeName, "\n", retValue$shinyURL)
  }
  
  ## Check for some warnings, but only for INTERVENTION patients
  i <- which(warningCodes %in% warningsToSend )
  if (!("NOT_IN_STUDY" %in% errorCode) && #patient is in study
      !("NOT_IN_INTERVENTION_ARM" %in% warningCodes) && #and in intervention arm
      length(i) != 0) { #and warnings exist
    warnings <- lapply(retValue$warnings[i], function(x) paste0("<h2>",x$codeName,"</h2>", "<p>",x$message, "</p>"))
    body <- paste0(body, "<h1>Warnings</h1>",
                   warnings)
  }
  
  if(is.null(body)) { #nothing to send
    return()
  } else {
    body <- paste0("<h1>Error/warning report for precision dosing request</h1><p>",retValue$shinyURL,"</p>",body)
  }
  
  mail <- email_message(
    from=mail_address("administrator@domain.be", "MIPD Owner"),
    to=list(
      mail_address("doctor1@domain.be", "Doctor1"),
      mail_address("doctor2@domain.be", "Doctor2")
    ),
    subject = "TacrolimusKWS message",
    textPart = body,
    htmlPart = body,
    customId = retValue$requestId
  )
  send_email(mail)
}

stringify <- function(safeRun) {
  e <- safeRun$error
  safeRun$error <- e$message
  safeRun$errorCodeName <- e$codeName
  safeRun$warnings <- lapply(safeRun$warnings, function(x) {list(codeName=x$codeName, message=x$message)} )
  safeRun
}

#' This function adds calling handlers to an expression.
#' @return name list with elements 'value', 'error', 'errorCodeName' and 'warnings'
#' This way, an expression will always return a named list, even if there was an error.
safeRun <- function(expr) {
  retValue <- list(
    value=NULL,
    error=NULL,
    warnings=list()
  )
  
  #tryCatch returns on any signal caught. Therefore, we cannot catch warnings with tryCatch
  #withCallingHandlers does not "swallow" signals, but passes them on. We can use this for warnings. withCallingHandlers does not modify normal flow.
  #
  #A combination is therefore needed to catch both errors and warnings.
  stackTraceFile <- NULL
  call.stack <- NULL
  tryCatch({
    withCallingHandlers({
      
      retValue$value <- force(expr)
      
    }, warning=function(w) {
      retValue$warnings <<- c(retValue$warnings, list(w))
    }, kws.exception=function(e) {
      #just pass it on
    }, error=function(e) {
      ## The error needs to be caught here and is then passed on to tryCatch
      call.stack <<- sys.calls() # is like a traceback within "withCallingHandlers"
      dump.frames()
      file <- tempfile(fileext=".RDa")
      save.image(file=file)
      stackTraceFile <<- file
    })
  },
  kws.exception = function(error) {
    retValue$error <<- error
  },
  error = function(e) {
    message <- paste("A critical error has occured:", 
            str(e),
            paste(limitedLabels(call.stack), collapse="\n"), 
            paste0("Stacktrace saved as ", stackTraceFile),
            sep = "\n")
    retValue$error <<- errorCondition(message=message, codeName="CRITICAL_ERROR", class="kws.exception", call=e$call)
  })
  
  retValue
}

# x <- noPrescriptionToUpdateException()
# withCallingHandlers({
#   stop(x)
# }, kws.exception=function(e) {
#   browser()
# })

getIpAddress <- function(req) {
  header <- req$HEADERS
  xForwardedFor <- "x-forwarded-for"
  if (xForwardedFor %in% names(header)) {
    ipAddress <- header[[xForwardedFor]]
  } else {
    ipAddress <- req$REMOTE_ADDR
  }
}

#* Return a previous request.
#* @get /request/<id>
plumber_getRequest <- function(req, res, id, pretty=FALSE, visualize=FALSE, original=FALSE, db) {
  # Get request from DB
  request <- getRequest(id, db)
  if (is.null(request) || request=="{}") {
    return("No request found")
  }
  
  json <- paste(request$request, collapse="\n")
  if(original)
    json <- paste(request$originalRequest, collapse="\n")
  
  if(isTRUE(as.logical(visualize))) {
    result <- safeRun({ plot_kwsRequest(json) }) #htmlwidget
    if(is.null(result$value)) return(stringify(result)) #return the stacktrace instead
    
    file <- tempfile(fileext=".html") #this needs to be an HTML file, otherwise the automated pandoc detection does not work!
    on.exit({unlink(file)})
    htmlwidgets::saveWidget(widget=result$value, file=file, selfcontained=TRUE)
    
    ### FIXME: Unfortunately, saveWidget stores the zoom-button HTML in <pre> tags
    ### This needs to be fixed, otherwise the JS setup script fails!
    htmlContent <- paste(readLines(file), collapse="\n")
    htmlContent <- sub("<pre><code>&lt;div class.*</code></pre>", '<div class="btn-group zoom-menu">
      <button type="button" class="btn btn-default btn-lg zoom-in" title="Zoom in">+</button>
      <button type="button" class="btn btn-default btn-lg zoom-out" title="Zoom out">-</button>
    </div>', htmlContent)
    
    res$body <- htmlContent
    res$setHeader("Content-type", "text/html")
    
    return(res)
  }
  
  if(isTRUE(as.logical(pretty))) {
    json <- jsonlite::prettify(json)
  }

  res$body <- json
  res$setHeader("Content-type", "application/json")
  res
}

#* Return a previous recommendation (based on associated request).
#* @get /recommendation/<id>
plumber_getRecommendation <- function(req, res, id, pretty, db){
  # Get request from DB
  request <- getRequest(id, db)
  if (is.null(request) || request=="{}") {
    return("No recommendation found")
  }
  
  json <- jsonlite::toJSON(request$recommendation, auto_unbox=TRUE)
  if(!missing(pretty) && isTRUE(as.logical(pretty))) {
    json <- jsonlite::prettify(json)
  }
  
  res$body <- json
  res$setHeader("Content-type", "application/json")
  res
}

plumber_debugAll <- function(undebug=FALSE) {
  functions <- list(plumber_admin, plumber_auth, plumber_getRecommendation, plumber_getRequest, plumber_heartbeat, plumber_logger, plumber_request)
  if(undebug) {
    lapply(functions, undebug)
  } else {
    lapply(functions, debug)
  }
}

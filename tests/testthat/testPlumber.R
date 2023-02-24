library(testthat)
library(tacrolimuskws)

context("Test the Plumber methods work correctly")

p <- NULL
tmp_lib <- NULL
port <- NULL
teardown({
  cat("Teardown: killing plumber process")
  if(!is.null(p)) {
    GET("stop", auth="foobar", httr::timeout(2))
    p$wait(timeout=5000)
  }
  if(!is.null(tmp_lib)) unlink(tmp_lib, recursive = TRUE)
})

setup({
  tmp_lib <<- tdmore::ensurePackagePresent("tacrolimuskws")
  port <<- shinytest:::random_open_port() # abuse utilty function in shinytest
  fun <- function() {}
  body(fun) <- substitute({
    tacrolimuskws::runPlumber(port=port)
  }, list(port=port))
  
  p <<- 
    withr::with_libpaths(tmp_lib, {
      callr::r_bg(fun, env=c(KWS_CONFIG_API_KEY="foobar;barfoo"), cmdargs=c())
    }, action="prefix")
  start <- Sys.time()
  while(TRUE) {
    out <- p$read_error_lines()
    message(paste(out, collapse="\n"))
    if(length( grep("Starting server to listen on port", out)) != 0 ) {
      break
    }
    Sys.sleep(0.2)
    if( (Sys.time() - start) > lubridate::dminutes(2) ) stop("Timeout waiting for plumber start!")
    if(!p$is_alive()) stop("Plumber process stopped early!")
  }
})

GET <- function(url, auth="foobar", ...) {
  httr::GET(
    url=paste0("http://localhost:", port, "/", url), 
    if(isFALSE(auth)) httr::add_headers() else httr::add_headers(X_KWS_TOKEN=auth),
    ...)
}

POST <- function(url, auth="foobar", ...) {
  httr::POST(
    url=paste0("http://localhost:", port, "/", url), 
    if(isFALSE(auth)) httr::add_headers() else httr::add_headers(X_KWS_TOKEN=auth),
    ...
  )
}

test_that("plumber process launches", expect_true( p$is_alive() ) )

describe("plumber API", {
  it("authenticates correctly", {
    x <- GET("requests", auth=FALSE)
    expect_equal( httr::status_code(x), 401 )
    expect_equal( httr::http_status(x)$reason, "Unauthorized" )
    
    x <- GET("requests")
    expect_equal( httr::status_code(x), 200)
  })
  
  p$read_output_lines() #clear buffer
  
  it("does not log unauthenticated requests", {
    x <- GET("requests", auth=FALSE)
    log <- p$read_output_lines()
    expect_length(log, 0) #no log messages, but maybe we did not wait long enough... Difficult to test
  })
  it("does log authenticated requests", {
    x <- GET("request/0")
    Sys.sleep(1) #wait a little bit for the message to appear
    for(i in seq(0, 100)) {
      if(i == 100) stop("Timeout (20s) waiting for log message")
      log <- p$read_output_lines()
      if(length(log)==0) {
        Sys.sleep(0.2)
        next
      }
      break
    }
    expect_match(log, ".*GET /request/0.*") #log message
  })
  
  it("provides an unauthenticated and unlogged heartbeat", {
    x <- GET("heartbeat", auth=FALSE)
    expect_equal( httr::status_code(x), 200)
    
    x <- GET("heartbeat", auth="foobar")
    expect_equal( httr::status_code(x), 200)
  })
  
  it("allows you to post an empty request, and returns the right error", {
    x <- POST("request") #empty request
    xContent <- httr::content(x)
    expect_equal(xContent$errorCodeName, "JSON_SCHEMA_VALIDATION_ERROR")
  })
  
  it("allows you to post a faulty request, and returns the right error", {
    x <- POST("request", body='
{
"ingreep":[{"datum":"2019-07-13T00:00:00+02:00","idType":"ICD9-O","id":"5569","omschrijving":"DUMMY DATA"}],
"patient":[{"geboorteDatum":"1950-01-01T00:00:00+01:00","geslacht":"v"}],
"request":{"id":"1563369809284115305","time":"2019-07-17T15:23:29.284115305+02:00"},
"uitvoering":[
  {"uitvoeringNr":68322582,"cnk":"2495786","omschrijving":"ADVAGRAF","datumVan":"2019-07-14T08:00:00+02:00","datumTot":"2019-07-14T08:00:00+02:00","dosis":{"value":1,"ucumUnit":"mg"}},
  {"uitvoeringNr":68322581,"cnk":"2669216","omschrijving":"ADVAGRAF","datumVan":"2019-07-14T08:00:00+02:00","datumTot":"2019-07-14T08:00:00+02:00","dosis":{"value":3,"ucumUnit":"mg"}},
  {"uitvoeringNr":68333857,"cnk":"0835017","omschrijving":"GLURENORM","datumVan":"2019-07-14T12:00:00+02:00","datumTot":"2019-07-14T12:00:00+02:00","dosis":{"value":10,"ucumUnit":"mg"}},
  {"uitvoeringNr":68340356,"cnk":"0835017","omschrijving":"GLURENORM","datumVan":"2019-07-14T17:00:00+02:00","datumTot":"2019-07-14T17:00:00+02:00","dosis":{"value":10,"ucumUnit":"mg"}},
  {"uitvoeringNr":68340357,"cnk":"0044842","omschrijving":"GLUCOPHAGE","datumVan":"2019-07-14T18:00:00+02:00","datumTot":"2019-07-14T18:00:00+02:00","dosis":{"value":500,"ucumUnit":"mg"}},
  {"uitvoeringNr":68322536,"cnk":"0835017","omschrijving":"GLURENORM","datumVan":"2019-07-14T08:00:00+02:00","datumTot":"2019-07-14T08:00:00+02:00","dosis":{"value":10,"ucumUnit":"mg"}},
  {"uitvoeringNr":68322551,"cnk":"2556512","omschrijving":"GALVUS","datumVan":"2019-07-14T08:00:00+02:00","datumTot":"2019-07-14T08:00:00+02:00","dosis":{"value":50,"ucumUnit":"mg"}},
  {"uitvoeringNr":68322534,"cnk":"0044842","omschrijving":"GLUCOPHAGE","datumVan":"2019-07-14T07:00:00+02:00","datumTot":"2019-07-14T07:00:00+02:00","dosis":{"value":500,"ucumUnit":"mg"}}
],
"voorbereiding":[
  {"voorschriftNr":149910462,"cnk":"2495786","omschrijving":"ADVAGRAF","datumVan":"2018-08-15T20:00:00+02:00","datumTot":"2018-08-15T20:00:00+02:00","dosis":{"value":1,"ucumUnit":"mg"}},
  {"voorschriftNr":149910462,"cnk":"2495786","omschrijving":"ADVAGRAF","datumVan":"2018-08-15T20:00:00+02:00","datumTot":"2018-08-15T20:00:00+02:00","dosis":{"value":1,"ucumUnit":"mg"}}
],
"zorg":[
  {"zorgNr":1171699670,"definitieCode":"FPgewicht","datum":"2018-08-14T09:59:30.693+02:00","attributen":[
    {"attribuutDefinitieCode":"FPgewichtW","waarde":"45.0"},
    {"attribuutDefinitieCode":"FPgewichtBMI","waarde":"true"},
    {"attribuutDefinitieCode":"FPgewichtogm","waarde":"Gevraagd gewicht"}]}
]
}
')
    xContent <- httr::content(x)
    expect_equal(xContent$errorCodeName, "NO_PRESCRIPTION_TO_UPDATE")
  })
  
  xContent <- NULL
  it("allows you to post a correct request, and returns 200 OK", {
    x <- POST("request", body='
{
"ingreep":[{"datum":"2019-07-13T00:00:00+02:00","idType":"ICD9-O","id":"5569","omschrijving":"DUMMY DATA"}],
"patient":[{"geboorteDatum":"1950-01-01T00:00:00+01:00","geslacht":"v"}],
"request":{"id":"1563369809284115305","time":"2019-07-14T15:23:29.284115305+02:00"},
"uitvoering":[
  {"uitvoeringNr":68322582,"cnk":"2495786","omschrijving":"ADVAGRAF","datumVan":"2019-07-14T08:00:00+02:00","datumTot":"2019-07-14T08:00:00+02:00","dosis":{"value":1,"ucumUnit":"mg"}},
  {"uitvoeringNr":68322581,"cnk":"2669216","omschrijving":"ADVAGRAF","datumVan":"2019-07-14T08:00:00+02:00","datumTot":"2019-07-14T08:00:00+02:00","dosis":{"value":3,"ucumUnit":"mg"}},
  {"uitvoeringNr":68333857,"cnk":"0835017","omschrijving":"GLURENORM","datumVan":"2019-07-14T12:00:00+02:00","datumTot":"2019-07-14T12:00:00+02:00","dosis":{"value":10,"ucumUnit":"mg"}},
  {"uitvoeringNr":68340356,"cnk":"0835017","omschrijving":"GLURENORM","datumVan":"2019-07-14T17:00:00+02:00","datumTot":"2019-07-14T17:00:00+02:00","dosis":{"value":10,"ucumUnit":"mg"}},
  {"uitvoeringNr":68340357,"cnk":"0044842","omschrijving":"GLUCOPHAGE","datumVan":"2019-07-14T18:00:00+02:00","datumTot":"2019-07-14T18:00:00+02:00","dosis":{"value":500,"ucumUnit":"mg"}},
  {"uitvoeringNr":68322536,"cnk":"0835017","omschrijving":"GLURENORM","datumVan":"2019-07-14T08:00:00+02:00","datumTot":"2019-07-14T08:00:00+02:00","dosis":{"value":10,"ucumUnit":"mg"}},
  {"uitvoeringNr":68322551,"cnk":"2556512","omschrijving":"GALVUS","datumVan":"2019-07-14T08:00:00+02:00","datumTot":"2019-07-14T08:00:00+02:00","dosis":{"value":50,"ucumUnit":"mg"}},
  {"uitvoeringNr":68322534,"cnk":"0044842","omschrijving":"GLUCOPHAGE","datumVan":"2019-07-14T07:00:00+02:00","datumTot":"2019-07-14T07:00:00+02:00","dosis":{"value":500,"ucumUnit":"mg"}}
],
"voorbereiding":[
  {"voorschriftNr":149910462,"cnk":"2495786","omschrijving":"ADVAGRAF","datumVan":"2019-07-15T20:00:00+02:00","datumTot":"2018-08-15T20:00:00+02:00","dosis":{"value":1,"ucumUnit":"mg"}},
  {"voorschriftNr":149910462,"cnk":"2495786","omschrijving":"ADVAGRAF","datumVan":"2019-07-15T20:00:00+02:00","datumTot":"2018-08-15T20:00:00+02:00","dosis":{"value":1,"ucumUnit":"mg"}}
],
"zorg":[
  {"zorgNr":1171699670,"definitieCode":"FPgewicht","datum":"2018-08-14T09:59:30.693+02:00","attributen":[
    {"attribuutDefinitieCode":"FPgewichtW","waarde":"45.0"},
    {"attribuutDefinitieCode":"FPgewichtBMI","waarde":"true"},
    {"attribuutDefinitieCode":"FPgewichtogm","waarde":"Gevraagd gewicht"}]}
]
}
')
    xContent <<- httr::content(x)
    expect_equal(httr::status_code(x), 200)
  })
  
  it("allows you to GET an existing request", {
    y <- GET(paste0("request/",xContent$requestId) )
    expect_equal(httr::status_code(y), 200)
    httr::content(y)
  })
  
  it("allows you to GET an existing recommendation", {
    y <- GET(paste0("recommendation/",xContent$requestId) )
    expect_equal(httr::status_code(y), 200)
    httr::content(y)
  })
  
  it("lists all previous requests in an admin interface", {
    y <- GET("requests")
    expect_equal(httr::status_code(y), 200)
    httr::content(y)
    
    y <- GET("requests", auth=F)
    expect_equal(httr::status_code(y), 401)
    #GET /requests
  })
})

### Manual test
if( FALSE ) {
  files <- rev(list.files("D:/OtterDrive/KwsDump20191125/", pattern="^.*json$", full.names=T))
  for(i in files) {
    cat("=== Testing file ", basename(i), "\n")
    x <- POST("request", body=paste(readLines(i), collapse="\n"))
    xContent <- httr::content(x)
    print(xContent)
    browser()
  }
}

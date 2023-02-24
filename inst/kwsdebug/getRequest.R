loadDb <- function(dumpFile, db) {
  if(missing(db)) {
    db <- mongolite::mongo(collection="requests", db="tacrolimuskws")
  }
  db$drop()
  files <- unzip(dumpFile, list = TRUE) %>% arrange(Date)
  for(i in seq(651, nrow(files))) {
    cat(i, "\t", files$Name[i], "\t", format(files$Date[i], usetz=TRUE), "\n")
    contents <- unz(description=dumpFile, filename = files$Name[i])
    requestJson <- readLines(contents)
    close(contents)
    id <- stringr::str_match(files$Name[i], "/([^/]+).json")[,2]
    
    
    # computeRecommendation(
    #   requestJson = requestJson,
    #   db=db
    # )
    # i<- i+1
    
    recommendation <- safeRun({
      computeRecommendation(
        requestJson = requestJson,
        db=db
      )
    })
    if(!is.null(recommendation$error) && 
       !(recommendation$error$codeName %in% c("NON_EXECUTED_VOORSCHRIFT_IN_PAST", "NO_PRESCRIPTION_TO_UPDATE", "NOT_IN_STUDY")))
      browser()
    
    retValue <- stringify(recommendation)
    retValue$value <- NULL
    retValue$uitvoering <- recommendation$value$updatedPrescriptions
    retValue <- retValue %>% purrr::discard(is.null) # Filter NULL elements
    
    pair <- list(request=requestJson, recommendation=retValue)
    
    pair$received_at <- format(files$Date[i], usetz=TRUE)
    pair$created_at <- format(files$Date[i], usetz=TRUE)
    pair$id <- id
    
    db$insert(rjson::toJSON(pair))
    
  }
}
loadDb("C:/Users/rfaelens/Downloads/dump (1).zip")

getRequest <- function(id, url="http://localhost:8080/request") {
  url <- paste0(url, "/", id)
  res <- httr::GET(url,
            httr::add_headers(`X-KWS-Token`="myKey"))
  httr::content(res, as = "text", encoding="UTF-8")
}

submitRequest <- function(json, url="http://localhost:8080/request") {
  res <- httr::POST(url,
                   httr::add_headers(`X-KWS-Token`="myKey"),
                   body = json)
  httr::content(res, as = "text", encoding="UTF-8")
}


patient <- readr::read_file("C:/Users/rfaelens/Downloads/demo.json")

x <- submitRequest(patient)
x <- tacrolimuskws::computeRecommendation(patient)



patient <- getRequest(1354)
sink("D:/1354.json"); cat(patient); sink()
patient <- readLines("D:/1354.json")
tacrolimuskws::computeRecommendation(patient)

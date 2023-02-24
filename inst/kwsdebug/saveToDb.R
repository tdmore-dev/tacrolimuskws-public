dir <- "D:/OtterDrive/KwsDump20200305//"
files <- list.files(dir)
mtimes <- file.info(file.path(dir, files))$mtime
files <- files[ order(-1*as.numeric(mtimes)) ]

db <- mongolite::mongo(db="tacrolimuskws", collection="requests")

db$drop()

p <- dplyr::progress_estimated(n=length(files))
lapply(files, function(i){
  p$tick()$print()
  content <- paste( readLines(file.path(dir, i)), collapse="\n")
  received_at <- file.info(file.path(dir, i))$mtime
  
  tacrolimuskws:::plumber_request(req=list(postBody=content), res=NULL, webApiDB=db, shinyAppBaseURL="", received_at=received_at)
  
  # recommendation <- NULL
  # recommendation <- safeRun(computeRecommendation(content)) #feel free to comment this out
  # recommendation <- stringify(recommendation)
  # 
  # recommendation$uitvoering <- recommendation$value$updatedPrescriptions
  # recommendation$value <- NULL
  # recommendation <- recommendation %>% purrr::discard(is.null) # Filter NULL elements
  # saveRequest(db, request = content, received_at=received_at, recommendation = recommendation, id=i)
})


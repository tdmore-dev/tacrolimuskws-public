options(tdmore.warnIov=FALSE) #do not warn about adding an OCC column each time...
suppressPackageStartupMessages({
  library(tdmore)
  library(shinytdmore)
  library(tacrolimuskws)
  library(crosstalk)
  library(plotly)
})
Sys.setenv(TZ='Europe/Brussels')

dbConfig <- list(
  user = Sys.getenv("KWS_CONFIG_MONGODB_ADMINUSERNAME"),
  password = Sys.getenv("KWS_CONFIG_MONGODB_ADMINPASSWORD"),
  server = Sys.getenv("KWS_CONFIG_MONGODB_SERVER", unset="localhost")
)
url <- composeUrl(host=dbConfig$server, user=dbConfig$user, password = dbConfig$password)
db <- mongolite::mongo(url=url, db="tacrolimuskws", collection="requests")
modelDir <- system.file("models", package="tacrolimuskws")

options(show.error.messages=TRUE)

tacrolimuskws::shinyApp(db, modelDir)
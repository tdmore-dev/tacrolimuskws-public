#' Perform a smoke test on a running shiny application
#' @export
testApplication <- function(web, url, TIMEOUT=10) {
  message("Opening shiny site")
  web$go(url)
  message("Waiting until #timevis appears")
  web$setTimeout(implicit=TIMEOUT*1000) #in ms
  web$findElement("#timevis > div.vis-timeline.vis-bottom")
  
  message("Opening prediction tab")
  predictionTab <- web$findElement("a[data-value=prediction]")
  predictionTab$click()
  
  message("Waiting until .plotly appears")
  web$findElement("#prediction-plots-population > div > div > svg:nth-child(1)")
  message("DONE")
  
  return(TRUE)
}
#'
#' Reports tab user interface.
#'
#' @param id namespace id
#' @return a panel
#' 
#' @export
#'
kwsReportsTabUI <- function(id) {
  ns <- NS(id)
  panel <- tabPanel(
    "Reports",
    icon = icon("file-text"),
    HTML("
    Reports tab under construction.<br/>
    Please note that this tab is specific to Tacrolimus KWS.
    ")
  )
  return(panel)
}

#' Reports tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' 
#' @export
#' 
kwsReportsTab <- function(input, output, session) {
  # Nothing to do
}
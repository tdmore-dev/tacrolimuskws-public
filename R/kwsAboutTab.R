#' About tab user interface.
#'
#' @param id namespace id
#' 
#' @return the about tab panel
#' @export
#' 
kwsAboutTabUI <- function(id) {
  ns <- NS(id)
  panel <- tabPanel(
    "About",
    icon = icon("question"),
    HTML("
    About tab under construction.<br/>
    Please note that this tab is specific to Tacrolimus KWS.
    ")
  )
  return(panel)
}

#' About tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' 
#' @export
#' 
kwsAboutTab <- function(input, output, session) {
  # Nothing to do
}
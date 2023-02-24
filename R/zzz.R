`%>%` <- magrittr::`%>%`
`%||%` <- function(a, b) {
  if(!is.null(a)) a else b
}

#' @importFrom rlang .data
NULL

str <- function(df) { paste(capture.output(print(df)), collapse="\n") }

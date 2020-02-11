#' Title Launch mlr3shiny
#'
#' @description
#' Launch an instance of mlr3shiny to perform machine learning in Shiny using mlr3.
#'
#' @param launch Argument used for testing purposes, Default is TRUE.
#'
#' @example
#' launchMlr3Shiny()
#'
#' Argument
#' @export
launchMlr3Shiny <- function(launch = TRUE) {
  appDir <- system.file("mlr3shiny", package = "mlr3shiny")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mlr3shiny`.", call. = FALSE)
  }
  if (launch) {
    shiny::runApp(appDir, display.mode = 'normal')
  }
  # for testing
  if (!launch) {
    return(shiny::shinyAppDir(appDir))
  }
}

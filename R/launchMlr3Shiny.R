
#' Title Launch mlr3shiny
#'
#' @description
#' Launch an instance of mlr3shiny to perform machine learning in Shiny using mlr3.
#'
#' @export
launchMlr3Shiny <- function() {
  appDir <- system.file("mlr3shiny", package = "mlr3shiny")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mlr3shiny`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

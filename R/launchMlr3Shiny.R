#' Title Launch mlr3shiny
#'
#' @description
#' Launch an instance of mlr3shiny to perform machine learning in Shiny using mlr3.
#'
#' @param test Argument used for testing purposes. Default is FALSE.
#' @return None
#' @examples
#' if(interactive()){
#' launchMlr3Shiny()
#'}
#'
#' @export
launchMlr3Shiny <- function(test = FALSE) {
  appDir <- system.file("mlr3shiny", package = "mlr3shiny")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mlr3shiny`.", call. = FALSE)
  }
  if (!test) {
    shiny::runApp(appDir, display.mode = 'normal')
  }
  # for testing
  if (test) {
    return(shiny::shinyAppDir(appDir))
  }
}

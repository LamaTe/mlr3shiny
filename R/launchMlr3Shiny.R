#' Title Launch mlr3shiny
#'
#' @description
#' Launch an instance of mlr3shiny to perform machine learning in Shiny using mlr3.
#'
#' @param test Argument used for testing purposes. Default is FALSE.
#' @return None
#' @examples
<<<<<<< HEAD
#' if(interactive()){
#' launchMlr3Shiny()
#'}
=======
#' if (interactive()) {
#'    launchMlr3Shiny()
#' }
>>>>>>> b3a20c2f93a445bca1de68530952c3fd869a4beb
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

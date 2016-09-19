#' Run odyssey app function
#' @keywords odyssey shiny
#' @export
#' @examples
#' This function runs the odyssey application.
#' quest()

runQuest <- function() {
  wd<<-getwd()
  appDir <- system.file("odyssey", package = "odyssey")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `odyssey`.", call. = FALSE)
  }

  shiny::runApp(appDir)
}

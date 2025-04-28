#' Launch the EQ-5D Shiny App
#'
#' This function starts the Shiny application bundled with the package.
#'
#' @export
runEQ5DApp <- function() {
  appDir <- system.file("app", package = "Shiny.EQ5D")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Re-install the Shiny.EQ5D package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

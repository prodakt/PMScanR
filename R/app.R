# app.R

#' Launch the PMScanR Shiny app
#' @examples
#' runPMScanRShiny()
#' @return Launches the GUI for users not familiar with R and those who prefer graphical interface.
#' @export
runPMScanRShiny <- function() {
  app <- build_app()
  shiny::runApp(app)
}

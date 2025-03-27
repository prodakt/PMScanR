# app.R

#' Launch the PMScanR Shiny app
#' @export
runPMScanRShiny <- function() {
  app <- build_app()
  shiny::runApp(app)
}

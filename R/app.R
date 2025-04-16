# app.R

#' Launch the PMScanR Shiny app
#' @return Launches the GUI for users not familiar with R and those who prefer graphical interface.
#' @export
runPMScanRShiny <- function() {
  app <- build_app()
  shiny::runApp(app)
}

# build_app.R

#' Build the PMScanR Shiny app
#' @return A Shiny app object
#' @noRd
build_app <- function() {
  options(shiny.maxRequestSize = 60*1024^2)
  ui <- build_ui
  server <- build_server
  shinyApp(ui = ui, server = server)
}

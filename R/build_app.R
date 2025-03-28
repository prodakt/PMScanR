# R/build_app.R

#' Build the PMScanR Shiny app
#' @return A Shiny app object
#' @export
build_app <- function() {
  options(shiny.maxRequestSize = 30*1024^2)
  ui <- build_ui
  server <- build_server
  shinyApp(ui = ui, server = server)
}

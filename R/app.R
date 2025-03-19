# app.R
#' @import shiny
#' @import plotly
#' @export
runShinyApp <- function() {
  options(shiny.maxRequestSize = 30*1024^2)
  shinyApp(ui, server)
}

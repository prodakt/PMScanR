# app.R
#' @import shiny
#' @export
runShinyApp <- function() {
  options(shiny.maxRequestSize = 30*1024^2)
  shinyApp(ui, server)
}

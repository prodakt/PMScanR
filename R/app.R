#' Launch the PMScanR Shiny Application
#'
#' Calling this function will launch the interactive graphical user interface
#' for the PMScanR package.
#'
#' @details
#' This function sets a higher file upload size limit for Shiny and then
#' launches the application, which is built using an internal UI function
#' (`buildUi`) and server function (`buildServer`).
#'
#' @return This function is called for its side effect of launching the Shiny
#'   application and does not return a value.
#'
#' @examples
#' if (interactive()) {
#'   # To run the app, simply call the function
#'   runPMScanRShiny()
#' }
#'
#' @export
#' @importFrom shiny shinyApp
runPMScanRShiny <- function() {
    options(shiny.maxRequestSize = 60 * 1024 ^ 2)
    ui <- buildUi()
    server <- buildServer
    shiny::shinyApp(ui = ui, server = server)
}

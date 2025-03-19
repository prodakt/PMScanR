# interface.R
#' @import shiny
#' @importFrom shiny fluidPage uiOutput
#' @export
ui <- fluidPage(
  uiOutput("page_content")
)

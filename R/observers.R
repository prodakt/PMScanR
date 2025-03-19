# observers.R

#' @import shiny
#' @importFrom shiny observeEvent
#' @export
setup_observers <- function(input, current_page) {
  # Navigate to Prosite analysis page
  observeEvent(input$goto_prosite, {
    current_page("prosite")
  })
  
  # Navigate to Data analysis page
  observeEvent(input$goto_data, {
    current_page("data")
  })
  
  # Back to main page from title
  observeEvent(input$back_to_main_title, {
    current_page("main")
  })
}
#' PMScanR Server Module
#' @param id Module namespace ID
#' @export
pmscanr_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) { # Set up a Shiny module with the given namespace
    sequences <- shiny::reactiveVal()         # Create a reactive value to store sequences
    analysis_time <- shiny::reactiveVal()     # Create a reactive value to store analysis time
    
    pmscanr_observers(input, session, sequences, analysis_time) # Run observers to update sequences and time
    pmscanr_outputs(output, sequences, input, analysis_time)    # Run outputs to render UI elements
  })
}

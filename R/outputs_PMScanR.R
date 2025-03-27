# R/outputs_PMScanR.R

#' Set up outputs for the PMScanR server
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param prosite_results_data Reactive value for Prosite results
#' @param data_matrix Reactive value for data matrix
#' @param original_data Reactive value for original data
#' @param prosite_status_text Reactive value for status text
#' @param loading Reactive value for loading state
#''@import shiny
#' @import shinyFiles
#' @import bslib
#' @import bsicons
#' @return None
#' @noRd
setup_outputs <- function(input, output, session, prosite_results_data, data_matrix, original_data, prosite_status_text, loading) {
  # Logo
  output$logo <- renderImage({
    list(src = system.file("img/PMlogo.png", package = "PMScanR"), height = "100%")
  }, deleteFile = FALSE)

  # Run Prosite button
  output$run_prosite_button <- renderUI({
    if (loading()) {
      tags$button(
        class = "btn btn-primary",
        type = "button",
        disabled = "disabled",
        tags$span(class = "spinner-border spinner-border-sm", `aria-hidden` = "true"),
        tags$span(role = "status", "Loading...")
      )
    } else {
      actionButton(
        "run_prosite",
        "Run Analysis",
        class = "btn btn-primary",
        style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;"
      )
    }
  })

  # Status outputs
  output$prosite_analysis_status_home <- renderText({
    "Ready (Home)"
  })

  output$prosite_analysis_status_prosite <- renderText({
    if (is.null(input$file_upload)) {
      "No file uploaded yet"
    } else {
      paste("File uploaded:", input$file_upload$name)
    }
  })

  # Prosite analysis status
  output$prosite_analysis_status <- renderText({
    prosite_status_text()
  })

  # Prosite results
  output$prosite_results_output <- renderTable({
    print(prosite_results_data())
    if (!is.null(prosite_results_data())) {
      as.data.frame(prosite_results_data())
    } else {
      data.frame(Message = "Analysis completed. Results are ready in the 'Results' tab.")
    }
  })

  # Heatmap 1
  output$heatmap1_output <- renderPlotly({
    req(data_matrix())
    matrix2hm(input = data_matrix(), x = input$highlight_x1, y = input$highlight_y1)
  })

  # Heatmap 2
  output$heatmap2_output <- renderPlotly({
    req(data_matrix())
    matrix2hm_2(input = data_matrix(), x = input$highlight_x2, y = input$highlight_y2)
  })

  # Home Heatmap
  output$home_heatmap_output <- renderPlotly({
    req(data_matrix())
    matrix2hm(input = data_matrix(), x = NULL, y = NULL)
  })

  # Pie Chart
  output$piechart_output <- renderPlot({
    req(original_data())
    freqPie(original_data())
  }, height = 800)
}

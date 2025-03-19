#' @import shiny
#' @importFrom shiny reactiveVal observeEvent updateTextInput req showNotification renderText updateTabsetPanel renderTable
#' @importFrom shinyFiles shinyDirChoose parseDirPath
#' @importFrom rtracklayer import.gff
#' @export
server <- function(input, output, session) {
  # Existing reactive value for current page
  current_page <- reactiveVal("main")
  
  # Existing reactive variables for Prosite analysis (leave unchanged)
  prosite_results_data <- reactiveVal(NULL)
  prosite_params <- reactiveValues(
    output_dir = getwd(),
    output_name = "prosite_results.txt",
    output_format = "gff",
    ps_scan_path = NULL,
    patterns_dat_path = NULL,
    pf_scan_path = NULL,
    os_choice = .Platform$OS.type
  )
  prosite_analysis_run <- reactiveVal(FALSE)
  
  # New reactive value for Prosite analysis status text
  prosite_status_text <- reactiveVal("Analysis status: waiting for inputs") # Initial status
  
  # New reactive values for Data analysis
  data_matrix <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  
  # Render the dynamic UI (already exists)
  output$page_content <- render_page_content(current_page)
  
  # Existing navigation observers (leave unchanged)
  setup_observers(input, current_page)
  
  # Existing directory selection logic (leave unchanged)
  volumes <- getLogicalDrives()
  
  observeEvent(input$output_dir_button, {
    shinyDirChoose(input, 'output_dir_button', roots = volumes, filetypes = NULL, restrictions = NULL, session = session)
  })
  
  observe({
    if (is.list(input$output_dir_button) && length(input$output_dir_button) > 0) {
      tryCatch({
        selected_dir <- parseDirPath(volumes, input$output_dir_button)
        updateTextInput(session, "output_dir", value = selected_dir)
      }, error = function(e) {
        print(paste("Error parsing directory:", e$message))
        updateTextInput(session, "output_dir", value = "Error selecting directory")
      })
    } else {
      updateTextInput(session, "output_dir", value = "")
    }
  })
  
  # Existing "Run Analysis" for Prosite (leave unchanged, but modified to update status)
  observeEvent(input$run_prosite, {
    # Update status to "running" when button is clicked
    prosite_status_text("Analysis status: running...")
    
    in_file <- input$file_upload$datapath
    out_dir <- input$output_dir
    out_name <- input$output_name
    out_format <- input$output_format
    ps_scan_path <- input$ps_scan_file$datapath
    patterns_dat_path <- input$patterns_dat_file$datapath
    pf_scan_path <- input$pf_scan_file$datapath
    os_choice <- input$os_choice
    
    if (is.null(in_file)) {
      showNotification("Please upload an input file.", type = "warning")
      prosite_status_text("Analysis status: waiting for inputs") # Reset status on error
      return()
    }
    
    full_output_path <- file.path(out_dir, out_name)
    
    tryCatch({
      print(paste("Running Prosite analysis with:", in_file, full_output_path, out_format, ps_scan_path, patterns_dat_path, pf_scan_path, os_choice))
      runPsScan(in_file = in_file, out_file = full_output_path, out_format = out_format,
                ps_scan = if (!is.null(ps_scan_path) && length(ps_scan_path) > 0) ps_scan_path else NULL,
                patterns_dat = if (!is.null(patterns_dat_path) && length(patterns_dat_path) > 0) patterns_dat_path else NULL,
                pf_scan = if (!is.null(pf_scan_path) && length(pf_scan_path) > 0) pf_scan_path else NULL,
                OS = if (!is.null(os_choice) && os_choice != "") os_choice else NULL)
      
      if (out_format == "gff") {
        prosite_results_data(rtracklayer::import.gff(full_output_path))
      } else if (out_format == "psa") {
        prosite_results_data(read.psa(full_output_path))
      }
      
      prosite_analysis_run(TRUE)
      
      output$prosite_results_output <- renderTable({
        if (!is.null(prosite_results_data())) {
          as.data.frame(prosite_results_data())
        } else {
          data.frame(Message = "Analysis completed. Results are ready in the 'Results' tab.")
        }
      })
      
      updateTabsetPanel(session, "results_tabs", selected = "results")
      prosite_status_text("Analysis status: completed") # Update status on success
      
    }, error = function(e) {
      showNotification(paste("Error during analysis:", e$message), type = "error", duration = 10)
      prosite_status_text("Analysis status: error") # Update status on error
    })
  })
  
  # Render the Prosite analysis status text
  output$prosite_analysis_status <- renderText({
    prosite_status_text()
  })
  
  
  # New "Analyse Data" observer for Data analysis
  observeEvent(input$analyse_data, {
    if (input$data_source == "Use Prosite analysis data") {
      if (!prosite_analysis_run()) {
        showNotification("Please run the Prosite analysis first.", type = "warning")
        return()
      }
      data <- prosite_results_data()
      if (inherits(data, "GRanges")) {
        data <- as.data.frame(data)
      }
    } else {
      req(input$uploaded_file)
      if (input$input_format == "gff") {
        data <- rtracklayer::import.gff(input$uploaded_file$datapath)
        data <- as.data.frame(data)
      } else if (input$input_format == "psa") {
        data <- read.psa(input$uploaded_file$datapath)
      }
    }
    matrix <- gff2matrix(data)
    data_matrix(matrix)
    original_data(data)
    updateTabsetPanel(session, "data_tabs", selected = "heatmap1")
  })
  
  # Rendering for Heatmap 1
  output$heatmap1_output <- renderPlotly({
    req(data_matrix())
    matrix2hm(input = data_matrix(), x = NULL, y = NULL)
  })
  
  # Rendering for Heatmap 2
  output$heatmap2_output <- renderPlotly({
    req(data_matrix())
    matrix2hm_2(input = data_matrix(), x = NULL, y = NULL)
  })
  
  observe({
    req(data_matrix())
    updateSelectInput(session, "highlight_x1", choices = colnames(data_matrix()))
    updateSelectInput(session, "highlight_y1", choices = rownames(data_matrix()))
    updateSelectInput(session, "highlight_x2", choices = colnames(data_matrix()))
    updateSelectInput(session, "highlight_y2", choices = rownames(data_matrix()))
  })
  
  # Rendering for Heatmap 1
  output$heatmap1_output <- renderPlotly({
    req(data_matrix())
    matrix2hm(input = data_matrix(), x = input$highlight_x1, y = input$highlight_y1)
  })
  
  # Rendering for Heatmap 2
  output$heatmap2_output <- renderPlotly({
    req(data_matrix())
    matrix2hm_2(input = data_matrix(), x = input$highlight_x2, y = input$highlight_y2)
  })
  
  # Rendering for Pie Chart
  output$piechart_output <- renderPlot({
    req(original_data())
    freqPie(original_data())
  }, height = 800)
  
  # Existing navigation for Prosite Home tab (leave unchanged)
  observeEvent(input$results_tabs, {
    if (input$results_tabs == "home") {
      current_page("main")
    }
  })
  
  # New navigation for Data Home tab
  observeEvent(input$data_tabs, {
    if (input$data_tabs == "home") {
      current_page("main")
    }
  })
}
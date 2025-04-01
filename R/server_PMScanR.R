#' Create the server function for PMScanR
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @import shiny
#' @import shinyFiles
#' @import bslib
#' @import bsicons
#' @import rtracklayer
#' @import seqinr
#' @import ggseqlogo
#' @return A Shiny server function
#' @noRd
build_server <- function(input, output, session) {

  # Existing reactive variables for Prosite analysis
  prosite_results_data <- reactiveVal(NULL)
  prosite_params <- reactiveValues(
    output_dir = getwd(),
    output_name = NULL,
    output_format = "gff",
    ps_scan_path = NULL,
    patterns_dat_path = NULL,
    pf_scan_path = NULL,
    os_choice = .Platform$OS.type
  )
  prosite_analysis_run <- reactiveVal(FALSE)
  prosite_status_text <- reactiveVal("Analysis status: waiting for inputs") # Initial status
  data_matrix <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  volumes <- getLogicalDrives()
  loading <- reactiveVal(FALSE)

  # Render the logo with the correct path
  output$logo <- renderImage(
    {
      logo_path <- system.file("img/PMlogo.png", package = "PMScanR")
      if (logo_path == "") {
        stop("Logo file not found in the package.")
      }
      list(src = logo_path, height = "100%")
    },
    deleteFile = FALSE
  )

  output$run_prosite_button <- renderUI({
    if (loading()) {
      # Loading state button with spinner
      tags$button(
        class = "btn btn-primary",
        type = "button",
        disabled = "disabled",
        tags$span(class = "spinner-border spinner-border-sm", `aria-hidden` = "true"),
        tags$span(role = "status", "Loading...")
      )
    } else {
      # Normal state button
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

  # Simulate analysis when button is clicked
  observeEvent(input$run_prosite, {
    # Set loading state to TRUE
    loading(TRUE)

    # Simulate a long-running process (e.g., 3 seconds)
    Sys.sleep(3)

    # After process completes, set loading back to FALSE
    loading(FALSE)
  })

  # Render the Prosite analysis status text
  output$prosite_analysis_status <- renderText({
    prosite_status_text()
  })

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

  # Existing "Run Analysis" for Prosite
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
    print(out_name)
    print(out_format)
    print(out_dir)
    if (is.null(in_file)) {
      showNotification("Please upload an input file.", type = "warning")
      prosite_status_text("Analysis status: waiting for inputs") # Reset status on error
      return()
    }

    # Check if output directory exists and is writable
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
      if (!dir.exists(out_dir)) {
        showNotification("Failed to create output directory.", type = "error")
        prosite_status_text("Analysis status: error")
        return()
      }
    }
    if (!file.access(out_dir, mode = 2) == 0) {
      showNotification("Output directory is not writable.", type = "error")
      prosite_status_text("Analysis status: error")
      return()
    }

    full_output_path <- file.path(out_dir, out_name)
    print(full_output_path)

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

      prosite_params$output_name <- out_name
      prosite_params$output_dir <- out_dir
      prosite_params$output_format <- out_format

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
      prosite_status_text(paste("Analysis status: error -", e$message)) # Include error message in status
    })
  })

  # "Analyse Data" observer for Data analysis
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
  })

  observe({
    if (input$seqlogo_type == "Motifs") {
      if (input$motif_data_source == "Upload my own PSA file") {
        req(input$psa_file_seqlogo)
        motifs <- extract_protein_motifs(input$psa_file_seqlogo$datapath)
        updateSelectInput(session, "motif_id", choices = names(motifs))
      } else if (input$motif_data_source == "Use Prosite analysis PSA output") {
        if (prosite_analysis_run() && prosite_params$output_format == "psa") {
          psa_file <- file.path(prosite_params$output_dir, prosite_params$output_name)
          if (!file.exists(psa_file)) {
            showNotification("Prosite analysis PSA output file not found.", type = "warning")
            updateSelectInput(session, "motif_id", choices = NULL)
            return()
          }
          motifs <- extract_protein_motifs(psa_file)
          updateSelectInput(session, "motif_id", choices = names(motifs))
        } else {
          updateSelectInput(session, "motif_id", choices = NULL)
          showNotification("Prosite analysis with PSA output is not available.", type = "warning")
        }
      }
    } else {
      updateSelectInput(session, "motif_id", choices = NULL)
    }
  })

  # Rendering for Heatmap 1
  output$heatmap1_output <- renderPlotly({
    req(data_matrix())
    matrix2hm(input = data_matrix(), x = input$highlight_x1, y = input$highlight_y1)
  })

  # Rendering for home heatmap
  output$home_heatmap_output <- renderPlotly({
    req(data_matrix())
    matrix2hm(input = data_matrix(), x = NULL, y = NULL)
  })

  # # Rendering for Heatmap 2
  # output$heatmap2_output <- renderPlotly({
  #   req(data_matrix())
  #   matrix2hm_2(input = data_matrix(), x = input$highlight_x2, y = input$highlight_y2)
  # })

  observe({
    req(data_matrix())
    updateSelectInput(session, "highlight_x1", choices = colnames(data_matrix()))
    updateSelectInput(session, "highlight_y1", choices = rownames(data_matrix()))
    # updateSelectInput(session, "highlight_x2", choices = colnames(data_matrix()))
    # updateSelectInput(session, "highlight_y2", choices = rownames(data_matrix()))
  })

  # Rendering for Pie Chart
  output$piechart_output <- renderPlot({
    req(original_data())
    freqPie(original_data())
  }, height = 800)

  observeEvent(input$generate_seqlogo, {
    if (input$seqlogo_type == "Raw Sequences") {
      req(input$fasta_file_seqlogo)
      tryCatch({
        seq <- seqinr::read.fasta(file = input$fasta_file_seqlogo$datapath, seqtype = if (input$seqtype == "Protein") "AA" else "DNA")
        from <- input$from_pos
        to <- input$to_pos
        if (from > to) {
          showNotification("From position must be less than or equal to To position.", type = "warning")
          return()
        }
        seq_short <- extract_segments(seq = seq, from = from, to = to)
        if (length(seq_short) > 0) {
          output$seqlogo_plot <- renderPlot({
            ggseqlogo(unlist(seq_short), seq_type = if (input$seqtype == "Protein") "aa" else "dna")
          })
        } else {
          showNotification("No sequences found in the specified range.", type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Error generating SeqLogo:", e$message), type = "error")
      })
    } else if (input$seqlogo_type == "Motifs") {
      # Generate seqlogo from motifs
      motifs <- NULL
      if (input$motif_data_source == "Upload my own PSA file") {
        req(input$psa_file_seqlogo)
        motifs <- extract_protein_motifs(input$psa_file_seqlogo$datapath)
      } else if (input$motif_data_source == "Use Prosite analysis PSA output") {
        if (prosite_analysis_run() && prosite_params$output_format == "psa") {
          psa_file <- file.path(prosite_params$output_dir, prosite_params$output_name)
          if (!file.exists(psa_file)) {
            showNotification("Prosite analysis PSA output file not found.", type = "warning")
            return()
          }
          motifs <- extract_protein_motifs(psa_file)
        } else {
          showNotification("Prosite analysis with PSA output is not available.", type = "warning")
          return()
        }
      }
      req(input$motif_id, motifs)
      if (input$motif_id %in% names(motifs)) {
        motif_seqs <- motifs[[input$motif_id]]
        output$seqlogo_plot <- renderPlot({
          ggseqlogo(motif_seqs, seq_type = "aa")
        })
      } else {
        showNotification("Selected motif ID not found.", type = "warning")
      }
    }
  })
}

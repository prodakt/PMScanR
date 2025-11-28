#' Create the server function for PMScanR
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @importFrom shiny reactiveVal reactiveValues renderImage renderUI tags
#' @importFrom shiny actionButton renderText observeEvent updateTextInput
#' @importFrom shiny showNotification updateTabsetPanel req updateSelectInput
#' @importFrom shiny renderPlot renderTable observe
#' @importFrom shinyFiles shinyDirChoose parseDirPath
#' @importFrom ggseqlogo ggseqlogo
#' @importFrom plotly renderPlotly
#' @importFrom rtracklayer import.gff
#' @return A Shiny server function
#' @noRd
buildServer <- function(input, output, session) {
  # --- Reactive Values ---
  prosite_results_data <- reactiveVal(NULL)
  prosite_params <- reactiveValues(output_dir = getwd(), output_name = NULL, output_format = "gff")
  prosite_analysis_run <- reactiveVal(FALSE)
  prosite_status_text <- reactiveVal("Analysis status: waiting for inputs")
  data_matrix <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  volumes <- getLogicalDrives()
  loading <- reactiveVal(FALSE)

  # --- UI Rendering & Outputs (Logo, Status) ---
  output$logo <- renderImage({
    logo_path <- system.file("img/PMlogo.png", package = "PMScanR")
    if (!nzchar(logo_path)) stop("Logo file not found in the package.")
    list(src = logo_path, height = "100%")
  }, deleteFile = FALSE)

  output$run_prosite_button <- renderUI({
    if (loading()) {
      tags$button(class = "btn btn-primary", type = "button", disabled = "disabled", tags$span(class = "spinner-border spinner-border-sm", `aria-hidden` = "true"), tags$span(role = "status", "Running..."))
    } else {
      actionButton("run_prosite", "Run Analysis", class = "btn btn-primary", style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
    }
  })

  output$prosite_analysis_status <- renderText({ prosite_status_text() })

  # --- Observers ---
  observeEvent(input$output_dir_button, { shinyDirChoose(input, 'output_dir_button', roots = volumes, session = session) })
  observe({
    if (is.list(input$output_dir_button)) {
      tryCatch({
        selected_dir <- parseDirPath(volumes, input$output_dir_button)
        updateTextInput(session, "output_dir", value = selected_dir)
      }, error = function(e) { updateTextInput(session, "output_dir", value = "Error selecting directory") })
    }
  })

  # --- MAIN RUN PROSITE LOGIC ---
  observeEvent(input$run_prosite, {
    loading(TRUE)
    prosite_status_text("Analysis status: running...")
    Sys.sleep(0.1)

    in_file <- input$file_upload$datapath
    out_dir <- input$output_dir
    out_name <- input$output_name
    out_format <- input$output_format

    custom_ps_scan_path <- if (!is.null(input$custom_ps_scan)) input$custom_ps_scan$datapath else NULL
    custom_db_path <- if (!is.null(input$custom_prosite_dat)) input$custom_prosite_dat$datapath else NULL
    custom_pfscan_path <- if (!is.null(input$custom_pfscan)) input$custom_pfscan$datapath else NULL

    if (is.null(in_file)) {
      showNotification("Please upload an input file.", type = "warning")
      prosite_status_text("Analysis status: waiting for inputs")
      loading(FALSE)
      return()
    }

    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    if (!file.access(out_dir, mode = 2) == 0) {
      showNotification("Output directory is not writable.", type = "error")
      prosite_status_text("Analysis status: error")
      loading(FALSE)
      return()
    }

    full_output_path <- file.path(out_dir, out_name)

    tryCatch({
      message(sprintf("Starting Prosite analysis. Output will be saved to: %s", full_output_path))

      runPsScan(
        in_file = in_file,
        out_file = full_output_path,
        out_format = out_format,
        ps_scan_path = custom_ps_scan_path,
        database_path = custom_db_path,
        pfscan_path = custom_pfscan_path
      )

      if (out_format == "gff") {
        prosite_results_data(as.data.frame(rtracklayer::import.gff(full_output_path)))
      } else if (out_format == "psa") {
        prosite_results_data(readPsa(full_output_path))
      } else if (out_format == "scan") {
        prosite_results_data(readProsite(full_output_path))
      }

      prosite_analysis_run(TRUE)
      prosite_params$output_name <- out_name
      prosite_params$output_dir <- out_dir
      prosite_params$output_format <- out_format

      output$prosite_results_output <- renderTable({
        req(prosite_results_data())
        as.data.frame(prosite_results_data())
      })

      updateTabsetPanel(session, "results_tabs", selected = "results")
      prosite_status_text("Analysis status: completed")

    }, error = function(e) {
      showNotification(sprintf("Error during analysis: %s", e$message), type = "error", duration = 10)
      prosite_status_text(sprintf("Analysis status: error - %s", e$message))
    }, finally = {
      loading(FALSE)
    })
  })

  # --- Data Analysis & SeqLogo ---
  observeEvent(input$analyse_data, {
    if (input$data_source == "Use Prosite analysis data") {
      if (!prosite_analysis_run()) {
        showNotification("Please run the Prosite analysis first.", type = "warning")
        return()
      }
      data <- prosite_results_data()
      if (inherits(data, "GRanges")) data <- as.data.frame(data)
    } else {
      req(input$uploaded_file)
      if (input$input_format == "gff") {
        data <- as.data.frame(rtracklayer::import.gff(input$uploaded_file$datapath))
      } else if (input$input_format == "psa") {
        data <- readPsa(input$uploaded_file$datapath)
      } else if (input$input_format == "scan") {
        data <- readProsite(input$uploaded_file$datapath)
      }
    }
    matrix <- gff2matrix(data)
    data_matrix(matrix)
    original_data(data)
  })

  observe({
    if (input$seqlogo_type == "Motifs") {
      motifs <- NULL
      file_to_process <- NULL
      if (input$motif_data_source == "Upload my own result file") {
        req(input$psa_file_seqlogo)
        file_to_process <- input$psa_file_seqlogo$datapath
      } else if (input$motif_data_source == "Use Prosite analysis output") {
        if (prosite_analysis_run()) {
          file_to_process <- file.path(prosite_params$output_dir, prosite_params$output_name)
          if (!file.exists(file_to_process)) {
            showNotification("Prosite analysis output file not found.", type = "warning")
            file_to_process <- NULL
          }
        } else {
          updateSelectInput(session, "motif_id", choices = NULL)
        }
      }
      if (!is.null(file_to_process)) {
        tryCatch({
          motifs <- extractProteinMotifs(file_to_process, format = "auto")
          updateSelectInput(session, "motif_id", choices = names(motifs))
        }, error = function(e) {
          showNotification(paste("Error reading motif file:", e$message), type = "error")
          updateSelectInput(session, "motif_id", choices = NULL)
        })
      }
    } else {
      updateSelectInput(session, "motif_id", choices = NULL)
    }
  })

  output$heatmap1_output <- renderPlotly({ req(data_matrix()); matrix2OP(input = data_matrix(), x = input$highlight_x1, y = input$highlight_y1) })
  output$home_heatmap_output <- renderPlotly({ req(data_matrix()); matrix2OP(input = data_matrix(), x = NULL, y = NULL) })
  observe({ req(data_matrix()); updateSelectInput(session, "highlight_x1", choices = colnames(data_matrix())); updateSelectInput(session, "highlight_y1", choices = rownames(data_matrix())) })
  output$piechart_output <- renderPlot({ req(original_data()); freqPie(original_data()) }, height = 800)

  observeEvent(input$generate_seqlogo, {
    if (input$seqlogo_type == "Raw Sequences") {
      req(input$fasta_file_seqlogo)
      tryCatch({
        seq_data <- seqinr::read.fasta(file = input$fasta_file_seqlogo$datapath, seqtype = if (input$seqtype == "Protein") "AA" else "DNA")
        from <- input$from_pos; to <- input$to_pos
        if (from > to) { showNotification("From > To position error", type="warning"); return() }
        seq_short <- extractSegments(sequences = seq_data, from = from, to = to)
        if (length(seq_short) > 0) output$seqlogo_plot <- renderPlot({ ggseqlogo(unlist(seq_short), seq_type = if (input$seqtype == "Protein") "aa" else "dna") })
        else showNotification("No sequences found.", type = "warning")
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    } else if (input$seqlogo_type == "Motifs") {
      req(input$motif_id)
      file_to_process <- NULL
      if (input$motif_data_source == "Upload my own result file") {
        req(input$psa_file_seqlogo); file_to_process <- input$psa_file_seqlogo$datapath
      } else if (input$motif_data_source == "Use Prosite analysis output") {
        if (prosite_analysis_run()) file_to_process <- file.path(prosite_params$output_dir, prosite_params$output_name)
      }
      if (!is.null(file_to_process)) {
        tryCatch({
          motifs <- extractProteinMotifs(file_to_process, format = "auto")
          if (input$motif_id %in% names(motifs)) {
            motif_seqs <- motifs[[input$motif_id]]
            output$seqlogo_plot <- renderPlot({ ggseqlogo(motif_seqs, seq_type = "aa") })
          } else showNotification("Selected motif ID not found.", type = "warning")
        }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
      }
    }
  })
}

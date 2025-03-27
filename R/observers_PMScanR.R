# R/observers_PMScanR.R

#' Set up observers for the PMScanR server
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param prosite_status_text Reactive value for status text
#' @param prosite_analysis_run Reactive value for analysis run status
#' @param prosite_params Reactive values for Prosite parameters
#' @param data_matrix Reactive value for data matrix
#' @param original_data Reactive value for original data
#' @param loading Reactive value for loading state
#' @param volumes Logical drives for directory selection
#' @import shinyFiles
#' @import rtracklayer
#' @return None
#' @noRd
setup_observers <- function(input, output, session, prosite_status_text, prosite_analysis_run, prosite_params, data_matrix, original_data, loading, volumes) {
  # Simulate analysis when button is clicked
  observeEvent(input$run_prosite, {
    loading(TRUE)
    Sys.sleep(3)
    loading(FALSE)
  })

  # Directory selection
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

  # Run Prosite analysis
  observeEvent(input$run_prosite, {
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
      prosite_status_text("Analysis status: waiting for inputs")
      return()
    }

    full_output_path <- file.path(out_dir, out_name)

    tryCatch({
      print(paste("Running Prosite analysis with:", in_file, full_output_path, out_format, ps_scan_path, patterns_dat_path, pf_scan_path, os_choice))
      runPsScan(
        in_file = in_file,
        out_file = full_output_path,
        out_format = out_format,
        ps_scan = if (!is.null(ps_scan_path) && length(ps_scan_path) > 0) ps_scan_path else NULL,
        patterns_dat = if (!is.null(patterns_dat_path) && length(patterns_dat_path) > 0) patterns_dat_path else NULL,
        pf_scan = if (!is.null(pf_scan_path) && length(pf_scan_path) > 0) pf_scan_path else NULL,
        OS = if (!is.null(os_choice) && os_choice != "") os_choice else NULL
      )

      if (out_format == "gff") {
        prosite_results_data(rtracklayer::import.gff(full_output_path))
      } else if (out_format == "psa") {
        prosite_results_data(read.psa(full_output_path))
      }

      prosite_analysis_run(TRUE)

      prosite_params$output_name <- out_name
      prosite_params$output_dir <- out_dir
      prosite_params$output_format <- out_format
      print(prosite_results_data())

      prosite_status_text("Analysis status: completed")
    }, error = function(e) {
      showNotification(paste("Error during analysis:", e$message), type = "error", duration = 10)
      prosite_status_text("Analysis status: error")
    })
  })

  # Analyse Data observer
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
      } else if (input$input_format == "prosite") {
        data <- read.prosite(input$uploaded_file$datapath)
      }
    }
    matrix <- gff2matrix(data)
    data_matrix(matrix)
    original_data(data)
  })

  # Update motif ID choices
  observe({
    if (input$seqlogo_type == "Motifs") {
      if (input$motif_data_source == "Upload my own PSA file") {
        req(input$psa_file_seqlogo)
        motifs <- extract_protein_motifs(input$psa_file_seqlogo$datapath)
        updateSelectInput(session, "motif_id", choices = names(motifs))
      } else if (input$motif_data_source == "Use Prosite analysis PSA output") {
        if (prosite_analysis_run() && prosite_params$output_format == "psa") {
          psa_file <- file.path(prosite_params$output_dir, prosite_params$output_name)
          motifs <- extract_protein_motifs(psa_file)
          updateSelectInput(session, "motif_id", choices = names(motifs))
        } else {
          updateSelectInput(session, "motif_id", choices = NULL)
          showNotification("Prosite analysis output is not available. Please run analysis first", type = "warning")
        }
      }
    } else {
      updateSelectInput(session, "motif_id", choices = NULL)
    }
  })

  # Update heatmap highlight choices
  observe({
    req(data_matrix())
    updateSelectInput(session, "highlight_x1", choices = colnames(data_matrix()))
    updateSelectInput(session, "highlight_y1", choices = rownames(data_matrix()))
    updateSelectInput(session, "highlight_x2", choices = colnames(data_matrix()))
    updateSelectInput(session, "highlight_y2", choices = rownames(data_matrix()))
  })

  # Generate SeqLogo
  observeEvent(input$generate_seqlogo, {
    if (input$seqlogo_type == "Raw Sequences") {
      if (input$seqtype == "Protein") {
        req(input$fasta_file_seqlogo)
        seq <- read.fasta(file = input$fasta_file_seqlogo$datapath, seqtype = "AA")
        from <- input$from_pos
        to <- input$to_pos
        seq_short <- extract_segments(seq = seq, from = from, to = to)
        if (length(seq_short) > 0) {
          output$seqlogo_plot <- renderPlot({
            ggseqlogo(unlist(seq_short), seq_type = "aa")
          })
        } else {
          showNotification("No sequences found in the specified range.", type = "warning")
        }
      } else {
        req(input$fasta_file_seqlogo)
        seq <- read.fasta(file = input$fasta_file_seqlogo$datapath, seqtype = "DNA")
        from <- input$from_pos
        to <- input$to_pos
        seq_short <- extract_segments(seq = seq, from = from, to = to)
        if (length(seq_short) > 0) {
          output$seqlogo_plot <- renderPlot({
            ggseqlogo(unlist(seq_short), seq_type = "dna")
          })
        } else {
          showNotification("No sequences found in the specified range.", type = "warning")
        }
      }
    } else if (input$seqlogo_type == "Motifs") {
      motifs <- NULL
      if (input$motif_data_source == "Upload my own PSA file") {
        req(input$psa_file_seqlogo)
        motifs <- extract_protein_motifs(input$psa_file_seqlogo$datapath)
      } else if (input$motif_data_source == "Use Prosite analysis PSA output") {
        if (prosite_analysis_run() && prosite_params$output_format == "psa") {
          psa_file <- file.path(prosite_params$output_dir, prosite_params$output_name)
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

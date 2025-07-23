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
    prosite_params <- reactiveValues(
        output_dir = getwd(),
        output_name = NULL,
        output_format = "gff"
    )
    prosite_analysis_run <- reactiveVal(FALSE)
    prosite_status_text <-
        reactiveVal("Analysis status: waiting for inputs")
    data_matrix <- reactiveVal(NULL)
    original_data <- reactiveVal(NULL)
    volumes <- getLogicalDrives()
    loading <- reactiveVal(FALSE)
    
    # --- UI Rendering ---
    output$logo <- renderImage({
        logo_path <- system.file("img/PMlogo.png", package = "PMScanR")
        if (!nzchar(logo_path)) {
            stop("Logo file not found in the package.")
        }
        list(src = logo_path, height = "100%")
    },
    deleteFile = FALSE)
    
    output$run_prosite_button <- renderUI({
        if (loading()) {
            tags$button(
                class = "btn btn-primary",
                type = "button",
                disabled = "disabled",
                tags$span(class = "spinner-border spinner-border-sm", `aria-hidden` = "true"),
                tags$span(role = "status", "Running...")
            )
        } else {
            actionButton("run_prosite",
                         "Run Analysis",
                         class = "btn btn-primary",
                         style = "background-color: #4e62c8; color: white; font-family: 'Inter', 
                         sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
        }
    })
    
    # --- Status Text Outputs ---
    output$prosite_analysis_status <- renderText({
        prosite_status_text()
    })
    
    # --- Observers ---
    
    # Directory selection logic
    observeEvent(input$output_dir_button, {
        shinyDirChoose(input,
                       'output_dir_button',
                       roots = volumes,
                       session = session)
    })
    
    observe({
        if (is.list(input$output_dir_button)) {
            tryCatch({
                selected_dir <- parseDirPath(volumes, input$output_dir_button)
                updateTextInput(session, "output_dir", value = selected_dir)
            }, error = function(e) {
                updateTextInput(session, "output_dir", value = "Error selecting directory")
            })
        }
    })
    
    # Main "Run Analysis" for Prosite
    observeEvent(input$run_prosite, {
        loading(TRUE)
        prosite_status_text("Analysis status: running...")
        Sys.sleep(0.1)
        
        in_file <- input$file_upload$datapath
        out_dir <- input$output_dir
        out_name <- input$output_name
        out_format <- input$output_format
        
        if (is.null(in_file)) {
            showNotification("Please upload an input file.", type = "warning")
            prosite_status_text("Analysis status: waiting for inputs")
            loading(FALSE)
            return()
        }
        
        if (!dir.exists(out_dir))
            dir.create(out_dir, recursive = TRUE)
        
        if (!file.access(out_dir, mode = 2) == 0) {
            showNotification("Output directory is not writable.", type = "error")
            prosite_status_text("Analysis status: error")
            loading(FALSE)
            return()
        }
        
        full_output_path <- file.path(out_dir, out_name)
        
        tryCatch({
            message(
                sprintf(
                    "Starting Prosite analysis. Output will be saved to: %s",
                    full_output_path
                )
            )
            runPsScan(
                in_file = in_file,
                out_file = full_output_path,
                out_format = out_format
            )
            
            if (out_format == "gff") {
                prosite_results_data(rtracklayer::import.gff(full_output_path))
            } else if (out_format == "psa") {
                prosite_results_data(readPsa(full_output_path))
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
            showNotification(
                sprintf("Error during analysis: %s", e$message),
                type = "error",
                duration = 10
            )
            prosite_status_text(sprintf("Analysis status: error - %s", e$message))
        }, finally = {
            loading(FALSE)
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
                data <- readPsa(input$uploaded_file$datapath)
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
                motifs <-
                    extractProteinMotifs(input$psa_file_seqlogo$datapath)
                updateSelectInput(session, "motif_id", choices = names(motifs))
            } else if (input$motif_data_source == "Use Prosite analysis PSA output") {
                if (prosite_analysis_run() &&
                    prosite_params$output_format == "psa") {
                    psa_file <-
                        file.path(prosite_params$output_dir,
                                  prosite_params$output_name)
                    if (!file.exists(psa_file)) {
                        showNotification("Prosite analysis PSA output file not found.",
                                         type = "warning")
                        updateSelectInput(session, "motif_id", choices = NULL)
                        return()
                    }
                    motifs <- extractProteinMotifs(psa_file)
                    updateSelectInput(session, "motif_id", choices = names(motifs))
                } else {
                    updateSelectInput(session, "motif_id", choices = NULL)
                    showNotification("Prosite analysis with PSA output is not available.",
                                     type = "warning")
                }
            }
        } else {
            updateSelectInput(session, "motif_id", choices = NULL)
        }
    })
    
    # Rendering for Heatmap 1
    output$heatmap1_output <- renderPlotly({
        req(data_matrix())
        matrix2hm(
            input = data_matrix(),
            x = input$highlight_x1,
            y = input$highlight_y1
        )
    })
    
    # Rendering for home heatmap
    output$home_heatmap_output <- renderPlotly({
        req(data_matrix())
        matrix2hm(input = data_matrix(),
                  x = NULL,
                  y = NULL)
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
                seq_data <-
                    seqinr::read.fasta(
                        file = input$fasta_file_seqlogo$datapath,
                        seqtype = if (input$seqtype == "Protein")
                            "AA"
                        else
                            "DNA"
                    )
                from <- input$from_pos
                to <- input$to_pos
                if (from > to) {
                    showNotification("From position must be less than or equal to To position.",
                                     type = "warning")
                    return()
                }
                seq_short <-
                    extractSegments(sequences = seq_data,
                                    from = from,
                                    to = to)
                if (length(seq_short) > 0) {
                    output$seqlogo_plot <- renderPlot({
                        ggseqlogo(unlist(seq_short),
                                  seq_type = if (input$seqtype == "Protein")
                                      "aa"
                                  else
                                      "dna")
                    })
                } else {
                    showNotification("No sequences found in the specified range.",
                                     type = "warning")
                }
            }, error = function(e) {
                showNotification(paste("Error generating SeqLogo:", e$message),
                                 type = "error")
            })
        } else if (input$seqlogo_type == "Motifs") {
            # Generate seqlogo from motifs
            motifs <- NULL
            if (input$motif_data_source == "Upload my own PSA file") {
                req(input$psa_file_seqlogo)
                motifs <-
                    extractProteinMotifs(input$psa_file_seqlogo$datapath)
            } else if (input$motif_data_source == "Use Prosite analysis PSA output") {
                if (prosite_analysis_run() &&
                    prosite_params$output_format == "psa") {
                    psa_file <-
                        file.path(prosite_params$output_dir,
                                  prosite_params$output_name)
                    if (!file.exists(psa_file)) {
                        showNotification("Prosite analysis PSA output file not found.",
                                         type = "warning")
                        return()
                    }
                    motifs <- extractProteinMotifs(psa_file)
                } else {
                    showNotification("Prosite analysis with PSA output is not available.",
                                     type = "warning")
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

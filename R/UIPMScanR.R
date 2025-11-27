#' Create a display card for the UI
#' @param header Card header text
#' @param body Card body content
#' @param colour Background color class for the header
#' @param fs Enable full-screen option
#' @param min_height Minimum height of the card body
#' @param fillable Whether the card body should be fillable
#' @return A Shiny card UI element
#' @noRd
displayCard <- function(header, body, colour = "bg-dark", fs = TRUE, min_height = "10em", fillable = TRUE) {
  bslib::card(
    full_screen = fs,
    bslib::card_header(header, class = colour),
    bslib::card_body(body, min_height = min_height, fillable = fillable)
  )
}

#' Create the main UI for the PMScanR Shiny App
#'
#' @importFrom bslib page_navbar navbar_options nav_panel page_fillable
#'   layout_columns page_sidebar sidebar navset_tab nav_spacer nav_menu nav_item
#'   card card_header card_body accordion accordion_panel
#' @importFrom shiny div imageOutput p textOutput fileInput textInput selectInput
#'   uiOutput tags actionButton conditionalPanel numericInput h3 strong
#'   plotOutput tableOutput icon
#' @importFrom shinyFiles shinyDirButton
#' @importFrom plotly plotlyOutput
#'
#' @return A Shiny UI object.
#' @noRd
buildUi <- function() {
  link_PMScanR <- shiny::tags$a(shiny::icon("github"), "PMScanR", href = "https://github.com/prodakt/PMScanR", target = "_blank")
  link_lncRna <- shiny::tags$a(shiny::icon("github"), "lncRna", href = "https://github.com/prodakt/lncRna", target = "_blank")
  link_PROSITE <- shiny::tags$a(shiny::icon("sitemap"), "PROSITE", href = "https://prosite.expasy.org/", target = "_blank")

  bslib::page_navbar(
    title = "PMScanR",
    navbar_options = bslib::navbar_options(bg = "#0062cc", underline = TRUE),

    # --- Home Panel ---
    bslib::nav_panel(title = "Home",
                     bslib::page_fillable(
                       shiny::div(style = " margin-bottom: 20px;",
                                  bslib::layout_columns(
                                    shiny::imageOutput("logo"),
                                    displayCard(
                                      "Description",
                                      shiny::p("Explore protein sequences for functional motifs using the Prosite Analysis tool...")
                                    )
                                  )),
                       shiny::div(style = " overflow-y: auto;",
                                  displayCard("Heatmap", plotly::plotlyOutput("home_heatmap_output", height = "100%"))
                       )
                     )),

    # --- Prosite Analysis Panel ---
    bslib::nav_panel(title = "Prosite analysis",
                     bslib::page_sidebar(
                       sidebar = bslib::sidebar(
                         width = "25em",
                         title = "Input load",
                         shiny::tags$div(
                           style = "background-color: #f0f0f0; border-radius: 5px; padding: 10px; margin-bottom: 10px; text-align: center;",
                           shiny::textOutput("prosite_analysis_status", inline = TRUE)
                         ),
                         shiny::fileInput("file_upload", "Upload FASTA file", buttonLabel = "Browse", placeholder = "No file selected", accept = c(".fasta", ".fa", "txt")),
                         shiny::textInput("output_dir", "Output Directory", value = getwd(), placeholder = "Select output directory"),
                         shinyFiles::shinyDirButton("output_dir_button", "Browse", title = "Select an output directory"),
                         shiny::textInput("output_name", "Output Filename", value = "prosite_results.txt", placeholder = "Enter file name"),
                         shiny::selectInput("output_format", "Select Output Format", choices = c("gff", "psa", "scan"), selected = "gff"),

                         bslib::accordion(
                           open = FALSE,
                           bslib::accordion_panel(
                             "Advanced Configuration",
                             shiny::fileInput("custom_ps_scan", "Custom ps_scan.pl script", accept = ".pl"),
                             shiny::fileInput("custom_prosite_dat", "Custom prosite.dat database", accept = c(".dat", ".txt")),
                             shiny::fileInput("custom_pfscan", "Custom pfscan executable", accept = NULL)
                           )
                         ),

                         shiny::uiOutput("run_prosite_button")
                       ),
                       bslib::page_fillable(
                         displayCard("Help", shiny::tags$ul(
                           shiny::tags$li(shiny::strong("Upload FASTA file:"), "Browse and select your input FASTA file."),
                           shiny::tags$li(shiny::strong("Output Directory:"), "Choose the directory where the results will be saved."),
                           shiny::tags$li(shiny::strong("Advanced:"), "You can upload your own PROSITE database or executables. If left empty, they will be downloaded automatically.")
                         )),
                         shiny::div(style = "overflow-y: auto;", displayCard("Results", shiny::tableOutput("prosite_results_output")))
                       )
                     )),

    # --- Data Analysis Panel ---
    bslib::nav_panel(title = "Data analysis",
                     bslib::page_sidebar(
                       sidebar = bslib::sidebar(
                         width = "25em",
                         title = "Input analysis data",
                         shiny::conditionalPanel(
                           condition = "input.data_analysis_tabs != 'SeqLogo'",
                           shiny::selectInput("data_source", "Select Data Source", choices = c("Use Prosite analysis data", "Upload my own file"), width = "100%"),
                           shiny::conditionalPanel(condition = "input.data_source == 'Upload my own file'",
                                                   shiny::tags$div(
                                                     shiny::fileInput("uploaded_file", "Upload File", accept = c(".gff", ".txt", ".psa")),
                                                     shiny::selectInput("input_format", "Input Format", choices = c("gff", "psa", "scan"))
                                                   )),
                           shiny::actionButton("analyse_data", "Analyse Data", style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
                         ),
                         shiny::conditionalPanel(
                           condition = "input.data_analysis_tabs == 'SeqLogo'",
                           shiny::selectInput("seqlogo_type", "Select SeqLogo Type", choices = c("Raw Sequences", "Motifs")),
                           shiny::conditionalPanel(
                             condition = "input.seqlogo_type == 'Raw Sequences'",
                             shiny::fileInput("fasta_file_seqlogo", "Upload FASTA File for SeqLogo", accept = c(".fasta", ".fa", ".txt")),
                             shiny::selectInput("seqtype", "Choose nature of the sequence", choices = c("Protein", "Nucleotide"), selected = "Protein"),
                             shiny::numericInput("from_pos", "From Position", value = 1, min = 1),
                             shiny::numericInput("to_pos", "To Position", value = 100, min = 1)
                           ),
                           shiny::conditionalPanel(
                             condition = "input.seqlogo_type == 'Motifs'",
                             shiny::selectInput("motif_data_source", "Select Motif Data Source", choices = c("Use Prosite analysis output", "Upload my own result file")),
                             shiny::conditionalPanel(
                               condition = "input.motif_data_source == 'Upload my own result file'",
                               shiny::fileInput("psa_file_seqlogo", "Upload Result File (GFF/PSA/TXT)", accept = c(".txt", ".psa", ".gff"))
                             ),
                             shiny::selectInput("motif_id", "Select Motif ID", choices = NULL)
                           ),
                           shiny::actionButton("generate_seqlogo", "Generate SeqLogo", style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
                         )
                       ),
                       bslib::navset_tab(
                         id = "data_analysis_tabs",
                         bslib::nav_panel(title = "Help", shiny::h3("Data Overview"), shiny::p("Here you will find help information...")),
                         bslib::nav_panel(title = "Heatmap", shiny::h3("Heatmap"), bslib::page_fillable(
                           bslib::layout_columns(
                             displayCard("Highlight Columns", shiny::selectInput("highlight_x1", "", choices = NULL, multiple = TRUE)),
                             displayCard("Highlight Rows", shiny::selectInput("highlight_y1", "", choices = NULL, multiple = TRUE))
                           ),
                           displayCard("Heatmap", plotly::plotlyOutput("heatmap1_output", height = "100%"))
                         )),
                         bslib::nav_panel(title = "Pie Chart", bslib::page_fillable(displayCard("Pie chart plot", shiny::plotOutput("piechart_output", height = "100%")))),
                         bslib::nav_panel(title = "SeqLogo", bslib::page_fillable(displayCard(header = "SeqLogo Plot", body = shiny::plotOutput("seqlogo_plot", height = "50em"))))
                       )
                     )),
    bslib::nav_spacer(),
    bslib::nav_menu(title = "Links", align = "right", bslib::nav_item(link_PMScanR), bslib::nav_item(link_lncRna), bslib::nav_item(link_PROSITE))
  )
}

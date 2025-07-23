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
  card(
    full_screen = fs,
    card_header(header, class = colour),
    card_body(body, min_height = min_height, fillable = fillable)
  )
}

#' Create the main UI for the PMScanR Shiny App
#'
#' This function builds the complete user interface for the application using
#' functions from the bslib and shiny packages.
#'
#' @importFrom shiny div imageOutput p textOutput fileInput textInput selectInput
#' @importFrom shiny uiOutput tags actionButton conditionalPanel numericInput h3
#' @importFrom shiny strong plotOutput tableOutput icon
#' @importFrom bslib page_navbar navbar_options nav_panel page_fillable
#' @importFrom bslib layout_columns page_sidebar sidebar navset_tab nav_spacer
#' @importFrom bslib nav_menu nav_item card card_header card_body
#' @importFrom shinyFiles shinyDirButton
#' @importFrom plotly plotlyOutput
#'
#' @return A Shiny UI object.
#' @noRd
buildUi <- function() {
  link_PMScanR <- tags$a(icon("github"), "PMScanR", href = "https://github.com/prodakt/PMScanR", target = "_blank")
  link_lncRna <- tags$a(icon("github"), "lncRna", href = "https://github.com/prodakt/lncRna", target = "_blank")
  link_PROSITE <- tags$a(icon("sitemap"), "PROSITE", href = "https://prosite.expasy.org/", target = "_blank")
  detected_os_val <- detectOs()

  page_navbar(
    title = "PMScanR",
    navbar_options = navbar_options(
      bg = "#0062cc",
      underline = TRUE
    ),
    nav_panel(
      title = "Home",
      page_fillable(
        div(
          style = " margin-bottom: 20px;",
          layout_columns(
            imageOutput("logo"),
            displayCard("Description", p(
              "Explore protein sequences for functional motifs using the Prosite Analysis tool.
               This feature allows you to input a protein sequence by uploading a FASTA file.
               Once submitted, the app will scan the sequence using the Prosite database to identify known motifs and patterns.
               The results are displayed in a detailed table, which can be used for further analysis in
               the Data Analysis section."
            ))
          )
        ),
        div(
          style = " overflow-y: auto;",
          displayCard("Heatmap", plotlyOutput("home_heatmap_output", height = "100%"))
        )
      )
    ),
    nav_panel(title = "Prosite analysis",
              page_sidebar(
                sidebar = sidebar(
                  width = "25em",
                  title = "Input settings",
                  tags$div(
                    style = "background-color: #f0f0f0; border-radius: 5px; padding: 10px; margin-bottom: 10px; text-align: center;",
                    textOutput("prosite_analysis_status", inline = TRUE)
                  ),
                  fileInput("file_upload", "Upload FASTA file", buttonLabel = "Browse", placeholder = "No file selected", accept = c(".fasta", ".fa", "txt")),
                  textInput("output_dir", "Output Directory", value = getwd(), placeholder = "Select output directory"),
                  shinyDirButton("output_dir_button", "Browse", title = "Select an output directory"),
                  textInput("output_name", "Output Filename", value = "prosite_results.txt", placeholder = "Enter file name"),
                  selectInput("output_format", "Select Output Format", choices = c("gff", "psa"), selected = "gff"),
                  uiOutput("run_prosite_button")
                ),
                page_fillable(
                  displayCard("Help", tags$ul(
                    tags$li(strong("Upload FASTA file:"), "Browse and select your input FASTA file."),
                    tags$li(strong("Output Directory:"), "Choose the directory where the results will be saved."),
                    tags$li(strong("Output Filename:"), "Specify the name for your output file."),
                    tags$li(strong("Note:"), "The required PROSITE executables and databases will be automatically downloaded and cached in a standardized location on the first run. Subsequent analyses will use these cached files.")
                  )),
                  div(
                    style = "overflow-y: auto;",
                    displayCard("Results", tableOutput("prosite_results_output"))
                  )
                )
              )
    ),
  nav_panel(
    title = "Data analysis",
    page_sidebar(
      sidebar = sidebar(
        width = "25em",
        title = "Input analysis data",
        selectInput("data_source", "Select Data Source", choices = c("Use Prosite analysis data", "Upload my own file"), width = "100%"),
        conditionalPanel(
          condition = "input.data_source == 'Upload my own file'",
          tags$div(
            fileInput("uploaded_file", "Upload File", accept = c(".gff", ".txt")),
            selectInput("input_format", "Input Format", choices = c("gff", "psa")))),
        actionButton("analyse_data", "Analyse Data", style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;"),
        conditionalPanel(
          condition = "input.data_analysis_tabs == 'SeqLogo'",
          selectInput("seqlogo_type", "Select SeqLogo Type", choices = c("Raw Sequences", "Motifs")),
          conditionalPanel(
            condition = "input.seqlogo_type == 'Raw Sequences'",
            fileInput("fasta_file_seqlogo", "Upload FASTA File for SeqLogo", accept = c(".fasta", ".fa", ".txt")),
            selectInput("seqtype", "Choose nature of the sequence", choices = c("Protein", "Nucleotide" ), selected = "Protein"),
            numericInput("from_pos", "From Position", value = 1, min = 1),
            numericInput("to_pos", "To Position", value = 100, min = 1)
          ),
          conditionalPanel(
            condition = "input.seqlogo_type == 'Motifs'",
            selectInput("motif_data_source", "Select Motif Data Source", choices = c("Use Prosite analysis PSA output", "Upload my own PSA file")),
            #selectInput("seqtype", "Choose nature of the sequence", choices = c("Protein", "Nucleotide"), selected = "Protein"),
            conditionalPanel(
              condition = "input.motif_data_source == 'Upload my own PSA file'",
              fileInput("psa_file_seqlogo", "Upload PSA File for SeqLogo", accept = c(".txt", ".psa"))
            ),
            selectInput("motif_id", "Select Motif ID", choices = NULL) # Choices will be updated dynamically
          ),
          actionButton("generate_seqlogo", "Generate SeqLogo", style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
        )

      ),
      navset_tab(
        id = "data_analysis_tabs",
        nav_panel(
          title = "Help",
          h3("Data Overview"),
          p("Here you will find help information to understand the inputs in the sidebar."),
          tags$ul(
            tags$li(strong("Select Data Source:"), "Choose whether to use the data from Prosite analysis or upload your own file."),
            tags$li(strong("Upload File:"), "If uploading, select a GFF or TXT file."),
            tags$li(strong("Input Format:"), "Specify the format of the uploaded file: 'gff' or 'psa'."),
            tags$li(strong("Analyse Data:"), "Click this button to process the data and generate visualizations."))
        ),
        nav_panel(
          title = "Heatmap",
          h3("Heatmap"),
          p("The graphical pressentation of motifs occurence accross submitted sequences"),
          page_fillable(
            layout_columns(
              displayCard("Highlight Columns", selectInput("highlight_x1", "", choices = NULL, multiple = TRUE)),
              displayCard("Highlight Rows", selectInput("highlight_y1", "",choices = NULL, multiple = TRUE))
            ),
            displayCard("Heatmap", plotlyOutput("heatmap1_output", height = "100%"))
          )


        ),
        # nav_panel(
        #   title = "Heatmap 2",
        #   h3("Heatmap ......"),
        #   p("Presented Hea"),
        #   sidebar = sidebar(
        #     title = "test sidebar",
        #     sliderInput("to_range", "To Position", min = 1, max = 1000, value = 100, step = 1)
        #   ),
        #   page_fillable(
        #     layout_columns(
        #       displayCard("Highlight Columns", selectInput("highlight_x2", "", choices = NULL, multiple = TRUE)),
        #       displayCard("Highlight Rows", selectInput("highlight_y2", "",choices = NULL, multiple = TRUE))
        #     ),
        #     displayCard("Heatmap", plotlyOutput("heatmap2_output", height = "100%"))
        #   )
        # ),
        nav_panel(
          title = "Pie Chart",
          page_fillable(
            displayCard("Pie chart plot", plotOutput("piechart_output", height = "100%"))
          )
        ),
        nav_panel(id = "SeqLogo",
                  title = "SeqLogo",
                  page_fillable(
                    displayCard(
                      header = "SeqLogo Plot",
                      body = plotOutput("seqlogo_plot", height = "50em")
                    )
                  )
        )
      )
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_PMScanR),
    nav_item(link_lncRna),
    nav_item(link_PROSITE)
    )
  )
}

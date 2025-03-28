#' Create a display card for the UI
#' @param header Card header text
#' @param body Card body content
#' @param colour Background color class for the header
#' @param fs Enable full-screen option
#' @param min_height Minimum height of the card body
#' @param fillable Whether the card body should be fillable
#' @return A Shiny card UI element
#' @noRd
display_card <- function(header, body, colour = "bg-dark", fs = T, min_height = "10em", fillable = T) {
  card(
    full_screen = fs,
    card_header(header, class = colour),
    card_body(body, min_height = min_height, fillable = fillable)
  )
}

#' Detect the operating system
#' @return A string indicating the OS ("WIN", "LINUX", "MAC")
#' @noRd
detect_os <- function() {
  os <- .Platform$OS.type
  if (os == "windows") {
    return("WIN")
  } else if (os == "unix") {
    if (Sys.info()["sysname"] == "Darwin") {
      return("MAC")
    } else {
      return("LINUX")
    }
  } else {
    return("UNKNOWN")
  }
}

link_PMScanR <- tags$a(shiny::icon("github"), "PMScanR", href = "https://github.com/prodakt/PMScanR", target = "_blank")
link_posit <- tags$a(shiny::icon("github"), "lncRna", href = "https://github.com/prodakt/lncRna", target = "_blank")
detected_os <- detect_os()

#' Create the main UI for PMScanR
#'
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import plotly
#' @return A Shiny UI object
#' @noRd
build_ui <- page_navbar(
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
          display_card("", imageOutput("logo"), fs = F),
          display_card("Description", p(
            "Explore protein sequences for functional motifs using the Prosite Analysis tool.
          This feature allows you to input a protein sequence either as a raw string or by uploading a FASTA file.
          Once submitted, the app will scan the sequence using the Prosite database to identify known motifs and patterns,
          such as phosphorylation sites, glycosylation sites, and more. The results are displayed in a detailed table,
          showing each motif's location, type, and additional attributes. You can then use these results for further analysis in
          the Data Analysis section, where they can be visualized as heatmaps or pie charts to better understand motif distribution
          across your sequence.
          Visualize and analyze protein motif data with the Data Analysis tool. Here, you can either upload your own motif data
          in GFF or PSA format or use the results generated from the Prosite Analysis. The app processes your data into a binary matrix,
          where rows represent motifs (e.g., specific patterns or features) and columns represent sequences or proteins.
          Two interactive heatmaps allow you to explore motif presence across sequences: one with a standard layout and another
          with a square-shaped layout for better comparison. Additionally, a pie chart visualizes the frequency distribution of different
          motif types. Use the highlighting feature in the heatmaps to focus on specific motifs or sequences, making it easier to identify
          patterns and trends in your data."
          ))
        )
      ),
      div(
        style = " overflow-y: auto;",
        display_card("Heatmap", plotlyOutput("home_heatmap_output", height = "100%"))
      )
    )
  ),
  nav_panel(title = "Prosite analysis",
            page_sidebar(
              sidebar = sidebar(
                width = "25em",
                title = "Input load",
                tags$div(
                  style = "background-color: #f0f0f0; border-radius: 5px; padding: 10px; margin-bottom: 10px; text-align: center;",
                  textOutput("prosite_analysis_status", inline = TRUE) # Display status message
                ),
                fileInput("file_upload", "Data to analyse", buttonLabel = "Browse", placeholder = "No file selected", accept = c(".fasta", ".fa", "txt")),
                textInput("output_dir", "Output Directory", value = getwd(), placeholder = "Select output directory"),
                shinyDirButton("output_dir_button", "Browse", title = "Select an output directory"),
                textInput("output_name", "Output Name", value = "prosite_results.txt", placeholder = "Enter file name"),
                selectInput("output_format", "Select Format", choices = c("gff", "psa"), selected = "gff"),
                fileInput("ps_scan_file", "ps-scan file", buttonLabel = "Browse", placeholder = "(Empty will be downloaded)", accept = ".pl"),
                fileInput("patterns_dat_file", "Prosite dat file", buttonLabel = "Browse", placeholder = "(Empty will be downloaded)", accept = ".dat"),
                fileInput("pf_scan_file", "pf scan executable", buttonLabel = "Browse", placeholder = "(Empty will be downloaded)"),
                selectInput("os_choice", "Select OS", choices = c("WIN", "LINUX", "MAC"), selected = detected_os),
                #actionButton("run_prosite", "Run Analysis", style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;"),
                uiOutput("run_prosite_button")

              ),
              page_fillable(
                display_card("Help",tags$ul(tags$li(strong("Load File:"), "Browse and select your input FASTA file."),
                                            tags$li(strong("Output Directory:"), "Choose the directory where the results will be saved. Defaults to the current working directory."),
                                            tags$li(strong("Output Name:"), "Specify the name for your output file."),
                                            tags$li(strong("Output Format:"), "Select the desired output format for the Prosite analysis (gff or psa)."),
                                            tags$li(strong("PS-Scan Script Path:"), "Optionally provide the path to the PS-Scan Perl script. If left empty, the package will attempt to use a default or download it."),
                                            tags$li(strong("PROSITE Patterns Database Path:"), "Optionally provide the path to the PROSITE patterns database file (prosite.dat). If left empty, the package will attempt to use a default or download it."),
                                            tags$li(strong("PFScan Executable Path:"), "Optionally provide the path to the PFScan executable. If left empty, the package will attempt to use a default or download and extract it based on your OS."),
                                            tags$li(strong("Operating System:"), "Select your operating system. This helps in choosing the correct PFScan executable if the path is not provided.")
                ), fs = T),
                div(
                  style = "overflow-y: auto;",
                  display_card("Results", tableOutput("prosite_results_output"))
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
          title = "Heatmap 1",
          h3("Heatmap ......"),
          p("Presented Hea"),
          page_fillable(
            layout_columns(
              display_card("Highlight Columns", selectInput("highlight_x1", "", choices = NULL, multiple = TRUE)),
              display_card("Highlight Rows", selectInput("highlight_y1", "",choices = NULL, multiple = TRUE))
            ),
            display_card("Heatmap", plotlyOutput("heatmap1_output", height = "100%"))
          )


        ),
        nav_panel(
          title = "Heatmap 2",
          h3("Heatmap ......"),
          p("Presented Hea"),
          sidebar = sidebar(
            title = "test sidebar",
            sliderInput("to_range", "To Position", min = 1, max = 1000, value = 100, step = 1)
          ),
          page_fillable(
            layout_columns(
              display_card("Highlight Columns", selectInput("highlight_x2", "", choices = NULL, multiple = TRUE)),
              display_card("Highlight Rows", selectInput("highlight_y2", "",choices = NULL, multiple = TRUE))
            ),
            display_card("Heatmap", plotlyOutput("heatmap2_output", height = "100%"))
          )
        ),
        nav_panel(
          title = "Pie Chart",
          page_fillable(
            display_card("Pie chart plot", plotOutput("piechart_output", height = "100%"))
          )
        ),
        nav_panel(id = "SeqLogo",
                  title = "SeqLogo",
                  page_fillable(
                    display_card(
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
    nav_item(link_posit)
  )
)

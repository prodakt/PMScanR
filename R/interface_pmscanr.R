#' PMScanR Shiny UI
#' @param id Module namespace ID
#' @return A Shiny UI object
#' @export
pmscanr_ui <- function(id) {
  ns <- shiny::NS(id)  # Create a namespace for this module to avoid ID conflicts
  shiny::fluidPage(    # Start a fluid page layout that adjusts to browser size
    shiny::titlePanel( # Add a title panel at the top of the page
      shiny::div(      # Use a div to combine logo and text horizontally
        shiny::img(src = system.file("images/PMlogo.png", package = "PMScanR"), # Load logo from package's inst/images/
                   height = 50, style = "vertical-align: middle; margin-right: 10px;"), # Set logo height and styling
        "PMScanR"      # Add the app title text next to the logo
      )
    ),
    shiny::sidebarLayout( # Create a two-column layout: sidebar on left, main panel on right
      shiny::sidebarPanel( # Define the sidebar for input controls
        shiny::fileInput(ns("file"), "Load file", accept = c(".fasta", ".fa", ".gff", ".psa", ".txt")), # File upload input with all formats
        shiny::textInput(ns("output_name"), "Output name", "output.txt"),       # Text input for output filename, default "output.txt"
        shiny::selectInput(ns("output_format"), "Output format",               # Dropdown menu for selecting output format
                           choices = c("FASTA", "TXT", "GFF", "PSA")),         # Options: FASTA, TXT, GFF, PSA
        shiny::numericInput(ns("min_length"), "Min sequence length", value = 5, min = 1), # Numeric input for min sequence length, default 5
        shiny::numericInput(ns("max_length"), "Max sequence length", value = 1000, min = 1), # Numeric input for max sequence length, default 1000
        shiny::actionButton(ns("analyze"), "Analyze"),                         # Button to trigger sequence analysis
        shiny::downloadButton(ns("download_results"), "Download Results")      # Button to download filtered sequences
      ),
      shiny::mainPanel( # Define the main panel for displaying outputs
        shiny::tabsetPanel( # Create a tabbed interface for multiple views
          shiny::tabPanel("Sequences", DT::DTOutput(ns("sequences_table"))),   # Tab 1: Show filtered sequences in a table
          shiny::tabPanel("Visualizations",                                    # Tab 2: Show plots
                          shiny::plotOutput(ns("aa_histogram")),               # Plot for amino acid frequency
                          shiny::plotOutput(ns("length_plot"))),              # Plot for sequence length distribution
          shiny::tabPanel("Summary", shiny::verbatimTextOutput(ns("summary"))) # Tab 3: Show text summary
        )
      )
    )
  )
}

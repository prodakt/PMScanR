r

Collapse

Wrap

Copy
# UI Function
pmscanr_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(
      div(
        img(src = system.file("images/PMlogo.png", package = "PMScanR"), 
            height = 50, style = "vertical-align: middle; margin-right: 10px;"),
        "PMScanR"
      )
    ),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Load file", accept = c(".fasta", ".fa")),
        textInput(ns("output_name"), "Output name", "output.txt"),
        selectInput(ns("output_format"), "Output format", 
                    choices = c("FASTA", "TXT")),
        numericInput(ns("min_length"), "Min sequence length", value = 5, min = 1),
        numericInput(ns("max_length"), "Max sequence length", value = 1000, min = 1),
        actionButton(ns("analyze"), "Analyze"),
        downloadButton(ns("download_results"), "Download Results")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Sequences", DT::DTOutput(ns("sequences_table")))
        )
      )
    )
  )
}
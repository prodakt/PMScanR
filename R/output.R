#' output.R
#' @import shiny
#' @importFrom shiny renderUI fluidRow column tags tagList fluidPage actionButton h1 p sidebarLayout sidebarPanel mainPanel fileInput textInput selectInput tableOutput tabPanel tabsetPanel
#' @importFrom bslib page_sidebar sidebar card card_header
#' @export
render_page_content <- function(current_page) {
  renderUI({
    if (current_page() == "main") {fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      h1("Welcome to Protein Motif Analysis App", style = "color: #5b5a5a; font-family: 'Inter', sans-serif; text-align: center; border-bottom: 2px solid #4e62c8; padding-bottom: 10px; margin-bottom: 15px;"),
      fluidRow(
        style = "background-color: #BBB1CE; min-height: 100vh; padding: 20px;",
        # Left section: Prosite analysis (unchanged)
        column(6,
               tags$div(
                 style = "
                  background: linear-gradient(220deg, rgba(13, 229, 60, 0.31) 0%, rgba(55, 9, 148, 0.31) 100%);
                  border-radius: 64px;
                  border: 1px solid rgba(255, 255, 255, 0.1);
                  padding: 20px;
                  height: 100vh;
                  display: flex;
                  flex-direction: column;
                  align-items: center;
                  overflow: hidden;
                  cursor: pointer;
                ",
                 onclick = "Shiny.setInputValue('goto_prosite', Math.random())",
                 h3(
                   "Prosite analysis",
                   style = "
                     font-family: 'Inter', sans-serif;
                     font-weight: 400;
                     color: #4e62c8;
                     font-size: 3.5rem;
                     text-align: center;
                     margin-bottom: 2rem;
                   "
                 ),
                 tags$div(
                   style = "
                     background-color: rgba(255, 255, 255, 0.74);
                     box-shadow: 0 4px 4px rgba(0, 0, 0, 0.25);
                     padding: 1rem;
                     width: 80%;
                     height: 50%;
                     overflow-y: auto;
                   ",
                   p(
                     "Explore protein sequences for functional motifs using the Prosite Analysis tool.
                     This feature allows you to input a protein sequence either as a raw string or by uploading a FASTA file.
                     Once submitted, the app will scan the sequence using the Prosite database to identify known motifs and patterns,
                     such as phosphorylation sites, glycosylation sites, and more. The results are displayed in a detailed table,
                     showing each motif's location, type, and additional attributes. You can then use these results for further analysis in
                     the Data Analysis section, where they can be visualized as heatmaps or pie charts to better understand motif distribution
                     across your sequence.",
                     style = "
                       font-family: 'Inter', sans-serif;
                       font-weight: 400;
                       color: #000000;
                       font-size: 2rem;
                       line-height: 1.5;
                     "
                   )
                 )
               )
        ),
        # Right section: Data analysis with Figma styling (unchanged)
        column(6,
               tags$div(
                 style = "
                  background: linear-gradient(135deg, rgba(39, 229, 13, 0.31) 0%, rgba(201, 201, 7, 0.31) 84%);
                  border-radius: 64px;
                  border: 1px solid rgba(255, 255, 255, 0.1);
                  padding: 37px 31px;
                  height: 100vh;
                  display: flex;
                  flex-direction: column;
                  align-items: center;
                  overflow: hidden;
                  cursor: pointer;
                ",
                 onclick = "Shiny.setInputValue('goto_data', Math.random())",
                 h1(
                   "Data analysis",
                   class = "title",
                   style = "
                     font-family: 'Inter', sans-serif;
                     font-weight: 400;
                     color: #4e62c8;
                     font-size: 3.5rem;
                     text-align: center;
                     margin-bottom: 2rem;
                   "
                 ),
                 tags$div(
                   class = "content-wrapper",
                   style = "
                     background-color: rgba(255, 255, 255, 0.74);
                     box-shadow: 0 4px 4px rgba(0, 0, 0, 0.25);
                     padding: 1rem;
                     width: 80%;
                     height: 50%;
                     overflow-y: auto;
                   ",
                   p(
                     "Visualize and analyze protein motif data with the Data Analysis tool. Here, you can either upload your own motif data
                     in GFF or PSA format or use the results generated from the Prosite Analysis. The app processes your data into a binary matrix,
                     where rows represent motifs (e.g., specific patterns or features) and columns represent sequences or proteins.
                     Two interactive heatmaps allow you to explore motif presence across sequences: one with a standard layout and another
                     with a square-shaped layout for better comparison. Additionally, a pie chart visualizes the frequency distribution of different
                     motif types. Use the highlighting feature in the heatmaps to focus on specific motifs or sequences, making it easier to identify
                     patterns and trends in your data.",
                     class = "content",
                     style = "
                       font-family: 'Inter', sans-serif;
                       font-weight: 400;
                       color: #333;
                       font-size: 2rem;
                       line-height: 1.5;
                     "
                   )
                 )
               )
        )
      ))
    } else if (current_page() == "prosite") {
      detected_os <- detect_os()
      page_sidebar(
        title = tags$div(
          h1("Prosite Analysis", style = "color: #5b5a5a; font-family: 'Inter', sans-serif; text-align: center; border-bottom: 2px solid #4e62c8; padding-bottom: 10px; margin-bottom: 15px;"),
        ),
        sidebar = sidebar(
          title = tags$div(
            h3("Prosite Analysis Tools", style = "color: #4e62c8; font-family: 'Inter', sans-serif; margin-top: 0;")
          ),
          style = "background-color: #DAF7A6; padding: 15px;",
          # Add the status text here, above the Run Analysis button
          tags$div(
            style = "background-color: #f0f0f0; border-radius: 5px; padding: 10px; margin-bottom: 10px; text-align: center;",
            textOutput("prosite_analysis_status", inline = TRUE) # Display status message
          ),
          tags$div(
            style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
            h4("Load File", style = "color: black; font-family: 'Inter', sans-serif; margin-top: 0;"),
            fileInput("file_upload", "Browse", buttonLabel = "Browse", placeholder = "No file selected", accept = c(".fasta", ".fa", "txt"))
          ),
          tags$div(
            style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
            h4("Output Directory", style = "color: black; font-family: 'Inter', sans-serif; margin-top: 0;"),
            textInput("output_dir", "Output Directory", value = getwd(), placeholder = "Select output directory"),
            shinyDirButton("output_dir_button", "Browse", title = "Select an output directory")
          ),
          tags$div(
            style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
            h4("Output Name", style = "color: black; font-family: 'Inter', sans-serif; margin-top: 0;"),
            textInput("output_name", "Output Name", value = "prosite_results.txt", placeholder = "Enter file name")
          ),
          tags$div(
            style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
            h4("Output Format", style = "color: black; font-family: 'Inter', sans-serif; margin-top: 0;"),
            selectInput("output_format", "Select Format", choices = c("gff", "psa"), selected = "gff")
          ),
          tags$div(
            style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
            h4("PS-Scan Script", style = "color: black; font-family: 'Inter', sans-serif; margin-top: 0;"),
            fileInput("ps_scan_file", "Browse", buttonLabel = "Browse", placeholder = "(Leave empty to use default)", accept = ".pl")
          ),
          tags$div(
            style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
            h4("PROSITE Patterns Database", style = "color: black; font-family: 'Inter', sans-serif; margin-top: 0;"),
            fileInput("patterns_dat_file", "Browse", buttonLabel = "Browse", placeholder = "(Leave empty to use default)", accept = ".dat")
          ),
          tags$div(
            style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
            h4("PFScan Executable", style = "color: black; font-family: 'Inter', sans-serif; margin-top: 0;"),
            fileInput("pf_scan_file", "Browse", buttonLabel = "Browse", placeholder = "(Leave empty to use default)")
          ),
          tags$div(
            style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 40px;",
            h4("Operating System", style = "color: black; font-family: 'Inter', sans-serif; margin-top: 0;"),
            selectInput("os_choice", "Select OS", choices = c("WIN", "LINUX", "MAC"), selected = detected_os)
          ),
          tags$div(
            style = "padding-top: 15px;",
            actionButton("run_prosite", "Run Analysis", style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
          )
        ),
        card(
          tags$style(HTML("
            .nav-tabs > li > a {
              font-size: 1.5em;
              font-weight: bold;
            }
          ")),
          tabsetPanel(
            id = "results_tabs",
            tabPanel("Home", value = "home"),
            tabPanel("Help", value = "help",
                     p("Here you will find help information to understand the inputs in the sidebar."),
                     tags$ul(style = "background-color: #fff3f3; border-radius: 5px; padding: 10px; margin-bottom: 10px;font-size: 1.6rem;
                       line-height: 1.5;",
                             tags$li(strong("Load File:"), "Browse and select your input FASTA file."),
                             tags$li(strong("Output Directory:"), "Choose the directory where the results will be saved. Defaults to the current working directory."),
                             tags$li(strong("Output Name:"), "Specify the name for your output file."),
                             tags$li(strong("Output Format:"), "Select the desired output format for the Prosite analysis (gff or psa)."),
                             tags$li(strong("PS-Scan Script Path:"), "Optionally provide the path to the PS-Scan Perl script. If left empty, the package will attempt to use a default or download it."),
                             tags$li(strong("PROSITE Patterns Database Path:"), "Optionally provide the path to the PROSITE patterns database file (prosite.dat). If left empty, the package will attempt to use a default or download it."),
                             tags$li(strong("PFScan Executable Path:"), "Optionally provide the path to the PFScan executable. If left empty, the package will attempt to use a default or download and extract it based on your OS."),
                             tags$li(strong("Operating System:"), "Select your operating system. This helps in choosing the correct PFScan executable if the path is not provided.")
                     )
            ),
            tabPanel("Results", value = "results",
                     tags$div(class = "table-responsive",
                              tableOutput("prosite_results_output"))
            ),
            
            selected = "help" # Set the 'Help' tab as the default
          )
        ),
        bg = "#BBB1CE" # Apply the background color
      )
    } else if (current_page() == "data") {
      page_sidebar(
        title = tags$div(
          h1("Data Analysis", style = "color: #5b5a5a; font-family: 'Inter', sans-serif; text-align: center; border-bottom: 2px solid #4e62c8; padding-bottom: 10px; margin-bottom: 15px;")
        ),
        sidebar = sidebar(
          title = tags$div(
            h3("Data Analysis Tools", style = "color: #4e62c8; font-family: 'Inter', sans-serif; margin-top: 0;")
          ),
          style = "background-color: #DAF7A6; padding: 15px;",
          tags$div(
            style = "background-color: #fff3f3; border-radius: 10px; padding: 10px; margin-bottom: 30px;",
            selectInput("data_source", "Select Data Source", choices = c("Use Prosite analysis data", "Upload my own file"), width = "100%")
          ),
          conditionalPanel(
            condition = "input.data_source == 'Upload my own file'",
            tags$div(
              style = "background-color: #fff3f3; border-radius: 10px; padding: 10px; margin-bottom: 30px;",
              fileInput("uploaded_file", "Upload File", accept = c(".gff", ".txt")),
              selectInput("input_format", "Input Format", choices = c("gff", "psa")))
          ),
          actionButton("analyse_data", "Analyse Data", style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
        ),
        mainPanel(
          style = "height: 100vh; overflow-x: scroll; overflow-y: auto; background-color: #BBB1CE;",
          tabsetPanel(
            id = "data_tabs",
            tabPanel("Home", value = "home"),
            tabPanel("Help", value = "help",
                     p("Here you will find help information to understand the inputs in the sidebar."),
                     tags$ul(
                       tags$li(strong("Select Data Source:"), "Choose whether to use the data from Prosite analysis or upload your own file."),
                       tags$li(strong("Upload File:"), "If uploading, select a GFF or TXT file."),
                       tags$li(strong("Input Format:"), "Specify the format of the uploaded file: 'gff' or 'psa'."),
                       tags$li(strong("Analyse Data:"), "Click this button to process the data and generate visualizations.")
                     )
            ),
            tabPanel("Heatmap 1", value = "heatmap1",
                     fluidRow(
                       column(4,
                              selectInput("highlight_x1", "Highlight Columns", choices = NULL, multiple = TRUE)
                       ),
                       column(4,
                              selectInput("highlight_y1", "Highlight Rows", choices = NULL, multiple = TRUE)
                       )
                     ),
                     plotlyOutput("heatmap1_output", height = "100%")
            ),
            tabPanel("Heatmap 2", value = "heatmap2",
                     fluidRow(
                       column(4,
                              selectInput("highlight_x2", "Highlight Columns", choices = NULL, multiple = TRUE)
                       ),
                       column(4,
                              selectInput("highlight_y2", "Highlight Rows", choices = NULL, multiple = TRUE)
                       )
                     ),
                     plotlyOutput("heatmap2_output", height = "100%")
            ),
            tabPanel("Pie Chart", value = "piechart",
                     plotOutput("piechart_output", height = "100%")
                     
            ),
            tabPanel("Sequence Extraction", value = "sequence_extraction",
                     fluidRow(
                       column(6,
                              fileInput("sequence_file", "Upload FASTA File", accept = c(".fasta"))
                       ),
                       column(3,
                              sliderInput("from_range", "From Position", min = 1, max = 1000, value = 1, step = 1)
                       ),
                       column(3,
                              sliderInput("to_range", "To Position", min = 1, max = 1000, value = 100, step = 1)
                       )
                     ),
                     fluidRow(
                       column(12,
                              actionButton("extract_segments_btn", "Extract Segments", 
                                           style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
                       )
                     ),
                     verbatimTextOutput("sequence_output")
            ),
            tabPanel("Motif Extraction", value = "motif_extraction",
                     fluidRow(
                       column(6,
                              fileInput("motif_file", "Upload PSA File", accept = c(".psa"))
                       )
                     ),
                     fluidRow(
                       column(12,
                              actionButton("extract_motifs_btn", "Extract Motifs", 
                                           style = "background-color: #4e62c8; color: white; font-family: 'Inter', sans-serif; border-radius: 5px; padding: 10px 15px; font-size: 1.1em;")
                       )
                     ),
                     tableOutput("motif_output")
            ),
            selected = "help"
          )
        ),
        bg = "#BBB1CE"
      )
    }
  })
}
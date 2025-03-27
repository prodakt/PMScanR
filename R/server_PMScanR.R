# R/server.R

#' Create the server function for PMScanR
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @return A Shiny server function
#' @noRd
build_server <- function(input, output, session) {

  # Reactive variables
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
  prosite_status_text <- reactiveVal("Analysis status: waiting for inputs")
  data_matrix <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  volumes <- getLogicalDrives()
  loading <- reactiveVal(FALSE)

  # Set up observers and outputs
  setup_observers(input, output, session, prosite_status_text, prosite_analysis_run, prosite_params, data_matrix, original_data, loading, volumes)
  setup_outputs(input, output, session, prosite_results_data, data_matrix, original_data, prosite_status_text, loading)

  return(list(
    prosite_results_data = prosite_results_data,
    prosite_params = prosite_params,
    prosite_analysis_run = prosite_analysis_run,
    prosite_status_text = prosite_status_text,
    data_matrix = data_matrix,
    original_data = original_data,
    loading = loading
  ))
}

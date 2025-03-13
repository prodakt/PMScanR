#' PMScanR Observers
#' @param input Shiny input object
#' @param session Shiny session object
#' @param sequences Reactive value for storing sequences
#' @param analysis_time Reactive value for storing analysis time
#' @export
pmscanr_observers <- function(input, session, sequences, analysis_time) {
  shiny::observeEvent(input$analyze, {  # Watch for clicks on the "Analyze" button
    shiny::req(input$file)              # Require a file to be uploaded, else stop
    start_time <- Sys.time()            # Record the start time of the analysis
    
    file_ext <- tools::file_ext(input$file$datapath) # Get the file extension (e.g., "fasta", "gff")
    if (file_ext %in% c("fasta", "fa")) {            # If file is FASTA format
      fasta_data <- Biostrings::readAAStringSet(input$file$datapath) # Read as FASTA
      seq_values <- as.character(fasta_data)    # Convert sequences to character strings
      seq_names <- names(fasta_data)            # Extract sequence names (e.g., ">seq1")
    } else {                                          # For other formats (GFF, PSA, TXT)
      shiny::showNotification("Only FASTA (.fasta, .fa) files are fully supported for now", type = "warning") # Warn user
      lines <- readLines(input$file$datapath)         # Read file as raw text lines
      seq_names <- lines[grep("^>", lines)]           # Extract lines starting with ">" as names
      seq_values <- lines[!grepl("^>", lines)]        # Assume other lines are sequences
      seq_names <- sub("^>", "", seq_names)           # Remove ">" from names
      if (length(seq_names) != length(seq_values)) {  # Check if names and sequences match
        shiny::showNotification("File format error: mismatched names and sequences", type = "error")
        return()
      }
    }
    
    # Filter sequences based on length
    valid_idx <- nchar(seq_values) >= input$min_length &  # Create logical index: TRUE if length >= min_length
      nchar(seq_values) <= input$max_length    # AND length <= max_length
    filtered_seqs <- seq_values[valid_idx]    # Subset sequences using the index
    filtered_names <- seq_names[valid_idx]    # Subset names to match filtered sequences
    
    if (length(filtered_seqs) == 0) {         # Check if any sequences remain after filtering
      shiny::showNotification("No valid sequences found", type = "error") # Show error if none found
      return()                                # Exit the observer if no sequences
    }
    
    sequences(data.frame(Name = filtered_names, Sequence = filtered_seqs)) # Store filtered data in sequences reactive
    analysis_time(difftime(Sys.time(), start_time, units = "secs")) # Calculate and store analysis time in seconds
  })
}

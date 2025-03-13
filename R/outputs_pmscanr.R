#' PMScanR Outputs
#' @param output Shiny output object
#' @param sequences Reactive value for sequences
#' @param input Shiny input object
#' @param analysis_time Reactive value for analysis time
#' @export
pmscanr_outputs <- function(output, sequences, input, analysis_time) {
  output$sequences_table <- DT::renderDT({    # Define how the sequences table is rendered
    shiny::req(sequences())                   # Require sequences to be available
    DT::datatable(sequences())                # Create an interactive DataTable from the sequences data frame
  })
  
  output$download_results <- shiny::downloadHandler( # Define the download button behavior
    filename = function() { input$output_name },     # Set filename from user input (e.g., "output.txt")
    content = function(file) {                       # Define what gets written to the file
      shiny::req(sequences())                        # Require sequences to be available
      if (input$output_format == "FASTA") {          # If user chose FASTA format
        Biostrings::writeXStringSet(                 # Write sequences in FASTA format
          Biostrings::AAStringSet(sequences()$Sequence), # Convert Sequence column to AAStringSet
          file)                                      # Write to the specified file path
      } else if (input$output_format == "TXT") {     # If user chose TXT format
        write.table(sequences(), file, sep = "\t", row.names = FALSE) # Write data frame as tab-separated text
      } else if (input$output_format == "GFF") {     # If user chose GFF format
        write_gff(sequences(), file)                 # Call helper function to write GFF
      } else if (input$output_format == "PSA") {     # If user chose PSA format
        write_psa(sequences(), file)                 # Call helper function to write PSA
      }
    }
  )
  
  output$aa_histogram <- shiny::renderPlot({  # Define the amino acid frequency plot
    shiny::req(sequences())                   # Require sequences to be available
    aa_freq <- table(unlist(strsplit(paste(sequences()$Sequence, collapse = ""), ""))) # Count amino acids
    # - paste(..., collapse = ""): Combine all sequences into one string
    # - strsplit(..., ""): Split into individual characters
    # - unlist: Flatten the list
    # - table: Count frequency of each amino acid
    ggplot2::ggplot(data.frame(AminoAcid = names(aa_freq), Frequency = as.numeric(aa_freq)), # Create data frame for plotting
                    ggplot2::aes(x = AminoAcid, y = Frequency)) + # Map amino acids to x, frequency to y
      ggplot2::geom_bar(stat = "identity", fill = "#138086") + # Bar plot with teal fill
      ggplot2::theme_minimal() +                    # Use a minimal theme
      ggplot2::ggtitle("Amino Acid Frequency")      # Add a title
  })
  
  output$length_plot <- shiny::renderPlot({     # Define the sequence length histogram
    shiny::req(sequences())                     # Require sequences to be available
    ggplot2::ggplot(sequences(), ggplot2::aes(x = nchar(Sequence))) + # Map sequence lengths to x-axis
      ggplot2::geom_histogram(binwidth = 5, fill = "#DC8665", color = "black") + # Histogram with coral fill, black borders
      ggplot2::theme_minimal() +                # Use a minimal theme
      ggplot2::ggtitle("Sequence Length Distribution") # Add a title
  })
  
  output$summary <- shiny::renderText({         # Define the summary text output
    shiny::req(sequences())                     # Require sequences to be available
    paste(                                      # Combine multiple lines of text
      "Analysis time:", round(analysis_time(), 2), "seconds\n", # Show analysis time, rounded to 2 decimals
      "Number of sequences:", nrow(sequences()), "\n",         # Show number of filtered sequences
      "Total size:", format(object.size(sequences()), units = "auto") # Show memory size of data frame
    )
  })
}

# Helper function to write GFF format
write_gff <- function(data, file) {
  gff_data <- data.frame(           # Create a data frame for GFF format
    seqid = data$Name,              # Column 1: Sequence ID from Name column
    source = "PMScanR",             # Column 2: Source of the data (hardcoded as PMScanR)
    type = "sequence",              # Column 3: Feature type (hardcoded as sequence)
    start = 1,                      # Column 4: Start position (assume 1 for simplicity)
    end = nchar(data$Sequence),     # Column 5: End position (length of each sequence)
    score = ".",                    # Column 6: Score (placeholder as ".")
    strand = "+",                   # Column 7: Strand (assume positive)
    phase = ".",                    # Column 8: Phase (placeholder as ".")
    attributes = paste0("ID=", data$Name) # Column 9: Attributes (e.g., "ID=seq1")
  )
  write.table(gff_data, file, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) # Write as tab-separated, no headers
}

# Helper function to write PSA format
write_psa <- function(data, file) {
  psa_data <- paste0(">", data$Name, "\n", data$Sequence) # Combine name and sequence with ">" prefix and newline
  writeLines(psa_data, file)                              # Write each line to the file
}
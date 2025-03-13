# Server Function
pmscanr_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    sequences <- reactiveVal()
    
    observeEvent(input$analyze, {
      req(input$file)
      fasta_data <- Biostrings::readAAStringSet(input$file$datapath)
      seq_values <- as.character(fasta_data)
      seq_names <- names(fasta_data)
      
      # Filter sequences
      valid_idx <- nchar(seq_values) >= input$min_length & 
        nchar(seq_values) <= input$max_length
      filtered_seqs <- seq_values[valid_idx]
      filtered_names <- seq_names[valid_idx]
      
      if (length(filtered_seqs) == 0) {
        showNotification("No valid sequences found", type = "error")
        return()
      }
      
      sequences(data.frame(Name = filtered_names, Sequence = filtered_seqs))
    })
    
    output$sequences_table <- DT::renderDT({
      req(sequences())
      DT::datatable(sequences())
    })
    
    output$download_results <- downloadHandler(
      filename = function() { input$output_name },
      content = function(file) {
        req(sequences())
        if (input$output_format == "FASTA") {
          Biostrings::writeXStringSet(
            Biostrings::AAStringSet(sequences()$Sequence), file)
        } else {
          write.table(sequences(), file, sep = "\t", row.names = FALSE)
        }
      }
    )
  })
}
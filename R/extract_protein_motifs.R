#' Extract protein motifs form a file in PSA format
#' 
#' This function reads a file in PSA format containing protein sequences and extracts motifs based on specific patterns.
#' 
#' @param file_path A PSA file which specifying the path to the input file.
#'                  The file should contain protein motifs.
#' @return A list with a motif identifier (e.g. PSXXXXX) and the coresponding to it motif sequence which is 
#'          associated wuth that identifier.
#' @example 
#' file_path <- "file in PSA format" 
#' protein_motifs <- extract_protein_motifs(file_path)
#' ggseqlogo(protein_motifs$PS60007, seq_type='aa')


extract_protein_motifs <- function(file_path) {
  lines <- readLines(file_path)
  motif_lines <- lines[grepl("^>.*: (PS\\d{5}) .*", lines, perl = TRUE)]
  
  # Getting the actual motifs from the following lines
  result <- list()
  for (i in seq_along(motif_lines)) {
    index <- which.max(lines == motif_lines[i]) + 1
    if (index <= length(lines) && grepl("^[A-Za-z]+$", lines[index])) {
      motif <- lines[index]
      ps_info <- sub("^>.*: (PS\\d{5}) .*", "\\1", motif_lines[i])
      result[[ps_info]] <- c(result[[ps_info]], motif)
    }
  }
  
  return(result)
}

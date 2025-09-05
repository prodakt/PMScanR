#' Extract protein motifs form a file in PSA format
#'
#' This function reads a file in PSA format containing protein sequences and extracts motifs based on specific patterns.
#'
#' @param file_path A PSA file which specifying the path to the input file.
#'                  The file should contain protein motifs.
#' @return A list with a motif identifier (e.g. PSXXXXX) and the coresponding to it motif sequence which is
#'          associated wuth that identifier.
#' @examples
#' file_path <- system.file("extdata/out_Hb_psa.txt", package = "PMScanR")
#' if (file_path != "") {
#'   protein_motifs <- extractProteinMotifs(file_path)
#' }
#' @export


extractProteinMotifs <- function(file_path) {
    lines <- readLines(file_path)
    motif_header_indices <-
        which(grepl("(PS\\d{5})", lines, perl = TRUE))
    
    result <- list()
    
    for (header_index in motif_header_indices) {
        sequence_index <- header_index + 1
        
        if (sequence_index <= length(lines) &&
            grepl("^[A-Za-z]+$", lines[sequence_index])) {
            motif_sequence <- lines[sequence_index]
            ps_identifier <-
                sub(".*(PS\\d{5}).*", "\\1", lines[header_index])
            result[[ps_identifier]] <-
                c(result[[ps_identifier]], motif_sequence)
        }
    }
    
    return(result)
}

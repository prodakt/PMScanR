#' Extract sequence fragments from a list of sequences
#'
#' This function iterates over a list of sequences and extracts a sub-sequence
#' from each based on a specified start and end position.
#'
#' @param sequences A list of sequences, where each element is a vector of
#'   single characters. This is typically the output of `seqinr::read.fasta`.
#' @param from An integer specifying the starting position for the extraction.
#' @param to An integer specifying the ending position for the extraction.
#'
#' @return A list representing the extracted sub-sequences. Sequences that were
#'   too short to have a fragment extracted are omitted from the list.
#' @examples
#' # Get the path to the example FASTA file
#' fasta_file <- system.file("extdata", "hemoglobins.fasta", package = "PMScanR")
#'
#' if (nzchar(fasta_file)) {
#'   sequences <- seqinr::read.fasta(fasta_file, seqtype = "AA")
#'   segments <- extractSegments(sequences, from = 10, to = 20)
#' }
#' @export
extractSegments <- function(sequences, from, to) {
    segments <- lapply(sequences, prepareSegments, from, to)
    segments <- segments[!is.na(segments)]
    
    return(segments)
}

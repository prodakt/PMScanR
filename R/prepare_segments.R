#' Extract a fragment from a file within a given range
#' 
#' This function extracts a sequence from a given file based on the specified range (`from` to `to`).
#' If the sequence is shorter than the specified range, the function returns `NA`
#' 
#' @param seq A character vector representing a sequence from file
#' @param from An integer specifying the starting position of the fragment to extract
#' @param to An integer specifying the ending position of the fragment to extract
#' @return A character string representing the extracted subsequence. 
#'          If the sequence is shorter than the specified range, `NA` is returned
#' @example 
#' seq <- read.fasta(file = 'file', seqtype = "AA")
#' fragment <- prepare_segments(seq, from = 10, to = 20)
#' print(fragment)
#' 
#' @export

prepare_segments <- function(seq, from, to) {
  if (length(seq) >= to) {
    return(paste(seq[from:to], collapse = ""))
  } else {
    return(NA)
  }
}



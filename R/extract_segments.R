#' Extract sequences from a file within a given range
#' 
#' This function extracts sequences from a file within the specified range (`from` to `to`).
#' Sequences shorter than the specified range are excluded from the result.
#' 
#' @param seq A FASTA file where are a sequences
#' @param from An integer specifying the starting position to extract from the file.
#' @param to An integer specifying the ending position to extract from the file
#' @return A list which are representing the extracted sequences. 
#'          Sequences shorter than the specified range are excluded.
#' @examples


extract_segments <- function(seq, from, to) {
  segments <- lapply(seq, prepare_segments, from, to)
  segments <- segments[!is.na(segments)]
  return(segments)
}


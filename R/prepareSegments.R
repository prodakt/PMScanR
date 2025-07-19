#' Extract a fragment from a sequence vector within a given range
#'
#' This is an internal helper function. It extracts a subsequence from a given
#' vector of characters based on the specified start (`from`) and end (`to`)
#' positions.
#'
#' @param seq A character vector representing a sequence.
#' @param from An integer specifying the starting position of the fragment.
#' @param to An integer specifying the ending position of the fragment.
#' @return A character string of the extracted subsequence, or `NA` if the
#'   sequence is too short.
#' @examples
#' # Create a sample sequence (a vector of single characters)
#' my_seq <- strsplit("ABCDEFGHIJ", "")[[1]]
#'
#' # Example 1: Extract a valid segment
#' prepareSegments(seq = my_seq, from = 3, to = 7) # Returns "CDEFG"
#'
#' # Example 2: Range exceeds sequence length, returns NA
#' prepareSegments(seq = my_seq, from = 5, to = 15) # Returns NA
#'
#' @noRd
prepareSegments <- function(seq, from, to) {
  # Check if the sequence is long enough for the extraction
  if (length(seq) >= to) {
    # Using seq() is slightly more robust than from:to
    return(paste(seq[seq(from, to)], collapse = ""))
  } else {
    return(NA)
  }
}

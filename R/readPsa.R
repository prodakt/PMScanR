#' Parse a PSA (PROSITE Scan ASCII) File
#'
#' This function reads a file in PSA format and converts it into a
#' standardized, GFF-like data frame for downstream analysis. It is robust
#' to files that may contain extraneous, non-PSA formatted data at the end.
#'
#' @param psa_file A character string specifying the path to the input PSA file.
#'
#' @return A data frame with a GFF-like structure, including all original
#'   placeholder columns.
#'
#' @examples
#' # Get the path to the example PSA file included with the package
#' psa_file_path <- system.file("extdata", "out_Hb_psa.txt", package = "PMScanR")
#'
#' # Check that the file exists before running the example
#' if (nzchar(psa_file_path)) {
#'   gff_like_data <- readPsa(psa_file_path)
#'   # You can view the output with:
#'   # head(gff_like_data)
#' }
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom stringr str_match
#' @export
readPsa <- function(psa_file) {
  
  if (!file.exists(psa_file)) {
    stop(sprintf("Input file not found: %s", psa_file), call. = FALSE)
  }
  
  psa_lines_all <- readLines(psa_file, warn = FALSE)
  first_tab_line <- grep("\t", psa_lines_all)[1]
  
  # If a tabbed line was found, truncate the lines to only the PSA section.
  if (!is.na(first_tab_line)) {
    psa_lines <- psa_lines_all[1:(first_tab_line - 1)]
  } else {
    psa_lines <- psa_lines_all
  }
  
  header_indices <- grep("^>\\b", psa_lines)
  
  if (length(header_indices) == 0) {
    warning("No header lines (starting with '>') found in the input file.")
    return(data.frame())
  }
  
  output_list <- list()
  pattern <- "^>\\s*([^/]+)/(\\d+)-(\\d+)\\s*:\\s*(\\S+)\\s+(\\S+)(\\s+L=(\\d+))?\\s*$"
  
  for (i in seq_along(header_indices)) {
    start_line <- header_indices[i]
    end_line <- if (i < length(header_indices)) header_indices[i+1] - 1 else length(psa_lines)
    block <- psa_lines[start_line:end_line]
    
    if (length(block) == 0) next
    
    header_line <- block[1]
    sequence_lines <- if (length(block) > 1) block[-1] else ""
    matches <- stringr::str_match(header_line, pattern)
    
    if (is.na(matches[1, 1])) {
      warning(sprintf("Could not parse header line: %s", header_line), call. = FALSE)
      next
    }
    
    seq_name <- matches[1, 2]
    start_pos <- as.integer(matches[1, 3])
    end_pos <- as.integer(matches[1, 4])
    motif_type <- matches[1, 5]
    motif_name <- matches[1, 6]
    level_val <- if (!is.na(matches[1, 8])) matches[1, 8] else NA_character_
    sequence <- paste(sequence_lines, collapse = "")
    
    if (is.na(start_pos) || is.na(end_pos)) {
      warning(sprintf("Could not convert start/end to integer in header: %s", header_line), call. = FALSE)
      next
    }
    
    tmpdf <- data.frame(
      seqnames = seq_name,
      start = start_pos,
      end = end_pos,
      width = end_pos - start_pos + 1,
      strand = NA_character_,
      source = "PSA",
      type = motif_type,
      score = NA_real_,
      phase = NA_integer_,
      Name = motif_name,
      Sequence = sequence,
      SequenceDescription = NA,
      SkipFlag = NA,
      KnownFalsePos = NA,
      Level = level_val,
      RawScore = NA,
      FeatureFrom = NA,
      FeatureTo = NA,
      stringsAsFactors = FALSE
    )
    
    output_list[[i]] <- tmpdf
  }
  
  if (length(output_list) == 0) {
    warning("No valid PSA blocks were processed.")
    return(data.frame())
  }
  
  final_output <- do.call(rbind, output_list)
  
  final_output <- final_output %>%
    dplyr::select(
      "seqnames", "source", "type", "start", "end", "score", "strand", "phase",
      "Name", "Sequence", "Level", "SequenceDescription", "width",
      "SkipFlag", "KnownFalsePos", "RawScore", "FeatureFrom", "FeatureTo"
    )
  
  return(final_output)
}

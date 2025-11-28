#' Convert PROSITE format to a GFF-like Data Frame
#'
#' This function parses a file from a PROSITE scan (standard output format)
#' into a data frame. It handles multi-line sequence outputs and extracts
#' information into a GFF-like structure compatible with rtracklayer imports.
#'
#' @param prosite_input Path to the PROSITE scan output file.
#' @return A data frame with columns approximating GFF fields plus additional
#'   PROSITE-specific information.
#' @examples
#' # Get path to example file
#' prosite_file <- system.file("extdata", "out_Hb_PROSITE.txt", package = "PMScanR")
#'
#' if (nzchar(prosite_file) && file.exists(prosite_file)) {
#'   prosite_data <- readProsite(prosite_file)
#'   head(prosite_data)
#' }
#' @importFrom dplyr mutate select bind_rows
#' @importFrom magrittr %>%
#' @importFrom stringr str_match str_detect
#' @export
readProsite <- function(prosite_input) {
  if (!file.exists(prosite_input)) {
    stop(sprintf("Input file not found: %s", prosite_input), call. = FALSE)
  }

  input_lines <- readLines(prosite_input, warn = FALSE)
  header_indices <- grep("^>", input_lines)

  if (length(header_indices) == 0) {
    warning("No header lines (starting with '>') found in the input file.")
    return(data.frame())
  }

  output_list <- list()

  header_pattern <- "^>\\s*([^\\s:]+)\\s*:\\s*(\\S+)\\s+(\\S+)\\s*(.*)$"
  hit_start_pattern <- "^\\s*(\\d+)\\s*-\\s*(\\d+)\\s+(\\S+)(.*)$"

  for (i in seq_along(header_indices)) {
    start_line <- header_indices[i]
    end_line <- if (i < length(header_indices)) header_indices[i+1] - 1 else length(input_lines)

    block <- input_lines[start_line:end_line]
    header_line <- block[1]

    h_match <- stringr::str_match(header_line, header_pattern)
    if (is.na(h_match[1,1])) {
      next
    }

    seq_name   <- h_match[1, 2]
    motif_type <- h_match[1, 3]
    motif_name <- h_match[1, 4]
    seq_desc   <- h_match[1, 5]

    hit_starts_idx <- which(stringr::str_detect(block, hit_start_pattern))

    if (length(hit_starts_idx) == 0) next

    for (j in seq_along(hit_starts_idx)) {
      curr_idx <- hit_starts_idx[j]
      curr_line <- block[curr_idx]

      hit_match <- stringr::str_match(curr_line, hit_start_pattern)

      s_pos <- as.integer(hit_match[1, 2])
      e_pos <- as.integer(hit_match[1, 3])
      seq_chunk <- hit_match[1, 4]
      remainder <- hit_match[1, 5]

      level_val <- NA_character_
      if (stringr::str_detect(remainder, "L=")) {
        level_match <- stringr::str_match(remainder, "L=(\\d+)")
        if (!is.na(level_match[1,2])) {
          level_val <- level_match[1,2]
        }
      }

      full_sequence <- seq_chunk
      next_hit_idx <- if (j < length(hit_starts_idx)) hit_starts_idx[j+1] else length(block) + 1

      if (curr_idx + 1 < next_hit_idx) {
        continuation_lines <- block[(curr_idx + 1):(next_hit_idx - 1)]
        cleaned_lines <- gsub("\\s+", "", continuation_lines)
        full_sequence <- paste0(full_sequence, paste0(cleaned_lines, collapse = ""))
      }

      tmp_row <- data.frame(
        seqnames = seq_name,
        start = s_pos,
        end = e_pos,
        width = as.integer(e_pos - s_pos + 1),
        strand = factor("*", levels = c("+", "-", "*")),
        source = "PROSITE",
        type = motif_type,
        score = NA_real_,
        phase = NA_integer_,
        Name = motif_name,
        Sequence = full_sequence,
        SequenceDescription = seq_desc,
        SkipFlag = NA_character_,
        Level = level_val,
        KnownFalsePos = NA_character_,
        RawScore = NA_character_,
        FeatureFrom = NA_character_,
        FeatureTo = NA_character_,
        stringsAsFactors = FALSE
      )

      output_list[[length(output_list) + 1]] <- tmp_row
    }
  }

  if (length(output_list) == 0) {
    return(data.frame())
  }

  final_output <- dplyr::bind_rows(output_list)

  final_output <- final_output %>%
    dplyr::select(
      "seqnames", "start", "end", "width", "strand", "source", "type",
      "score", "phase", "Name", "Sequence", "SequenceDescription",
      "SkipFlag", "Level", "RawScore", "FeatureFrom", "FeatureTo"
    )

  return(final_output)
}

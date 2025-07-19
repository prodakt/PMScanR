#' Convert PROSITE format to a GFF-like Data Frame
#'
#' This function parses a file from a PROSITE scan into a data frame,
#' extracting information about motif occurrences into a GFF-like structure.
#'
#' @param prosite_input Path to the PROSITE scan output file.
#' @return A data frame with columns approximating GFF fields plus additional
#'   PROSITE-specific information.
#'
#' @examples
#' prosite_file <- system.file("extdata", "PROSITEoutput.txt", package = "PMScanR")
#'
#' # Check that the example file exists before running
#' if (nzchar(prosite_file)) {
#'   gff_like_data <- readProsite(prosite_file)
#'   # You can view the output with:
#'   # head(gff_like_data)
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
readProsite <- function(prosite_input) {

  # Check if input file exists
  if (!file.exists(prosite_input)) {
    stop(sprintf("Input file not found: %s", prosite_input), call. = FALSE)
  }

  # Read lines from the input file
  input_lines <- readLines(prosite_input, warn = FALSE)

  # Filter for headers ('>') or motif hits (e.g., '10 - 20')
  relevant_lines <- input_lines[grepl("^>\\b+", input_lines) | grepl("^\\s*\\d+\\s+-\\s+\\d+", input_lines)]

  if (length(relevant_lines) == 0) {
    warning("No relevant header or motif lines found in the input file.")
    # Return an empty data frame with the expected structure
    return(data.frame(seqnames=character(), source=character(), type=character(), start=integer(), end=integer(), score=numeric(),
                      strand=character(), phase=integer(), Name=character(), Sequence=character(), Level=character(),
                      SequenceDescription=character(), width=integer(), SkipFlag=logical(), KnownFalsePos=logical(),
                      RawScore=numeric(), FeatureFrom=integer(), FeatureTo=integer(),
                      stringsAsFactors=FALSE))
  }

  output_list <- list()
  split_lines <- split(relevant_lines, cumsum(grepl("^>\\b", relevant_lines)))
  num_chunks <- length(split_lines)

  for (i in seq_len(num_chunks)) {
    current_chunk <- split_lines[[as.character(i)]]
    if (length(current_chunk) < 2) next # Skip if malformed

    header_line <- current_chunk[1]
    hit_lines <- current_chunk[-1]

    seq_name <- sub("^>\\s*([^\\s:]+).*", "\\1", header_line)
    motif_type <- sub("^>.*?:\\s*(\\S+).*", "\\1", header_line)
    motif_name <- sub("^>.*?:\\s*\\S+\\s+(\\S+).*", "\\1", header_line)
    seq_description_part <- sub("^>.*?:\\s*\\S+\\s+\\S+\\s*(.*)", "\\1", header_line)
    full_seq_description <- paste(seq_name, seq_description_part)

    # --- Process Hit Lines ---
    missing_level_indices <- !grepl("\\bL=\\d+$", hit_lines)
    if(any(missing_level_indices)) {
      hit_lines[missing_level_indices] <- paste0(hit_lines[missing_level_indices], " L=NA")
    }

    cleaned_hit_lines <- gsub("^\\s+|\\s+$|\\s+-\\s+|\\s+", " ", hit_lines)
    split_hit_data <- strsplit(cleaned_hit_lines, " ")

    lengths <- vapply(split_hit_data, length, integer(1))
    valid_splits <- lengths == 4
    if(!all(valid_splits)){
      warning("Skipping malformed hit lines for sequence: ", seq_name, call. = FALSE)
      split_hit_data <- split_hit_data[valid_splits]
      if(length(split_hit_data) == 0) next
    }

    tmptab <- as.data.frame(do.call(rbind, split_hit_data), stringsAsFactors = FALSE)
    colnames(tmptab) <- c("start", "end", "Sequence", "Level")

    # Convert start/end to numeric; LET the warning about NAs appear if data is invalid
    tmptab$start <- as.integer(tmptab$start)
    tmptab$end <- as.integer(tmptab$end)

    tmptab <- tmptab[!is.na(tmptab$start) & !is.na(tmptab$end), ]
    if(nrow(tmptab) == 0) next

    tmptab <- tmptab %>%
      mutate(
        seqnames = seq_name,
        width = .data$end - .data$start + 1,
        strand = NA_character_,
        source = "PROSITE",
        type = motif_type,
        score = NA_real_,
        phase = NA_integer_,
        Name = motif_name,
        SequenceDescription = full_seq_description,
        Level = as.character(.data$Level),
        SkipFlag = NA, KnownFalsePos = NA, RawScore = NA, FeatureFrom = NA, FeatureTo = NA
      )
    output_list[[i]] <- tmptab
  }

  if (length(output_list) == 0) return(data.frame()) # Return empty frame if nothing was processed

  final_output <- do.call(rbind, output_list)

  final_output <- final_output %>%
    select(
      "seqnames", "source", "type", "start", "end", "score", "strand", "phase",
      "Name", "Sequence", "Level", "SequenceDescription", "width",
      "SkipFlag", "KnownFalsePos", "RawScore", "FeatureFrom", "FeatureTo"
    ) %>%
    mutate(Level = gsub("L=", "", .data$Level))

  return(final_output)
}

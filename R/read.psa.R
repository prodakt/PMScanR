#' Convert PSA format to GFF-like Data Frame
#'
#' This function reads and parses data in PSA (PROSITE Scan ASCII) format
#' into a data frame resembling the GFF (General Feature Format) structure.
#'
#' @param psa_input Path to a file containing output from a PSA scan.
#' @return A data frame with columns approximating GFF fields plus additional
#'   PSA-specific information like 'Name', 'Sequence', 'Level', etc.
#'
#' @examples
#' # Use example file included with the package
#' file_path <- system.file("extdata", "out_Hb_psa.txt", package = "PMScanR")
#' gffdata <- NULL # Initialize
#' if (file_path != "" && file.exists(file_path)) {
#'   # Check dependencies for example
#'   if (requireNamespace("dplyr", quietly = TRUE) &&
#'       requireNamespace("magrittr", quietly = TRUE) &&
#'       requireNamespace("stringr", quietly = TRUE)) {
#'      gffdata <- read.psa(file_path)
#'      # print(head(gffdata))
#'   }
#' } else {
#'    message("Example file 'extdata/out_Hb_psa.txt' not found in PMScanR package.")
#'    # Can optionally create dummy data here if needed for examples to pass
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom stringr str_match
#' @export
read.psa <- function(psa_input) {

  # Check if input file exists
  if (!file.exists(psa_input)) {
    stop("Input file not found: ", psa_input, call. = FALSE)
  }

  # Read lines from the input file
  psa_lines <- readLines(psa_input, warn = FALSE)

  # Find indices of header lines (starting with '>')
  header_indices <- grep("^>\\b", psa_lines)

  # Check if any header lines were found
  if (length(header_indices) == 0) {
    warning("No header lines (starting with '>') found in the input file.")
    # Return an empty data frame with expected columns
    return(data.frame(seqnames=character(), start=integer(), end=integer(), width=integer(),
                      strand=character(), source=character(), type=character(), score=numeric(),
                      phase=integer(), Name=character(), Sequence=character(),
                      SequenceDescription=character(), SkipFlag=logical(), KnownFalsePos=logical(),
                      Level=character(), RawScore=numeric(), FeatureFrom=integer(), FeatureTo=integer(),
                      stringsAsFactors=FALSE))
  }

  output_list <- list() # Initialize a list to store data frames for each block

  # Define the regular expression pattern to capture parts of the header line
  pattern <- "^>\\s*([^/]+)/(\\d+)-(\\d+)\\s*:\\s*(\\S+)\\s+(\\S+)(\\s+L=(\\d+))?\\s*$"

  # Process each block defined by headers
  num_blocks <- length(header_indices)
  for (i in seq_len(num_blocks)) {
    start_line <- header_indices[i]
    # Determine end line for the block (next header or end of file)
    end_line <- if (i < num_blocks) header_indices[i+1] - 1 else length(psa_lines)

    # Extract the block lines
    block <- psa_lines[start_line:end_line]

    # Ensure block is not empty
    if (length(block) == 0) next

    header_line <- block[1]
    sequence_lines <- if (length(block) > 1) block[-1] else ""

    # Parse the header using stringr::str_match for efficiency
    matches <- stringr::str_match(header_line, pattern)

    # Check if parsing was successful
    if (is.na(matches[1, 1])) { # Check if the overall match failed
      warning("Could not parse header line: ", header_line, call. = FALSE)
      next # Skip this block
    }

    # Extract captured groups (match matrix columns: [,1]=full, [,2]=grp1, etc.)
    seq_name <- matches[1, 2]
    start_pos <- suppressWarnings(as.integer(matches[1, 3]))
    end_pos <- suppressWarnings(as.integer(matches[1, 4]))
    motif_type <- matches[1, 5]
    motif_name <- matches[1, 6]
    # Level is in the 8th column of the match matrix (7th is the optional space+L= part)
    level_val <- if (!is.na(matches[1, 8])) matches[1, 8] else NA_character_

    # Combine sequence lines
    sequence <- paste(sequence_lines, collapse = "")

    # Check if start/end conversion worked
    if (is.na(start_pos) || is.na(end_pos)) {
      warning("Could not convert start/end to integer in header: ", header_line, call. = FALSE)
      next # Skip this block
    }

    # Create a temporary data frame for this hit
    tmpdf <- data.frame(
      seqnames = seq_name,
      start = start_pos,
      end = end_pos,
      width = end_pos - start_pos + 1, # Calculate width
      strand = NA_character_,          # PSA doesn't typically specify strand
      source = "PSA",                  # Source is PSA format
      type = motif_type,
      score = NA_real_,
      phase = NA_integer_,
      Name = motif_name,
      Sequence = sequence,
      SequenceDescription = NA,        # PSA header less descriptive than PROSITE format
      SkipFlag = NA,
      KnownFalsePos = NA,
      Level = level_val,               # Use parsed level
      RawScore = NA,
      FeatureFrom = NA,
      FeatureTo = NA,
      stringsAsFactors = FALSE
    )

    output_list[[i]] <- tmpdf # Add data frame to the list
  }

  # Combine all processed data frames from the list
  if (length(output_list) > 0) {
    final_output <- do.call(rbind, output_list)
  } else {
    # Return empty frame with correct columns if no data was processed
    warning("No valid PSA blocks were processed.")
    return(data.frame(seqnames=character(), start=integer(), end=integer(), width=integer(),
                      strand=character(), source=character(), type=character(), score=numeric(),
                      phase=integer(), Name=character(), Sequence=character(),
                      SequenceDescription=character(), SkipFlag=logical(), KnownFalsePos=logical(),
                      Level=character(), RawScore=numeric(), FeatureFrom=integer(), FeatureTo=integer(),
                      stringsAsFactors=FALSE))
  }

  # Final column selection and ordering (using the pipe correctly)
  final_output <- final_output %>%
    dplyr::select(
      "seqnames", "source", "type", "start", "end", "score", "strand", "phase", # GFF-like core
      "Name", "Sequence", "Level", "SequenceDescription", "width", # Extras
      "SkipFlag", "KnownFalsePos", "RawScore", "FeatureFrom", "FeatureTo" # Placeholders
    )
  # No mutate needed here unless further cleaning is required

  return(final_output)
}

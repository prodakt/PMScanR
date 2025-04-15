#' Convert PROSITE format to GFF-like Data Frame
#'
#' This function reads and parses data in PROSITE scan format into a data frame
#' resembling the GFF (General Feature Format) structure.
#' PROSITE is a database of protein families and domains. This function extracts
#' relevant information about motif occurrences.
#'
#' @param prosite_input Path to a file containing output from a PROSITE scan
#'   (typically plain text format, not the `.dat` database itself).
#' @return A data frame with columns approximating GFF fields plus additional
#'   PROSITE-specific information like 'Name', 'Sequence', 'Level', etc.
#'
#' @examples
#' # Corrected path for example file within the package
#' prosite_file <- system.file("extdata", "PROSITEoutput.txt", package = "PMScanR")
#' gff_data <- NULL # Initialize gff_data
#' if (prosite_file != "" && file.exists(prosite_file)) {
#'   # Check if dependencies are available for the example
#'   if (requireNamespace("dplyr", quietly = TRUE) &&
#'       requireNamespace("magrittr", quietly = TRUE)) {
#'     gff_data <- read.prosite(prosite_file)
#'     # print(head(gff_data)) # Optional: view the first few rows
#'   }
#' } else {
#'   message("Example file 'extdata/PROSITEoutput.txt' not found in PMScanR package.")
#'   # Create dummy data if file not found, so example doesn't error
#'   dummy_prosite_content <- c(
#'    ">Seq1 : PS00001 MOTIF_A Some description here",
#'    "10 - 20",
#'    "35 - 45 L=0",
#'    ">Seq2 : PS00002 MOTIF_B Another description",
#'    "5 - 15 L=1",
#'    "100 - 110"
#'   )
#'   dummy_file <- tempfile(fileext = ".txt")
#'   writeLines(dummy_prosite_content, dummy_file)
#'   if (requireNamespace("dplyr", quietly = TRUE) &&
#'       requireNamespace("magrittr", quietly = TRUE)) {
#'      gff_data <- read.prosite(dummy_file)
#'      # print(head(gff_data))
#'   }
#'   unlink(dummy_file)
#' }
#'
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
read.prosite <- function(prosite_input) {

  # Check if input file exists
  if (!file.exists(prosite_input)) {
    stop("Input file not found: ", prosite_input, call. = FALSE)
  }

  # Read lines from the input file
  input_lines <- readLines(prosite_input, warn = FALSE)

  # Filter lines containing sequence headers (starting with '>') or motif hits
  relevant_lines <- input_lines[grepl("^>\\b+", input_lines) | grepl("^\\s*\\d+\\s+-\\s+\\d+", input_lines)]

  # Check if any relevant lines were found
  if (length(relevant_lines) == 0) {
    warning("No relevant header or motif lines found in the input file.")
    # Return an empty data frame with expected columns
    return(data.frame(seqnames=character(), source=character(), type=character(), start=integer(), end=integer(), score=numeric(),
                      strand=character(), phase=integer(), Name=character(), Sequence=character(), Level=character(),
                      SequenceDescription=character(), width=integer(), SkipFlag=logical(), KnownFalsePos=logical(),
                      RawScore=numeric(), FeatureFrom=integer(), FeatureTo=integer(),
                      stringsAsFactors=FALSE))
  }

  output_list <- list() # Initialize a list to store data frames

  # Split the relevant lines into chunks based on the sequence header lines
  split_lines <- split(relevant_lines, cumsum(grepl("^>\\b", relevant_lines)))

  # Process each chunk (use seq_along instead of 1:length)
  # Using seq_len(length(split_lines)) is safer than 1:length()
  num_chunks <- length(split_lines)
  for (i in seq_len(num_chunks)) {
    current_chunk <- split_lines[[as.character(i)]] # split names groups as '1', '2', etc.

    if (length(current_chunk) < 2) next # Skip if only header or malformed

    header_line <- current_chunk[1]
    hit_lines <- current_chunk[-1]

    # --- Parse Header Line ---
    seq_name <- sub("^>\\s*([^\\s:]+).*", "\\1", header_line)
    motif_type <- sub("^>.*?:\\s*(\\S+).*", "\\1", header_line)
    motif_name <- sub("^>.*?:\\s*\\S+\\s+(\\S+).*", "\\1", header_line)
    seq_description_part <- sub("^>.*?:\\s*\\S+\\s+\\S+\\s*(.*)", "\\1", header_line)
    full_seq_description <- paste0(seq_name, " ", seq_description_part)

    # --- Process Hit Lines ---
    missing_level_indices <- !grepl("\\bL=\\d+$", hit_lines)
    hit_lines[missing_level_indices] <- paste0(hit_lines[missing_level_indices], " L=NA")

    # Clean up hit lines sequentially (removed pipe with '.' placeholder)
    cleaned_hit_lines <- gsub("^\\s+|\\s+$", "", hit_lines)
    cleaned_hit_lines <- gsub("\\s+-\\s+", " ", cleaned_hit_lines)
    cleaned_hit_lines <- gsub("\\s+", " ", cleaned_hit_lines)

    split_hit_data <- strsplit(cleaned_hit_lines, " ")

    valid_splits <- sapply(split_hit_data, length) == 4
    if(!all(valid_splits)){
      warning("Skipping malformed hit lines for sequence: ", seq_name, call. = FALSE)
      split_hit_data <- split_hit_data[valid_splits]
      if(length(split_hit_data) == 0) next
    }

    tmptab <- as.data.frame(do.call(rbind, split_hit_data), stringsAsFactors = FALSE)
    if (ncol(tmptab) == 4) {
      colnames(tmptab) <- c("start", "end", "Sequence", "Level")
    } else {
      warning("Unexpected number of columns after processing hit lines for sequence: ", seq_name, ". Skipping.", call. = FALSE)
      next
    }

    # Convert start/end to numeric early for width calculation
    tmptab$start <- suppressWarnings(as.integer(tmptab$start))
    tmptab$end <- suppressWarnings(as.integer(tmptab$end))
    # Handle potential NAs introduced by coercion
    tmptab <- tmptab[!is.na(tmptab$start) & !is.na(tmptab$end), ]
    if(nrow(tmptab) == 0) next # Skip if no valid numeric ranges remain


    # --- Combine Header and Hit Data ---
    tmptab <- tmptab %>%
      dplyr::mutate(
        seqnames = seq_name,
        # Calculate width using numeric start/end
        width = .data$end - .data$start + 1,
        strand = NA_character_,
        source = "PROSITE",
        type = motif_type,
        score = NA_real_,
        phase = NA_integer_,
        Name = motif_name,
        SequenceDescription = full_seq_description,
        # Ensure Level is character before potential modification later
        Level = as.character(.data$Level),
        # Add other placeholder columns
        SkipFlag = NA,
        KnownFalsePos = NA,
        RawScore = NA,
        FeatureFrom = NA,
        FeatureTo = NA
      )

    output_list[[i]] <- tmptab
  }

  # Combine all processed data frames
  if (length(output_list) > 0) {
    final_output <- do.call(rbind, output_list)
  } else {
    # Return empty frame with correct columns if no data was processed
    return(data.frame(seqnames=character(), source=character(), type=character(), start=integer(), end=integer(), score=numeric(),
                      strand=character(), phase=integer(), Name=character(), Sequence=character(), Level=character(),
                      SequenceDescription=character(), width=integer(), SkipFlag=logical(), KnownFalsePos=logical(),
                      RawScore=numeric(), FeatureFrom=integer(), FeatureTo=integer(),
                      stringsAsFactors=FALSE))
  }

  # Final selection and formatting
  final_output <- final_output %>%
    dplyr::select(
      "seqnames", "source", "type", "start", "end", "score", "strand", "phase", # GFF-like core
      "Name", "Sequence", "Level", "SequenceDescription", "width", # Extras
      "SkipFlag", "KnownFalsePos", "RawScore", "FeatureFrom", "FeatureTo" # Placeholders
    ) %>%
    # Clean up the Level column using .data$
    dplyr::mutate(
      Level = gsub("L=", "", .data$Level)
    )

  return(final_output)
}

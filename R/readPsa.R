<<<<<<< Updated upstream:R/read.psa.R
#' Convert PSA to GFF
#' 
#' This function converts a PSA format data into a GFF format data frame
#' 
#' @param psa A character vector where each element represents a line from a PSA file, file in a psa format
#' @return A data frame in GFF format
#' @example 
#' raw_data <- 'file'
#' s1 <- PSAtoGFF(raw_data)
#' print(raw_data)
#' 
#' @importFrom dplyr mutate select
#' @export


read.psa <- function(psa_input) {
  psa <- readLines(psa_input)
  outputpsa <- data.frame() 
  x <- sum(grepl("^>\\b", psa)) 
  for (i in 1:x) { 
    block <- split(psa, cumsum(grepl("^>\\b", psa)))[[i]] 
    tmpdf <- data.frame(start = NA) 
    if (grepl("L=", block[1]) == T) { 
      pattern <- "^>(\\S+)/(\\d+)-(\\d+)\\s:\\s(\\w+)\\s(\\w+)\\sL=(\\d+)\\s$" 
      tmpdf$Level =  gsub(pattern, "\\6", block[1])
    } else {
      pattern <- "^>(\\S+)/(\\d+)-(\\d+)\\s:\\s(\\w+)\\s(\\w+)\\s$" 
      tmpdf$Level = NA 
=======
#' Parse a PSA (PROSITE Scan ASCII) File
#'
#' This function reads a file in PSA format and converts it into a
#' standardized, GFF-like data frame for downstream analysis.
#'
#' @param psa_file A character string specifying the path to the input PSA file.
#'
#' @return A data frame with a GFF-like structure, containing columns such as
#'   'seqnames', 'start', 'end', and 'Name'.
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
#' @export
readPsa <- function(psa_file) {
  # Check if input file exists
  if (!file.exists(psa_file)) {
    stop("Input file not found: ", psa_file, call. = FALSE)
  }

  # Read lines from the input file
  psa_lines <- readLines(psa_file, warn = FALSE)

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
>>>>>>> Stashed changes:R/readPsa.R
    }
    tmpdf <- tmpdf %>% 
      mutate(start = gsub(pattern, "\\2", block[1]), 
             end = gsub(pattern, "\\3", block[1]), 
             Sequence = paste(block[-1], collapse = ""), 
             seqnames = gsub(pattern, "\\1", block[1]), 
             width = NA,
             strand = NA,
             source = NA,
             type = gsub(pattern, "\\4", block[1]), 
             score = NA,
             phase = NA,
             Name = gsub(pattern, "\\5", block[1]), 
             SkipFlag = NA,
             KnownFalsePos = NA,
             RawScore = NA,
             FeatureFrom = NA,
             FeatureTo = NA,
             SequenceDescription = NA) 
    outputpsa <- rbind(outputpsa, tmpdf) 
  }
  outputpsa <- outputpsa %>%
    select(c("seqnames", "start", "end", "width", "strand", "source", "type",
             "score", "phase", "Name", "Sequence", "SequenceDescription",
             "SkipFlag", "KnownFalsePos", "Level", "RawScore", "FeatureFrom",
             "FeatureTo")) 
  return(outputpsa)
} 


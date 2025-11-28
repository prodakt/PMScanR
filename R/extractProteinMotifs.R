#' Extract protein motifs from GFF, PSA, or PROSITE text files
#'
#' This function extracts protein motif sequences from various file formats
#' output by PROSITE analysis tools. It automatically detects the format
#' (GFF, PSA, or standard PROSITE scan output) and returns a list of sequences
#' grouped by motif identifier.
#'
#' @param file_path A character string specifying the path to the input file.
#' @param format A character string specifying the format: "auto" (default),
#'   "gff", "psa", or "scan" (for PROSITE text output).
#'
#' @return A list where keys are motif identifiers (e.g., "PS00001") and values
#'   are character vectors of the corresponding motif sequences found.
#'   Returns an empty list if no motifs/sequences are found.
#'
#' @examples
#' # Example with PSA file
#' psa_file <- system.file("extdata", "out_Hb_psa.txt", package = "PMScanR")
#' if (nzchar(psa_file)) {
#'   motifs <- extractProteinMotifs(psa_file)
#'   # head(motifs$PS00005)
#' }
#'
#' @export
extractProteinMotifs <- function(file_path, format = "auto") {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  if (format == "auto") {
    format <- detectFileFormat(file_path)
  }

  data <- switch(
    format,
    "gff" = {
      if (!requireNamespace("rtracklayer", quietly = TRUE)) {
        stop("Package 'rtracklayer' is required for GFF parsing.")
      }
      df <- as.data.frame(rtracklayer::import.gff(file_path))
      df
    },
    "psa" = {
      readPsa(file_path)
    },
    "scan" = {
      readProsite(file_path)
    },
    stop("Unsupported format specified: ", format)
  )

  if (nrow(data) == 0) {
    warning("No data found in the file.")
    return(list())
  }

  if (!"Sequence" %in% colnames(data)) {
    warning("The input file does not contain 'Sequence' information. ",
            "Cannot extract motifs. Ensure the PROSITE run included sequences.")
    return(list())
  }

  motif_sequences <- as.character(data$Sequence)
  motif_ids <- as.character(data$type)

  valid_idx <- !is.na(motif_sequences) & motif_sequences != ""
  if (!any(valid_idx)) {
    return(list())
  }

  result <- split(motif_sequences[valid_idx], motif_ids[valid_idx])

  return(result)
}

#' @noRd
detectFileFormat <- function(file_path) {
  lines <- readLines(file_path, n = 15, warn = FALSE)


  if (any(grepl("^##gff", lines)) ||
      (any(grepl("\t", lines)) && any(grepl("ps_scan", lines)))) {
    return("gff")
  }

  if (any(grepl("^>.*\\/\\d+-\\d+", lines))) {
    return("psa")
  }

  if (any(grepl("^>.*\\s*:\\s*PS\\d{5}", lines))) {
    return("scan")
  }

  stop("Could not automatically detect file format. Please specify 'format' argument.")
}

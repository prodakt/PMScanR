#' Convert PROSITE format to GFF
#'
#' This function converts data in PROSITE format into a GFF data frame.
#' PROSITE is a database of protein families and domains, and this function parses the relevant information
#' into a structured GFF data frame suitable for further analysis and visualization.
#'
#' @param prosite_input A file in PROSITE format from Prosite database
#' @return A GFF file as data frame.
#' @examples
#' prosite_file <- system.file("inst/extdata/PROSITEoutput.txt", package = "PMScanR")
#' if (prosite_file != "") {
#' gff_data <- read.prosite(prosite_file)
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export



read.prosite <- function(prosite_input) {
  input <- readLines(prosite_input)
  output <- data.frame()
  test <- input[grepl("^>\\b+", input) | grepl("^\\s+\\d+", input) | grepl("^\\d+\\s", input)]
  x <- sum(grepl("^>\\b", test))
  for (i in 1:x) {
    small_lines <- split(test, cumsum(grepl("^>\\b", test)))[[i]]
    z <- !grepl("\\bL\\W+\\d+$", small_lines)
    z[1] <- F
    small_lines[z] <- paste0(small_lines[z], " L=NA")
    small_lines <- small_lines %>%
      gsub("^\\s+", "",.) %>%
      gsub(" - ", " ",.) %>%
      gsub("\\s+", " ",.)
    tmptab <- as.data.frame(do.call(rbind, strsplit(small_lines[2:length(small_lines)], " ")))
    colnames(tmptab) <- c("start", "end", "Sequence", "Level")
    tmptab <- tmptab %>%
      mutate(seqnames = gsub("^>(.*?)\\s:.*$", "\\1", small_lines[1]),
             width = NA,
             strand = NA,
             source = NA,
             type = gsub("^>.*?:\\s(\\S+).*", "\\1", small_lines[1]),
             score = NA,
             phase = NA,
             Name = gsub("^>.*?:\\s\\S+\\s(\\S+)\\s.*", "\\1", small_lines[1]),
             SkipFlag = NA,
             KnownFalsePos = NA,
             RawScore = NA,
             FeatureFrom = NA,
             FeatureTo = NA)
    small_lines[1] <- gsub("^>.*?:\\s\\S+\\s\\S+\\s"," ", small_lines[1])
    tmptab <- tmptab %>%
      mutate(SequenceDescription = paste0(tmptab$seqnames, "", small_lines[1]))
    output <- rbind(output, tmptab)

  }
  output <- output %>%
    select(c("seqnames", "start", "end", "width", "strand", "source", "type",
             "score", "phase", "Name", "Sequence", "SequenceDescription",
             "SkipFlag", "KnownFalsePos", "Level", "RawScore", "FeatureFrom",
             "FeatureTo")) %>%
    mutate(Level = gsub("L=", "", Level))
  return(output)
}


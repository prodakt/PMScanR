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


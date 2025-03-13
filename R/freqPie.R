#' Create a pie chart from a GFF file which showing the distribution of protein motifs in the 'Name' column
#' 
#' This function calculates the number of occurrences and percentages for each protein motifs in the `Name` column.
#' It then creates a pie chart using `ggplot2` to visualize the distribution of these protein motifs.
#' 
#' @param data A file in GFF format containing a column named 'Name' with the names of each protein motifs
#' @return A pie chart in ggplot which displays the percentage distribution of each protein motifs
#'         in the `Name` column, with labels showing the protein motifs name and its corresponding percentage.
#'  @example 
#' data <- 'GFF file'
#' count <- name_counts(data)
#' piechart <- ggplot(name_counts, aes(x = "", y = count, fill = Name_and_procent)) +
#'              geom_bar(stat = "identity", width = 1) +
#'              coord_polar(theta = "y") +
#'              theme_minimal() +
#'              scale_fill_brewer(palette = "Set1") +    # Optional colour selection to be included  
#'              theme(axis.text.x = element_blank())
#'              
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar theme_minimal theme element_blank
#' @export             


freqPie <- function(data) {
  # Calculate the number of occurrences for each protein motifs in the ‘Name’ column
  name_counts <- data %>%
    group_by(Name) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100,  # Calculate the percentage
           Name_and_procent = paste0(Name, " (", round(percentage, 1), "%)", sep = ""))
  
  # Tworzenie wykresu kołowego
  pie_chart <- ggplot(name_counts, aes(x = "", y = count, fill = Name_and_procent)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_minimal() +
    # scale_fill_brewer(palette = "Set1") +    # Optional colour selection to be included
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.title = element_blank())
  
  return(pie_chart)
}


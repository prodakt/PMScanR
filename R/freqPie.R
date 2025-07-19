#' Create a pie chart showing protein motif distribution
#'
#' This function calculates the occurrences and percentages for each protein
#' motif in the 'Name' column of a GFF-like data frame. It then creates a
#' pie chart using `ggplot2` to visualize the distribution.
#'
#' @param data A data frame in GFF format containing a column named 'Name'
#'   with the names of each protein motif.
#' @return A ggplot object representing the pie chart.
#' @examples
#' # Create sample data frame similar to parsed GFF output
#' sample_data <- data.frame(
#'   seqid = rep(c("Seq1", "Seq2"), each = 5),
#'   source = rep("PROSITE", 10),
#'   type = rep("MOTIF", 10),
#'   start = sample(1:100, 10),
#'   end = sample(101:200, 10),
#'   score = runif(10),
#'   strand = sample(c("+", "-"), 10, replace = TRUE),
#'   phase = sample(0:2, 10, replace = TRUE),
#'   Name = sample(c("Zinc_finger", "EGF_domain", "Kinase_domain"), 10, replace = TRUE)
#' )
#'
#' # Generate the pie chart
#' motif_pie_chart <- freqPie(sample_data)
#' # print(motif_pie_chart)
#'
#' @importFrom dplyr count mutate arrange desc
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar theme_minimal theme element_blank
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
freqPie <- function(data) {
  # Calculate the number of occurrences for each protein motif
  name_counts <- data %>%
    count(.data$Name, name = "count") %>%
    mutate(
      percentage = .data$count / sum(.data$count) * 100,
      Name_and_procent = paste0(.data$Name, " (", round(.data$percentage, 1), "%)")
    ) %>%
    arrange(desc(.data$Name))

  pie_chart <- ggplot(name_counts, aes(x = "", y = .data$count, fill = .data$Name_and_procent)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = 0) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_blank()
    )

  return(pie_chart)
}

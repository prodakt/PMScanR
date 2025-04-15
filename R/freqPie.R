#' Create a pie chart from a GFF file which showing the distribution of protein motifs in the 'Name' column
#'
#' This function calculates the number of occurrences and percentages for each protein motifs in the `Name` column.
#' It then creates a pie chart using `ggplot2` to visualize the distribution of these protein motifs.
#'
#' @param data A file in GFF format containing a column named 'Name' with the names of each protein motifs
#' @return A pie chart in ggplot which displays the percentage distribution of each protein motifs
#'         in the `Name` column, with labels showing the protein motifs name and its corresponding percentage.
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
#'   # Ensure the 'Name' column exists and has some repetition
#'   Name = sample(c("Zinc_finger", "EGF_domain", "Kinase_domain"), 10, replace = TRUE)
#' )
#'
#' # Generate the pie chart using the function
#'   motif_pie_chart <- freqPie(sample_data)
#'   # To display the chart (usually not done directly in examples unless needed)
#'   # print(motif_pie_chart)
#'
#' @importFrom dplyr group_by summarise mutate n arrange desc
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar theme_minimal theme element_blank scale_fill_brewer
#' @importFrom rlang .data
#' @export


freqPie <- function(data) {
  # Calculate the number of occurrences for each protein motifs in the ‘Name’ column
  name_counts <- data %>%
    dplyr::count(.data$Name, name = "count") %>%
    dplyr::mutate(
      percentage = .data$count / sum(.data$count) * 100,
      Name_and_procent = paste0(.data$Name, " (", round(.data$percentage, 1), "%)")
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Name)) # Arrange factor levels for consistent plot order if needed

  pie_chart <- ggplot2::ggplot(name_counts, ggplot2::aes(x = "", y = .data$count, fill = .data$Name_and_procent)) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") + # Added white border for clarity
    ggplot2::coord_polar(theta = "y", start = 0) + # Start at 12 o'clock
    ggplot2::theme_minimal() +
    # Optional: Use a specific color palette
    # ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),       # Remove axis text
      axis.ticks = ggplot2::element_blank(),      # Remove axis ticks
      panel.grid = ggplot2::element_blank(),     # Remove grid lines
      legend.title = ggplot2::element_blank()     # Remove legend title
    )

  return(pie_chart)
}


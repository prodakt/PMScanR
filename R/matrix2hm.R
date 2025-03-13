#' Generate a heatmap from a matrix
#' 
#' This function generates a heatmap using the `plotly` package. 
#' The heatmap highlights specific rows and columns provided by the user, while the rest of the matrix is dimmed. 
#' The function also adds grid lines to the heatmap for better readability.
#' 
#' @param x A character vector specifying the columns to highlight in the heatmap
#' @param y A character vector specifying the rows to highlight in the heatmap
#' @param input A matrix containing the data to be visualized in the heatmap
#' @return A heatmap with highlighted specifed rows and columns
#' @example 
#' heatmap <- matrixtoheatmap(x = colnames('file'), y = row.names('file'), input = 'file')
#' heatmap
#' 
#' @importFrom dplyr mutate_all
#' @importFrom plotly plot_ly add_segments
#' @export


matrix2hm <- function(input, x = NULL, y = NULL) {
  if (is.null(x)) {
    x <- input
  }
  if (is.null(y)) {
    y <- input
  }
  mat_sel <- input %>% mutate_all(~ -1)
  mat_sel[rownames(mat_sel) %in% y,] <- input[rownames(input) %in% y,]
  mat_sel[,colnames(mat_sel) %in% x] <- input[,colnames(input) %in% x]
  hp <- plot_ly(z = ~as.matrix(mat_sel),
                x = ~colnames(mat_sel),
                y = ~rownames(mat_sel),
                type = "heatmap",
                colors = c("white", "#80274f", "#008caf"),
                showscale = F,
  )  %>%
    add_segments(
      x = ~colnames(mat_sel), xend = ~colnames(mat_sel),
      y = ~rownames(mat_sel)[1], yend = ~rownames(mat_sel)[nrow(mat_sel)],
      line = list(color = "black", width = 0.3)
    ) %>%
    add_segments(
      y = ~rownames(mat_sel), yend = ~rownames(mat_sel),
      x = ~colnames(mat_sel)[1], xend = ~colnames(mat_sel)[ncol(mat_sel)],
      line = list(color = "black", width = 0.3)
    )
  
  return(hp)
}
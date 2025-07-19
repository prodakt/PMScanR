<<<<<<< Updated upstream:R/matrix2hm_2.R
#' Generate a heatmap from a matrix with layout which provides a square shape of heatmap
#' 
#' This function generates a heatmap using the `plotly` package. 
#' The heatmap highlights specific rows and columns provided by the user, while the rest of the matrix is dimmed. 
#' The function also adds grid lines to the heatmap for better readability.
#' 
#' @param x A character vector specufying the columns to highlight in the heatmap
#' @param y A character vector specifying the rows to highlight in the heatmap
#' @param input A matrix containing the data to be visualized in the heatmap
#' @return A heatmap with highlighted specifed rows and columns.
#'          Grid lines are added for better visualization, and the axes are properly scaled.
#' @example
#' heatmap <- matrixtoheatmap(x = colnames('file'), y = row.names('file'), input = 'file')
#' heatmap
#' 
#' @importFrom dplyr mutate_all
=======
#' Generate a square heatmap from a matrix
#'
#' This function generates a heatmap using `plotly`, ensuring the plot has a
#' square aspect ratio. It highlights user-specified rows and columns.
#'
#' @param input A matrix containing the data to be visualized.
#' @param x A character vector specifying the columns to highlight.
#' @param y A character vector specifying the rows to highlight.
#' @return A plotly heatmap object with a square layout.
#' @examples
#' # Create a sample matrix
#' mat <- matrix(c(1, 0, 1, 0), 2, 2)
#' colnames(mat) <- c("Col1", "Col2")
#' rownames(mat) <- c("Row1", "Row2")
#' sq_heatmap <- matrixToSquareHeatmap(input = mat, x = "Col1", y = "Row1")
#' # To display in an interactive session:
#' # sq_heatmap
#' @importFrom dplyr mutate across everything
>>>>>>> Stashed changes:R/matrixToSquareHeatmap.R
#' @importFrom plotly plot_ly add_segments layout
#' @importFrom magrittr %>%
#' @export
<<<<<<< Updated upstream:R/matrix2hm_2.R


matrix2hm_2 <- function(input, x, y) {
=======
matrixToSquareHeatmap <- function(input, x = NULL, y = NULL) {
>>>>>>> Stashed changes:R/matrixToSquareHeatmap.R
  if (is.null(x)) {
    x <- input
  }
  if (is.null(y)) {
    y <- input
  }
<<<<<<< Updated upstream:R/matrix2hm_2.R
  mat_sel <- input %>% mutate_all(~ -1)
  mat_sel[rownames(mat_sel) %in% y,] <- input[rownames(input) %in% y,]
  mat_sel[,colnames(mat_sel) %in% x] <- input[,colnames(input) %in% x]
  
  heatmap_plot <- plot_ly(z = ~as.matrix(mat_sel),
                          x = ~colnames(mat_sel),
                          y = ~rownames(mat_sel),
                          type = "heatmap",
                          colors = c("white", "#80274f", "#008caf"),
                          showscale = FALSE
=======
  # Ensure input is a matrix
  mat_sel <- as.data.frame(input) %>%
    mutate(across(.cols = everything(), .fns = ~ -1))
  mat_sel <- as.matrix(mat_sel)
  mat_sel[rownames(mat_sel) %in% y, ] <- input[rownames(input) %in% y, ]
  mat_sel[, colnames(mat_sel) %in% x] <- input[, colnames(input) %in% x]

  # Calculate height based on number of rows
  n_rows <- nrow(mat_sel)
  plot_height <- max(500, n_rows * 20)

  # Create heatmap with layout options for a square plot
  heatmap_plot <- plot_ly(
    z = mat_sel,
    x = colnames(mat_sel),
    y = rownames(mat_sel),
    type = "heatmap",
    colors = c("white", "#80274f", "#008caf"),
    showscale = FALSE,
    height = plot_height
>>>>>>> Stashed changes:R/matrixToSquareHeatmap.R
  ) %>%
    layout(
      xaxis = list(scaleanchor = "y"),
      yaxis = list(scaleanchor = "x")
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
  
  return(heatmap_plot)
}
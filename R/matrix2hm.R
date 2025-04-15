#' Generate a heatmap from a matrix
#'
#' This function generates a heatmap using the `plotly` package.
#' The heatmap highlights specific rows and columns provided by the user, while the rest of the matrix is dimmed.
#' The function also adds grid lines to the heatmap for better readability.
#'
#' @param input A matrix containing the data to be visualized in the heatmap.
#' @param x A character vector specifying the columns to highlight in the heatmap.
#' @param y A character vector specifying the rows to highlight in the heatmap.
#' @return A plotly heatmap with highlighted specified rows and columns.
#' @examples
#' # Create a sample matrix with row and column names
#' mat <- matrix(c(1, 0, 1, 0), 2, 2)
#' colnames(mat) <- c("Col1", "Col2")
#' rownames(mat) <- c("Row1", "Row2")
#' heatmap <- matrix2hm(input = mat, x = "Col1", y = "Row1")
#' heatmap
#' @import reshape2
#' @importFrom dplyr mutate across everything
#' @importFrom plotly plot_ly add_segments layout
#' @export
matrix2hm <- function(input, x = NULL, y = NULL) {
  # Ensure input has row and column names
  if (is.null(colnames(input))) {
    colnames(input) <- paste0("Col", seq_len(ncol(input)))
  }
  if (is.null(rownames(input))) {
    rownames(input) <- paste0("Row", seq_len(nrow(input)))
  }

  # Set x and y to all columns/rows if NULL
  if (is.null(x)) {
    x <- colnames(input)
  }
  if (is.null(y)) {
    y <- rownames(input)
  }

  # Ensure input is a matrix
  mat_sel <- as.data.frame(input) %>% mutate(across(.cols = everything(), .fns = ~ -1))
  mat_sel <- as.matrix(mat_sel)
  mat_sel[rownames(mat_sel) %in% y,] <- input[rownames(input) %in% y,]
  mat_sel[,colnames(mat_sel) %in% x] <- input[,colnames(input) %in% x]

  # Calculate height based on number of rows
  n_rows <- nrow(mat_sel)
  plot_height <- max(500, n_rows * 20)  # At least 500px, or 20px per row

  # Create heatmap with height specified in plot_ly
  hp <- plot_ly(
    z = mat_sel,
    x = colnames(mat_sel),
    y = rownames(mat_sel),
    type = "heatmap",
    colors = c("white", "#80274f", "#008caf"),
    showscale = FALSE,
    height = plot_height
  )

  # Add vertical grid lines (one for each column)
  for (col in colnames(mat_sel)) {
    hp <- hp %>% add_segments(
      x = col, xend = col,
      y = rownames(mat_sel)[1], yend = rownames(mat_sel)[n_rows],
      line = list(color = "black", width = 0.3),
      inherit = FALSE
    )
  }

  # Add horizontal grid lines (one for each row)
  for (row in rownames(mat_sel)) {
    hp <- hp %>% add_segments(
      x = colnames(mat_sel)[1], xend = colnames(mat_sel)[ncol(mat_sel)],
      y = row, yend = row,
      line = list(color = "black", width = 0.3),
      inherit = FALSE
    )
  }

  # Add layout adjustments
  hp <- hp %>% layout(
    yaxis = list(
      tickfont = list(size = 10),
      tickangle = 45,
      automargin = TRUE
    ),
    xaxis = list(
      tickfont = list(size = 10),
      tickangle = 45,
      automargin = TRUE
    ),
    margin = list(l = 150, r = 50, t = 50, b = 100)
  )

  return(hp)
}

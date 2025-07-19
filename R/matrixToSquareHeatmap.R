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
#' @importFrom plotly plot_ly add_segments layout
#' @importFrom magrittr %>%
#' @export
matrixToSquareHeatmap <- function(input, x = NULL, y = NULL) {
  if (is.null(x)) {
    x <- colnames(input)
  }
  if (is.null(y)) {
    y <- rownames(input)
  }
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
  ) %>%
    layout(
      xaxis = list(
        scaleanchor = "y",
        tickfont = list(size = 10),
        tickangle = 45,
        automargin = TRUE
      ),
      yaxis = list(
        scaleanchor = "x",
        tickfont = list(size = 10),
        tickangle = 45,
        automargin = TRUE
      ),
      margin = list(l = 150, r = 50, t = 50, b = 100)
    ) %>%
    add_segments(
      x = colnames(mat_sel), xend = colnames(mat_sel),
      y = rownames(mat_sel)[1], yend = rownames(mat_sel)[nrow(mat_sel)],
      line = list(color = "black", width = 0.3),
      inherit = FALSE
    ) %>%
    add_segments(
      y = rownames(mat_sel), yend = rownames(mat_sel),
      x = colnames(mat_sel)[1], xend = colnames(mat_sel)[ncol(mat_sel)],
      line = list(color = "black", width = 0.3),
      inherit = FALSE
    )

  return(heatmap_plot)
}

#' Plot Plates
#'
#' @param Data A data frame containing the well locations and the values to fill
#' @param fill The column name of the value to fill consider wrapping in double brackets and quotes
#' @param wellID The column name of the character column of well locations
#' @param facetrows The column name of the character column selected for the row facet
#' @param facetcols The column name of the character column selected for the column facet
#' @param plate The size of the plate
#' @param size The size of the plotted data on the plate
#' @param shape The shape of each plotted data point on the plate
#' @param na_fill Fill color for the plate
#' @param na_size_ratio Background ratio
#' @param na_alpha Background transparency
#'
#' @return An image object
#' @export

library(ggplot2)

plot_plate <- function(data,
                            fill,
                            well_id,
                            facet_rows = NULL,
                            facet_cols = NULL,
                            plate = 384,
                            size = 5,
                            shape = 22,
                            na_fill = "white",
                            na_size_ratio = 0.95,
                            na_alpha = 0.1) {

  plate_info <- platesize_check(plate)

  xlim <- c(0.5, plate_info$colmax + 0.5)
  ylim <- c(plate_info$rowmax + 0.5, 0.5)

  plotting_data <- to_rows_columns(data, well_id)

  # Bind fill data into the dataframe for plotting, as a new column
  plotting_data$fill <- as.factor(data[[fill]])

  p <- ggplot(plotting_data, aes(x = Column, y = Row, fill = fill)) +
    geom_point(colour = "gray20", shape = shape, size = size) +
    geom_point(data = expand.grid(Column = seq(plate_info$colmax), Row = seq(1, plate_info$rowmax)),
               aes(x = Column, y = Row), color = "grey90", fill = na_fill,
               shape = shape, size = size * na_size_ratio, alpha = na_alpha) +
    coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
    scale_y_reverse(breaks = seq(1, plate_info$rowmax), labels = LETTERS[1:plate_info$rowmax]) +
    scale_x_continuous(breaks = seq(1, plate_info$colmax), position = "top")

  # Optional faceting
  if (!is.null(facet_rows)) {
    if (!is.null(facet_cols)) {
      p <- p + facet_grid(rows = vars(.data[[facet_rows]]), cols = vars(.data[[facet_cols]]), labeller = label_both)
    } else {
      p <- p + facet_grid(rows = vars(.data[[facet_rows]]))
    }
  } else if (!is.null(facet_cols)) {
    p <- p + facet_grid(cols = vars(.data[[facet_cols]]))
  }

  return(p)
}

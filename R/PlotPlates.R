#' Plot Plate
#'
#' @param Data A data frame containing the well locations and the values to fill
#' @param fill The column name of the value to fill consider wrapping in double brackets and quotes
#' @param wellID The column name of the character column of well locations
#' @param plate The size of the plate
#' @param size The size of the plotted data on the plate
#' @param shape The shape of each plotted data point on the plate
#' @param na_fill Fill color for the plate
#' @param na_size_ratio Background ratio
#' @param na_alpha Background transparency
#'
#' @return An image object
#' @export
plotPlate <- function(Data,
                       fill = fill,
                       wellID = wellID,
                       plate = 384,
                       size = 5,
                       shape = 22,
                       na_fill = "white",
                       na_size_ratio = 0.95,
                       na_alpha = 0.1){

  mtpR::platesize.check(plate) -> plate.size

  plate.size$rows -> rows
  plate.size$columns -> columns
  plate.size$rowmax -> rowmax
  plate.size$colmax -> colmax

  rbound <- as.numeric(rowmax) + 0.5
  cbound <- as.numeric(colmax) + 0.5

  xlim <- c(0.5, cbound)
  ylim <- c(rbound, 0.5)


  plotting.data <- Data %>%
    mtpR::to.rowsandcolumns(wellID = {{wellID}})


  if (max(plotting.data$Row) > rowmax | max(plotting.data$Column) > colmax) {
    print("Wells greater than plate size, please select appropriate plate size")
    return()
  }

  p <- plotting.data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "Column", y = "Row")) +
    ggplot2::geom_point(ggplot2::aes_string(fill = fill),
                        colour = "gray20",
                        shape = shape,
                        size = size) +
    ggplot2::geom_point(data = expand.grid(seq(colmax), seq(1, rowmax)),
                        ggplot2::aes_string(x = "Var1", y = "Var2"),
                        color = "grey90",
                        fill = na_fill,
                        shape = shape,
                        size = size * na_size_ratio,
                        alpha = na_alpha) +
    ggplot2::coord_fixed(ratio = (cbound / colmax) / (rbound / rowmax), xlim = xlim, ylim = ylim) +
    ggplot2::scale_y_reverse(breaks = seq(1, rowmax), labels = LETTERS[1:rowmax]) +
    ggplot2::scale_x_continuous(position = "top", breaks = seq(1, colmax))

  return(p)
}


#' Convert to Row and Column indices
#'
#' @param Wells The dataframe containing the wells to be converted
#' @param wellID The ColName of the column containing the character values to be converted
#'
#' @return Two columns added to the dataframe for the row and column indices
#' @export
#'
to.rowsandcolumns <- function(Wells,
                              wellID){

  well.Var <- rlang::enquo(wellID)

  Wells.RC <- Wells %>%
    dplyr::mutate(Row = as.numeric(match(toupper(substr(!!(well.Var), 1,1)), LETTERS)),
           Column = as.numeric(substr(!!(well.Var), 2, 5))
    )


  return(Wells.RC)

}

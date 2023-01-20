#' Plot Plates
#'
#' @param Data A data frame containing the well locations and the values to fill
#' @param fill The column name of the value to fill consider wrapping in double brackets and quotes
#' @param wellID The column name of the character column of well locations
#' @param fillwise The direction the plate is filled by to calculate the plate index
#' @param plate The size of the plate
#' @param size The size of the plotted data on the plate
#' @param shape The shape of each plotted data point on the plate
#' @param na_fill Fill color for the plate
#' @param na_size_ratio Background ratio
#' @param na_alpha Background transparency
#'
#' @return An image object
#' @export

plotPlate_multi <- function(Reactions,
                            fill = fill,
                            wellID = wellID,
                            fillwise = "row",
                            # facet,
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


  plotting.data <- Reactions |>
    mutate(num = mtpR::to.num(well = {{wellID}},
                              wise = fillwise,
                              plate = plate)) |>
    mtpR::addPlates(Well.Index.ColumnID = num,
                    Plate.Size = plate) |>
    mtpR::toRowsColumns(wellID = {{wellID}})


p <- plotting.data |>
  ggplot2::ggplot(aes_string(x = "Column", y = "Row")) +
  geom_point(aes_string(fill = fill), colour = "gray20", shape = shape, size = size) +
  geom_point(data = expand.grid(seq(colmax), seq(1, rowmax)),
             aes_string(x = "Var1", y = "Var2"),
             color = "grey90", fill = na_fill, shape = shape, size = size * na_size_ratio, alpha = na_alpha) +
  coord_fixed(ratio = (cbound / colmax) / (rbound / rowmax), xlim = xlim, ylim = ylim) +
  scale_y_reverse(breaks = seq(1, rowmax), labels = LETTERS[1:rowmax]) +
  scale_x_continuous(position = "top", breaks = seq(1, colmax)) +
  facet_grid(rows = vars(Plate.Index),
             labeller = label_both)

return(p)
}

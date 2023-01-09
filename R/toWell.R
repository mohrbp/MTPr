#' To Well
#'
#' @param num Numeric - a number less than the plate size representing the location on the well plate
#' @param wise "row" or "col" - Indicates the direction the plate is read, across (L->R) a row or down a column
#' @param plate The size of the well plate (96 and 384 are supported)
#'
#' @return A character vector representing the row location with a letter and the column location with a number
#' @export
#'
to.well <- function(num, wise, plate = 96){

  stopifnot(is.numeric(num))
  num.check <- num |>
    stringr::str_subset("NA", negate = TRUE)
  if (as.numeric(max((num.check))) > plate) {
    print(paste("Number of wells ", as.numeric(max((num.check)))))
    stop("num cannot be greater than the number of wells in the plate",
         call. = FALSE)
  }

  mtpR::platesize.check(plate) -> plate.size

  plate.size$rows -> rows
  plate.size$columns -> columns
  plate.size$rowmax -> rowmax
  plate.size$colmax -> colmax

  num.df <- data.frame(num = num)

  if (wise == "row" | wise == "col") {

    num.df |>
      dplyr::mutate(n_col = dplyr::case_when(wise == "row" ~ (((num-1)%%colmax) + 1),
                                             wise == "col" ~ (((num-1)%/%rowmax) + 1)),
             n_row = dplyr::case_when(wise == "row" ~ (((num-1)%/%colmax) + 1),
                                      wise == "col" ~ (((num-1)%%rowmax) + 1))
      ) |>
      dplyr::mutate(pos_col = columns[n_col],
             pos_row = rows[n_row]) |>
      dplyr::group_by(num) |>
      dplyr::mutate(well = paste0(pos_row, pos_col)) |>
      dplyr::pull(well) -> well

    return (well)

  } else {
    print("Please enter \`col\` or \`row\` for wise")
    return()
  }
}

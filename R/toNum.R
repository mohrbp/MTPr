#' to.num
#'
#' @param well A character vector of well locations to be converted to numbers
#' @param wise "row" or "col" - Indicates the direction the plate is read, across (L->R) a row or down a column
#' @param plate The size of the well plate (96 and 384 are supported)
#'
#' @return A numeric vector of the well locations on the plate
#' @export
#'
to.num <- function(well, wise, plate = 96){


  MTPtools::platesize.check(plate) -> plate.size

  plate.size$rows -> rows
  plate.size$columns -> columns
  plate.size$rowmax -> rowmax
  plate.size$colmax -> colmax

  well.df <- data.frame(wellname = well)

  if (wise == "row" | wise == "col") {

    well.df <- well.df %>%
      dplyr::mutate(Row = as.numeric(match(toupper(substr((wellname), 1,1)), LETTERS)),
                    Column = as.numeric(substr((wellname), 2, 5))
      )

    if (max(!is.na(well.df$Row)) > rowmax | max(!is.na(well.df$Column)) > colmax) {
      print("Wells greater than plate size, please select appropriate plate size")
      return()
    }

    well.df %>%
      dplyr::mutate(num = dplyr::case_when(wise == "row" ~ (((Row-1) * colmax) + Column),
                                           wise == "col" ~ (((Column - 1) * rowmax) + Row))) %>%
      dplyr::pull(num) -> num

  } else {
    print("Please enter \`col\` or \`row\` for wise")
    return()
  }

  return(num)

}

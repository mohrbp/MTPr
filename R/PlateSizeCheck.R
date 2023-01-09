#' Check the entered plate size is allowable
#'
#' @param plate Numeric, the number of wells in the plate
#'
#' @return a list containing the row ids, column ids and max number for both rows and columns
#' @export
#'
platesize.check <-function(plate) {

  if (plate == 6L) {
    rowmax <- 2
    colmax <- 3
    rows <- LETTERS[1:rowmax]
    columns <- 1:colmax

  } else if (plate == 12L) {
    rowmax <- 3
    colmax <- 4
    rows <- LETTERS[1:rowmax]
    columns <- 1:colmax

  } else if (plate == 24L) {
    rowmax <- 4
    colmax <- 6
    rows <- LETTERS[1:rowmax]
    columns <- 1:colmax

  } else if (plate == 48L) {
    rowmax <- 6
    colmax <- 8
    rows <- LETTERS[1:rowmax]
    columns <- 1:colmax

  } else if (plate == 96L){
    rowmax <- 8
    colmax <- 12
    rows <- LETTERS[1:rowmax]
    columns <- 1:colmax

  } else if (plate == 384L){
    rowmax <- 16
    colmax <- 24
    rows <- LETTERS[1:rowmax]
    columns <- 1:colmax

  } else if (plate == 1536L){
    rowmax <- 32
    colmax <- 48
    first_26 <- LETTERS[1:26]
    last_6 <- vector(length = 6)
    for (i in 1:6){
      last_6[i] <- paste(LETTERS[rep(i, 2)],
                         collapse = "")
    }
    rows <- c(first_26, last_6)
    columns <- 1:colmax

  } else stop("Plate needs to be 96, 384 or 1536")

  plate.size <- list("rows" = rows,
                     "columns" = columns,
                     "rowmax" = rowmax,
                     "colmax" = colmax)

  return(plate.size)

}

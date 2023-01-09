#' Plot Plates
#'
#' @param dat A data frame containing the well index
#' @param Well.Index.ColumnID The column name of well index
#' @param Plate.Size The size of plate for this set of wells
#'
#' @return An dataframe where with an additional column Plate.Index and Well.Index <= Plate.Size
#' @export

addPlates <- function(dat,
                      Well.Index.ColumnID,
                      Plate.Size
) {

  dat.new <- dat %>%
    mutate(Plate.Index = ceiling({{Well.Index.ColumnID}} / Plate.Size),
           Well.Index.New = {{Well.Index.ColumnID}} - Plate.Size * (Plate.Index - 1),
           Well.Index = Well.Index.New) %>%
    select(-c(Well.Index.New))

  return(dat.new)

}

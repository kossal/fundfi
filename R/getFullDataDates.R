#' Eliminates empty dates columns
#'
#' Eliminates dates columns that are half empty
#' @param st data.frame returned by getStatement
#' @return data.frame with dates columns that are half full of data
#' @keywords format dates
#' @export
#' @import dplyr
getFullDataDates <- function(st) {

  statement <- st

  datesCol <- !is.na(as.Date(names(statement), format="%Y-%m-%d"))

  fullDates <- sapply(statement[,datesCol], function(x) {
    length(x[is.na(x) == TRUE]) / length(x) < .5
  })

  for (date in names(fullDates)) {
    if (fullDates[[date]] == FALSE) {
      statement <- dplyr::select(statement, !! -(date))
    }
  }

  invisible(statement)

}

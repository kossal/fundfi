#' Drops empty dates columns
#'
#' Drops half empty columns dates.
#'
#' Any column which the name can be transformed to
#' a date with as.Date(format="%Y-%m-%d") will be
#' evaluated, if more then half or equal of the values of
#' the date column is NA, it will be droped. It will return
#' a data.frame and wont modify the original.
#' @param st data.frame
#' @return data.frame without half empty date columns
#' @keywords format dates
#' @export
dropEmptyDates <- function(st) {

  statement <- st

  datesCol <- !is.na(as.Date(colnames(statement), format="%Y-%m-%d"))
  normalCol <- !datesCol

  datesCol[datesCol] <- sapply(statement[,datesCol], function(x) {
    length(x[is.na(x) == TRUE]) / length(x) < .5
  })

  invisible(statement[,normalCol | datesCol])

}

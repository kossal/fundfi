#' Print financial statement
#'
#' This function prints a financial statement returned by
#' getStatement. It can print all dates reported or only those
#' tha are 50% or more full.
#' @param statement data.frame returned by getStatement
#' @param all.Dates Defaults to FALSE, will only print dates that have more
#' then 50 % of data. TRUE will print them all.
#' @return Prints to console or to .md the financial statement
#' @keywords Print financial statement
#' @export
#' @examples
#' printStatement(getStatement(allXBRLs$PG, statement = "income_statement"))
#' @import pander
printStatement <- function(statement, all.Dates=FALSE) {

  datesCol <- !is.na(as.Date(names(statement), format="%Y-%m-%d"))
  numDates <- length(datesCol[datesCol == TRUE]) + 1

  if (!all.Dates) {

    temp <- sapply(statement[,datesCol], function(x) {
      length(is.na(x)[is.na(x) == TRUE]) / length(x) < .5
    })
    haveMuchData <- unname(unlist(temp))

    cols <- c((datesCol == FALSE)[ 1:(length(datesCol)-length(haveMuchData)) ], haveMuchData)

    statement <- statement[,cols]

    numDates <- ncol(statement) - length(haveMuchData[haveMuchData == TRUE]) + 1

  }

  pander::pandoc.table(
    statement[, c((numDates-2):ncol(statement))],
    style = "rmarkdown",
    justify = "left",
    split.table = 300,
    header = c("Concept", "Balance", names(statement)[numDates:ncol(statement)]),
    emphasize.strong.rows = which(statement$level == 1)
  )

}

#' Print financial statement
#'
#' This function prints a financial statement returned by
#' getStatement.
#' @param st data.frame returned by getStatement
#' @return Prints to console or to .md the financial statement
#' @keywords Print financial statement
#' @export
#' @examples
#' printStatement(getStatement(allXBRLs$PG, statement = "income_statement"))
#' @import pander
#' @import dplyr
printStatement <- function(st) {

  statement <- st %>%
    select(level, element:ncol(st)) %>%
    rename(Concepto = element, Balance = balance)

  pander::pandoc.table(
    statement[,2:ncol(statement)],
    style = "rmarkdown",
    justify = "left",
    split.table = 300,
    emphasize.strong.rows = which(statement$level == 1)
  )

}

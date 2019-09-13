#' Create finantial reports from tickers
#'
#' Fetches the XBRLs of the tickers specified from the SEC's EDGAR,
#' and creates finantial reports.
#'
#' This function will fetch the XBRL documents of
#' the specified tickers, download them and format them
#' to create balance sheet, income statement and cash flow.
#' It will return a list of lists, each on of them
#' corresponding to the ticker name and as his elements,
#' data.frames containing the finantial report.
#' There is also the option to save each ticker list
#' as a .rds in a folder using the statements.dir
#' parameter. For network performance, XBRL taxonomies
#' and XBRL list are cached by this behaviour can
#' be modified.
#' @param tickers A ticker character value or vector
#' @param type Type of filling like 10-Q or 10-K
#' @param force.new Defaults to FALSE. If TRUE, ignores rds cache.
#' @param xbrl.cache Folder where XBRL taxonomies are saved.
#' If null then it wont save cache.
#' @param xbrl.rds.cache Folder where XBRL lists are saved as .rds
#' If null, it wont save cache.
#' @param drop.empty.dates Defaults to FALSE. Bolean that indicates
#' if it should keep or drop half empty date columns in statements.
#' @param statements.dir Defaults to NULL. If a string is passed,
#' each ticker list of statement will be saved as a .rds in the
#' folder whith the name specified.
#' @return List that contains the balance sheet, income statement and
#' cash flow of each ticker. The names of the list are the tickers.
#' @export
#' @examples
#' getXBRL(c("AAPL", "TSLA", "GE"), type = "10-Q")
fundfiDoAll <- function(tickers,
                        type = "10-Q",
                        force.new = FALSE,
                        xbrl.cache = "xbrl.Cache",
                        xbrl.rds.cache = "xbrl.rds.Cache",
                        drop.empty.dates = FALSE,
                        statements.dir = FALSE) {

  allXBRLs <- getXBRL(tickers)

  # Test all tickers statements
  statNames <- c("balance_sheet", "income_statement", "cash_flow")
  statements <- list()
  for (ticker in tickers) {

    tickerStatements <- list()

    for (statName in statNames) {
      tickerStatements[[statName]] <- tryCatch({
        getStatement(allXBRLs[[ticker]], statement = statName)
      },
      error=function(cond){return(NA)})
    }

    statements[[ticker]] <- tickerStatements

  }

  # Drop empty date cols
  for (statement in names(statements)) {
    for (statName in statNames) {
      statements[[statement]][[statName]] <- dropEmptyDates(statements[[statement]][[statName]])
    }
  }

  # Save all statements
  for (statement in names(statements)) {
    saveRDS(data, file = paste0("statements/", statement, "-statement.rds"))
  }

}

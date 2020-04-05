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
#' and XBRL list are cached but this behaviour can
#' be modified.
#' @param tickers A ticker character value or vector
#' @param type Type of filling like 10-Q or 10-K
#' @param count Number of maximum filings. If until parameter is set, the
#' number of filings returned could be less.
#' @param until yyyymmdd fillings are downloaded starting from the until
#' parameter until the oldest, or until it reaches the count limit.
#' @param drop.empty.dates Defaults to FALSE. Bolean that indicates
#' if it should keep or drop half empty date columns in statements.
#' @param keep.xbrl Defaults to FALSE. If TRUE, the list of XBRL will also
#' be returned as an element of the list returned called original.xbrl .
#' @param statements.dir Defaults to NULL. If a string is passed,
#' each ticker list of statement will be saved as a .rds in the
#' folder with the name specified.
#' @param force.new Defaults to FALSE. If TRUE, ignores XBRL rds cache.
#' @param xbrl.cache Folder where XBRL taxonomies are saved.
#' If null then it wont save cache.
#' @param xbrl.rds.cache Folder where XBRL lists are saved as .rds
#' If null, it wont save cache.

#' @return List that contains the balance sheet, income statement and
#' cash flow of each ticker. The names of the list are the tickers.
#' @export
#' @examples
#' getXBRL(c("AAPL", "TSLA", "GE"), type = "10-Q")
fundfiDoAll <- function(tickers,
                        type = "10-Q",
                        count = 1,
                        until = NULL,
                        drop.empty.dates = FALSE,
                        keep.xbrl = FALSE,
                        statements.dir = FALSE,
                        force.new = FALSE,
                        xbrl.cache = "xbrl.Cache",
                        xbrl.rds.cache = "xbrl.rds.Cache") {

  allXBRLs <- getXBRL(tickers = tickers,
                      type = type,
                      count = count,
                      until = until,
                      force.new = force.new,
                      xbrl.cache = xbrl.cache,
                      xbrl.rds.cache = xbrl.rds.cache)


  statNames <- c("balance_sheet", "income_statement", "cash_flow")

  allStat <- lapply(allXBRLs, function(tickerXBRLs) {

    statsPerStat <- lapply(statNames, function(statName) {

      tempStatements <- lapply(tickerXBRLs, function(xbrl) {

        statement <- tryCatch({

          # TODO implement custom role id description
          temp <- getStatement(xbrl, statement = statName)

          if (drop.empty.dates) {
            temp <- dropEmptyDates(temp)
          }

          temp

        },
        error=function(e) return(NA)
        )

        return(statement)

      })

      # Names of individual xbrl to each statement
      names(tempStatements) <- names(tickerXBRLs)

      return(tempStatements)

    })

    names(statsPerStat) <- statNames

    return(statsPerStat)

  })

  names(allStat) <- names(allXBRLs)

  # # Loop all tickers
  # for (ticker in names(allXBRLs)) {
  #
  #   tickerStatements <- list()
  #
  #   # Loop all statements
  #   for (statName in statNames) {
  #
  #     statStatement <- list()
  #
  #     # Loop all xbrl
  #     for () {
  #
  #       tickerStatements[[statName]] <- tryCatch({
  #         getStatement(allXBRLs[[ticker]], statement = statName)
  #       },
  #       error=function(cond) return(NA)
  #       )
  #
  #     }
  #
  #   }
  #
  #   allStat[[ticker]] <- tickerStatements
  #
  # }

  # Save all statements
  # for (statement in names(statements)) {
  #   saveRDS(data, file = paste0("statements/", statement, "-statement.rds"))
  # }

  # Save allXBRLs if user indicate
  if (keep.xbrl) {
    allStat[["original.xbrl"]] <- allXBRLs
  }

  invisible(allStat)

}

#' Get a list of XBRL
#'
#' This function will fetch the XBRL documents of
#' the specified tickers. It will return a list of
#' XBRL list.
#' @param tickers A ticker character value or vector
#' @return List with ticker names which have lists of data.frames, formated as XBRL
#' @keywords XBRL
#' @export
#' @examples
#' getXBRL(c("AAPL", "TSLA", "GE"), type = "10-Q")
#' @import edgarWebR XBRL digest
getXBRL <- function(tickers = NULL, type = "10-Q", new=FALSE) {

  if (is.null(tickers)) {
    stop("No tickers were specified")
  }

  allXBRLs <- list()

  # Check if data is on cache
  hash.file <- paste0(digest(paste0(tickers), "md5", serialize = FALSE), "-xbrl.rds")
  if (!file.exists(hash.file)) {

    for (ticker in tickers) {

      # Returns last fillings, type functions as start with
      fillings <- edgarWebR::company_filings(ticker, type = type, count = 10)

      # Filter by type. If not, 10-KA would also work even if 10-K was specified
      fillings <- fillings[fillings$type == type,]

      # Get last href filling
      fillHref <- fillings$href[[1]]

      # Get all documents of that filling
      documents <- edgarWebR::filing_documents(fillHref)

      # XBRL instances have diferent names depending on the company
      # Loop through common names and try to find href to the document
      xbrlInstanceNames <- c("XBRL INSTANCE DOCUMENT", "EXTRACTED XBRL INSTANCE DOCUMENT")
      for (name in xbrlInstanceNames) {
        xbrlHref <- documents[documents$description == name,]$href

        if (length(xbrlHref) > 0) {
          break
        }
      }
      if (length(xbrlHref) == 0) {
        stop(paste("XBRL instance filename of", ticker, "not found.", documents$description))
      }

      # Fetch XBRL
      old_o <- options(stringsAsFactors = FALSE)
      allXBRLs[[ticker]] <- XBRL::xbrlDoAll(xbrlHref, verbose = TRUE)
      options(old_o)

    }

    saveRDS(allXBRLs, file = hash.file)
    print(paste("Saved data to file cache:", hash.file))

  } else {

    allXBRLs <- readRDS(file = hash.file)
    print(paste("Reading from file cache:", hash.file))

  }

  invisible(allXBRLs)

}

#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'










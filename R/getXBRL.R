#' Fetch a list of XBRLs
#'
#' Fetches the XBRLs of the tickers specified from the SEC's EDGAR.
#'
#' This function will fetch the XBRL documents of
#' the specified tickers. It will return a list of
#' XBRL list. Each XBRL list contains the data.frames
#' returned by the library XBRL. Each ticker XBRL from
#' a data are cached as a .rds in the folder xbrl.rds.Cache.
#' All XBRL taxonomies are cached in the folder xbrl.Cache.
#' These behaviors can be modified using parameters.
#' If any ticker do not exist, it will be dropped. It
#' is reccomended to check if the length of the tickers
#' is equal to the returned list.
#' @param tickers A ticker character value or vector
#' @param type Defaults to 10-Q. Type of filling like 10-Q or 10-K
#' @param force.new Defaults to FALSE. If TRUE, ignores rds cache.
#' @param xbrl.cache Folder where XBRL taxonomies are saved.
#' If null then it wont save cache.
#' @param xbrl.rds.cache Folder where XBRL lists are saved as .rds
#' If null, it wont save cache.
#' @return List with ticker names which have lists of data.frames, formated as XBRL
#' @keywords XBRL
#' @export
#' @examples
#' getXBRL(c("AAPL", "TSLA", "GE"), type = "10-Q")
getXBRL <- function(tickers = NULL,
                    type = "10-Q",
                    force.new = FALSE,
                    xbrl.cache = "xbrl.Cache",
                    xbrl.rds.cache = "xbrl.rds.Cache") {

  if (is.null(tickers)) {
    stop("No tickers were specified")
  }

  allXBRLs <- list()

  # No string as factor
  old_o <- options(stringsAsFactors = FALSE)

  # Loop all tickers and get XBRL
  # hash.file <- paste0(digest::digest(paste0(tickers), "md5", serialize = FALSE), "-xbrl.rds")
  for (ticker in tickers) {

    allXBRLs[[ticker]] <- tryCatch({

      tickerXBRL <- list()

      # Returns last fillings, type functions as start with
      fillings <- edgarWebR::company_filings(ticker, type = type, count = 10)

      if (nrow(fillings) == 0) {
        stop(paste("Could not find any filing information for", ticker, "using type", type))
      }

      # Filter by type. If not, 10-KA would also work even if 10-K was specified
      fillings <- fillings[fillings$type == type,]

      # Check cache if force.new is FALSE
      if (!force.new) {

        ticker.file.name <- file.path(xbrl.rds.cache,
                                      paste0(
                                        ticker, "-",
                                        type, "-",
                                        fillings$filing_date[[1]], ".rds"
                                        )
                                      )

        if (file.exists(ticker.file.name)) {
          tickerXBRL <- readRDS(ticker.file.name)
          print(paste("Using", ticker.file.name, "cache for ticker:", ticker))
        }

      }

      # If file was not found on cache then download it
      if (length(tickerXBRL) == 0) {

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

        tickerXBRL <- XBRL::xbrlDoAll(xbrlHref, verbose = TRUE, cache.dir = xbrl.cache)

        # Save to cache
        if (!is.null(xbrl.rds.cache)) {

          # Create rds cache directory if not exits
          if (!dir.exists(xbrl.rds.cache)) {
            dir.create(xbrl.rds.cache)
          }

          saveRDS(tickerXBRL, file = ticker.file.name)

        }

      }

      tickerXBRL

    },
    error=function(cond){
      print(cond)
      return(NA)
    })

  }

  options(old_o)

  # Inform succesfull tickers and drop unsuccesfull
  print("Succesfull downloads:")
  print(names(allXBRLs[!is.na(allXBRLs)]))

  print("Unsuccesfull downloads:")
  print(names(allXBRLs[is.na(allXBRLs)]))

  allXBRLs <- allXBRLs[!is.na(allXBRLs)]

  invisible(allXBRLs)

}

#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'










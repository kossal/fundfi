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
#' If any ticker doesn´t exist, it will be dropped. It´s
#' recomended to check if the length of the tickers
#' is equal to the returned list.
#' @param tickers A ticker character value or vector
#' @param type Defaults to 10-Q. Type of filling like 10-Q or 10-K
#' @param count Number of maximum filings. If until parameter is set, the
#' number of filings returned could be less.
#' @param until yyyymmdd fillings are downloaded starting from the until
#' parameter until the oldest, or until it reaches the count limit.
#' @param force.new Defaults to FALSE. If TRUE.
#' @param xbrl.cache Folder where XBRL taxonomies are saved.
#' If null then it wont save cache.
#' @param xbrl.rds.cache Folder where XBRL lists are saved as .rds
#' If null, it wont save cache.
#' @return List with ticker names which have lists of data.frames, formated
#' as XBRL
#' @keywords XBRL
#' @export
#' @examples
#' getXBRL(c("AAPL", "TSLA", "GE"), type = "10-Q")
getXBRL <- function(tickers = NULL,
                    type = "10-Q",
                    count = 1,
                    until = NULL,
                    force.new = FALSE,
                    xbrl.cache = "xbrl.Cache",
                    xbrl.rds.cache = "xbrl.rds.Cache") {

  if (is.null(tickers)) {
    stop("No tickers were specified")
  }

  # TODO
  # Implement until date checkup

  allXBRLs <- list()

  # No string as factor
  old_o <- options(stringsAsFactors = FALSE)

  # Loop all tickers and get XBRL
  # hash.file <- paste0(digest::digest(paste0(tickers), "md5", serialize = FALSE), "-xbrl.rds")
  for (ticker in tickers) {

    allXBRLs[[ticker]] <- tryCatch({

      print(paste("Starting:", ticker))

      # Type functions as "start with"
      fillings <- edgarWebR::company_filings(ticker, type = type, count = count, before = until)

      # Filter by type. If not, 10-KA would also work even if 10-K was specified
      fillings <-  fillings[fillings$type == type,]

      # Limit number of fillings using count
      if (nrow(fillings) > count) {
        fillings <- fillings[1:count,]
      }

      if (nrow(fillings) == 0) {
        stop(paste("Could not find any filing information for", ticker, "using type", type))
      }

      companyXBRL <- list()

      for (filling in 1:nrow(fillings)) {

        companyXBRL[[paste0(
          ticker, "-",
          type, "-",
          fillings$filing_date[[filling]]
        )]] <- tryCatch({

          tickerXBRL <- list()

          # Create cache file name
          ticker.file.name <- file.path(xbrl.rds.cache,
                                        paste0(
                                          ticker, "-",
                                          type, "-",
                                          fillings$filing_date[[filling]], ".rds"
                                        )
          )

          # Check cache if force.new is FALSE
          if (!force.new && file.exists(ticker.file.name)) {

            tickerXBRL <- readRDS(ticker.file.name)
            cat(paste0("\nUsing ", ticker.file.name, " cache for ticker: ", ticker, "\n"))

          }

          # If file was not found on cache then download it
          if (length(tickerXBRL) == 0) {

            # Get last href filling
            fillHref <- fillings$href[[filling]]

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
            tickerXBRL <- fundfi::xbrlDoAllFun(xbrlHref, verbose = TRUE, cache.dir = xbrl.cache)

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

      companyXBRL

    },
      error=function(cond){
        print(cond)
        return(NA)
      })

  }

  options(old_o)

  # TODO
  # Implement detailed success and failure messages with reason why

  # Complete failures
  if (length(allXBRLs[is.na(allXBRLs)]) != 0) {

    cat("\n\nUnsuccesfull downloads:\n")
    print(names(allXBRLs[is.na(allXBRLs)]))

    allXBRLs <- allXBRLs[!is.na(allXBRLs)]

  }

  # Single XBRL failure
  for (t in seq_along(allXBRLs[!is.na(allXBRLs)])) {

    if (length(allXBRLs[[t]][is.na(allXBRLs[[t]])]) != 0) {
      cat(paste0("\n\n",ticker, " failures:\n"))
      print(names(allXBRLs[[t]][is.na(allXBRLs[[t]])]))
    }

    allXBRLs[[t]] <- allXBRLs[[t]][!is.na(allXBRLs[[t]])]

  }

  invisible(allXBRLs)

}

#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

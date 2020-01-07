#' Function that fixes FixFileName
funXBRL <- function() {
  self <- list(element = NULL, role = NULL,
               label = NULL, presentation = NULL, definition = NULL, calculation = NULL,
               context = NULL, unit = NULL, fact = NULL, footnote = NULL)

  cache.dir <- NULL
  discovered.files <- NULL
  doc.inst <- NULL
  dname.inst <- NULL
  verbose <- FALSE
  inst.lnkb <- NULL

  fixFileName <- function(dname, file.name) {
    if (!(gsub("^(http|https|ftp)://.*$", "\\1", file.name) %in% c("http", "https", "ftp"))) {

      # Count "../" in file.name
      count <- 0
      tempFileName <- file.name
      while (substr(tempFileName, 1, 3) == "../") {
        tempFileName <- substr(tempFileName, 4, nchar(tempFileName))
        count <- count + 1
      }

      # Apply dirname to dname counts times
      tempDname <- dname
      while (count > 0) {
        tempDname <- dirname(tempDname)
        count <- count - 1
      }

      # Paste tempDname to file.path
      file.name <- paste0(tempDname, "/", tempFileName)
      # rm(count, tempFileName, tempDname)
    }
    file.name
  }

  setVerbose <- function(newVerbose) {
    oldVerbose <- verbose
    verbose <<- newVerbose
    oldVerbose
  }

  setCacheDir <- function(new.cache.dir) {
    if (!file.exists(new.cache.dir)) {
      dir.create(new.cache.dir)
    }
    cache.dir <<- new.cache.dir
  }

  fileFromCache <- function(file) {
    if (!(gsub("^(http|https|ftp)://.*$", "\\1", file) %in% c("http", "https", "ftp"))) {
      return(file)
    }
    bname <- basename(file)
    cached.file <- paste0(cache.dir, "/", bname)
    if (!file.exists(cached.file)) {
      if (verbose) {
        cat("Downloading to cache dir...")
      }

      status <- try(download.file(file, cached.file, quiet = !verbose),
                    silent = TRUE)

      if (class(status)[1] == "try-error" || status == 1) {
        unlink(cached.file)
        stop(status, "\n")
      }

    } else {
      if (verbose) {
        cat("Using file from cache dir...\n")
      }
    }
    cached.file
  }

  openInstance <- function(file.inst) {
    dname.inst <<- dirname(file.inst)
    if (!is.null(cache.dir)) {
      file.inst <- fileFromCache(file.inst)
      inst.lnkb <<- file.inst
    }
    doc.inst <<- XBRL::xbrlParse(file.inst)
  }

  deleteCachedInstance <- function() {
    if (verbose) {
      cat("Deleting the following downloaded and/or cached files...\n")
      print(inst.lnkb)
    }
    unlink(inst.lnkb)
    if (verbose) {
      cat("Done...\n")
    }
  }

  getSchemaName <- function() {
    fixFileName(dname.inst, .Call("xbrlGetSchemaName", doc.inst, PACKAGE = "XBRL"))
  }

  processSchema <- function(file, level = 1) {
    if (verbose) {
      cat("Schema: ", file, "\n")
    }
    if (length(which(discovered.files == file)) > 0) {
      if (verbose) {
        cat("Already discovered. Skipping\n")
      }
      return(NULL)
    }
    discovered.files <<- c(discovered.files, file)
    dname <- dirname(file)
    if (level >= 1 && !is.null(cache.dir)) {
      if (verbose) {
        cat("Level:", level, "==>", file, "\n")
      }
      file <- fileFromCache(file)
      if (level == 1) {
        inst.lnkb <<- c(inst.lnkb, file)
      }
    }

    doc <- XBRL::xbrlParse(file)

    if (level == 1) {
      processRoles(doc)
    }
    processElements(doc)
    linkbaseNames <- .Call("xbrlGetLinkbaseNames", doc, PACKAGE = "XBRL")
    importNames <- .Call("xbrlGetImportNames", doc, PACKAGE = "XBRL")
    .Call("xbrlFree", doc, PACKAGE = "XBRL")

    for (linkbaseName in linkbaseNames) {
      linkbaseName <- fixFileName(dname, linkbaseName)
      if (verbose) {
        cat(file, " ==> Linkbase: ", linkbaseName, "\n")
      }
      processLinkbase(linkbaseName, level + 1)
    }

    for (importName in importNames) {
      importName <- fixFileName(dname, importName)
      if (verbose) {
        cat(file, " ==> Schema: ", importName, "\n")
      }
      processSchema(importName, level + 1)
    }
  }

  processRoles <- function(doc) {
    if (verbose) {
      cat("Roles\n")
    }
    self$role <<- rbind(self$role,
                        .Call("xbrlProcessRoles", doc, PACKAGE = "XBRL"))
  }

  processElements <- function(doc) {
    if (verbose) {
      cat("Elements\n")
    }
    self$element <<- rbind(self$element,
                           .Call("xbrlProcessElements", doc, PACKAGE = "XBRL"))
  }

  processLinkbase <- function(file, level) {
    if (verbose) {
      cat("Linkbase: ", file, "\n")
    }
    if (length(which(discovered.files == file)) > 0) {
      if (verbose) {
        cat("Already discovered. Skipping\n")
      }
      return(NULL)
    }
    discovered.files <<- c(discovered.files, file)
    if (level >= 2 && !is.null(cache.dir)) {
      if (verbose) {
        cat("Level:", level, "==>", file, "\n")
      }
      file <- fileFromCache(file)
      inst.lnkb <<- c(inst.lnkb, file)
    }
    doc <- XBRL::xbrlParse(file)

    ## We assume there can be only one type per linkbase file
    if (!processLabels(doc)) {
      if (!processPresentations(doc)) {
        if (!processDefinitions(doc)) {
          processCalculations(doc)
        }
      }
    }
    .Call("xbrlFree", doc, PACKAGE = "XBRL")
  }

  processLabels <- function(doc) {
    pre.length <- length(self$label)
    self$label <<- rbind(self$label,
                         ans <- .Call("xbrlProcessLabels", doc, PACKAGE = "XBRL"))
    if (!is.null(ans)) {
      if (verbose) {
        cat("Labels.\n")
      }
      return(TRUE)
    }
    FALSE
  }

  processPresentations <- function(doc) {
    pre.length <- length(self$presentation)
    self$presentation <<- rbind(self$presentation,
                                ans <- .Call("xbrlProcessArcs", doc, "presentation", PACKAGE = "XBRL"))
    if (!is.null(ans)) {
      if (verbose) {
        cat("Presentations.\n")
      }
      return(TRUE)
    }
    FALSE
  }

  processDefinitions <- function(doc) {
    pre.length <- length(self$definition)
    self$definition <<- rbind(self$definition,
                              ans <- .Call("xbrlProcessArcs", doc, "definition", PACKAGE = "XBRL"))
    if (!is.null(ans)) {
      if (verbose) {
        cat("Definitions.\n")
      }
      return(TRUE)
    }
    FALSE
  }

  processCalculations <- function(doc) {
    pre.length <- length(self$calculation)
    self$calculation <<- rbind(self$calculation,
                               ans <- .Call("xbrlProcessArcs", doc, "calculation", PACKAGE = "XBRL"))
    if (!is.null(ans)) {
      if (verbose) {
        cat("Calculations.\n")
      }
      return(TRUE)
    }
    FALSE
  }

  processContexts <- function() {
    if (verbose) {
      cat("Contexts\n")
    }
    self$context <<- .Call("xbrlProcessContexts", doc.inst, PACKAGE = "XBRL")
  }

  processFacts <- function() {
    if (verbose) {
      cat("Facts\n")
    }
    self$fact <<- .Call("xbrlProcessFacts", doc.inst, PACKAGE = "XBRL")
  }

  processUnits <- function() {
    if (verbose) {
      cat("Units\n")
    }
    self$unit <<- .Call("xbrlProcessUnits", doc.inst, PACKAGE = "XBRL")
  }

  processFootnotes <- function() {
    if (verbose) {
      cat("Footnotes\n")
    }
    self$footnote <<- .Call("xbrlProcessFootnotes", doc.inst, PACKAGE = "XBRL")
  }

  closeInstance <- function() {
    .Call("xbrlFree", doc.inst, PACKAGE = "XBRL")
    doc.inst <<- NULL
  }

  getResults <- function() {
    self
  }

  list(
    fileFromCache = fileFromCache,
    setVerbose = setVerbose,
    setCacheDir = setCacheDir,
    openInstance = openInstance,
    deleteCachedInstance = deleteCachedInstance,
    getSchemaName = getSchemaName,
    processSchema = processSchema,
    processContexts = processContexts,
    processFacts = processFacts,
    processUnits = processUnits,
    processFootnotes = processFootnotes,
    closeInstance = closeInstance,
    getResults = getResults)
}



#' A mirror function of XBRL
#'
#' A mirror function to package XBRL function xbrlDoAll
#'
#' This function is almost exactly equal to the function xbrlDoAll of the
#' package XBRL. The reason it exist is because the above mentioned doesn´t work
#' with https type links, which is fixed by editing the fixFileName function
#' inside the XBRL function.
#' On linux I was able to built it but It seams I´m not able to do so on
#' Windows because I don´t know how to make libxml accesible to Rtools. For the
#' mean time, this is a workaround.
#' @return A XBRL list of data.frames
#' @keywords XBRL
#' @export
xbrlDoAllFun <- function(file.inst, cache.dir="xbrl.Cache",
                         prefix.out=NULL, verbose=FALSE,
                         delete.cached.inst=TRUE) {
  xbrl <- funXBRL()
  xbrl$setVerbose(verbose)
  if (!is.null(cache.dir)) {
    xbrl$setCacheDir(cache.dir)
  }
  xbrl$openInstance(file.inst)
  xbrl$processSchema(xbrl$getSchemaName())
  xbrl$processContexts()
  xbrl$processFacts()
  xbrl$processUnits()
  xbrl$processFootnotes()
  xbrl$closeInstance()
  if (delete.cached.inst &&
      gsub("^(http|https|ftp)://.*$", "\\1", file.inst) %in% c("http", "https", "ftp")) {
    xbrl$deleteCachedInstance()
  }

  xbrl.vars <- xbrl$getResults()
  if (!is.null(prefix.out)) {
    if (verbose) {
      cat("Saving data\n")
    }
    write.csv(xbrl.vars$role, file=paste0(prefix.out, "_roles.csv"))
    write.csv(xbrl.vars$element, file=paste0(prefix.out, "_elements.csv"))
    write.csv(xbrl.vars$label, file=paste0(prefix.out, "_labels.csv"))
    write.csv(xbrl.vars$presentation, file=paste0(prefix.out, "_presentations.csv"))
    write.csv(xbrl.vars$definition, file=paste0(prefix.out, "_definitions.csv"))
    write.csv(xbrl.vars$calculation, file=paste0(prefix.out, "_calculations.csv"))
    write.csv(xbrl.vars$context, file=paste0(prefix.out, "_contexts.csv"))
    write.csv(xbrl.vars$fact, file=paste0(prefix.out, "_facts.csv"))
    write.csv(xbrl.vars$unit, file=paste0(prefix.out, "_units.csv"))
    write.csv(xbrl.vars$footnote, file=paste0(prefix.out, "_footnotes.csv"))
  }
  invisible(xbrl.vars)
}

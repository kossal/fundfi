#' Create finantial reports
#'
#' This function will create finantial reports of a list
#' of data.frames with the XBRL format.
#' @param xbrl.vars XBRL list of data.frames
#' @param statement Type of statement. Can be balance_sheet, income_statement and cash_flow
#' @return A data.frame as a financial report
#' @keywords Finantial Statement
#' @export
#' @examples
#' getStatement(getXBRL(c("PG", type="10-Q"))$PG, statement = income_statement)
#' @import tidyr dplyr
getStatement <- function(xbrl.vars = NULL, statement = "balance_sheet", custom.roleID=NULL) {

  if (is.null(xbrl.vars)) {
    stop("You have to provide a list of data.frames with XBRL format.")
  }

  role_names <- NULL
  if (is.null(custom.roleID)) {

    # Get names of statement specified and search for them in roles table
    switch (statement,
            balance_sheet = {
              role_names <- c(
                "CONSOLIDATED BALANCE SHEETS",
                "CONDENSED CONSOLIDATED BALANCE SHEETS (Unaudited)",
                "Consolidated Balance Sheets (Unaudited)"
              )
            },
            income_statement = {
              role_names <- c(
                "CONSOLIDATED STATEMENTS OF EARNINGS",
                "Consolidated Statements of Comprehensive Loss (Unaudited)",
                "Consolidated Statements of Comprehensive Income (Unaudited)",
                "CONDENSED CONSOLIDATED STATEMENTS OF COMPREHENSIVE INCOME (Unaudited)"
              )
            },
            cash_flow = {
              role_names <- c(
                "CONSOLIDATED STATEMENTS OF CASH FLOWS",
                "Consolidated Statements of Cash Flows (Unaudited)",
                "CONDENSED CONSOLIDATED STATEMENTS OF CASH FLOWS (Unaudited)"
              )
            }
    )

    if (is.null(role_names)) {
      stop(paste("There is no", statement, "option."))
    }

  } else {
    role_names <- c(custom.roleID)
  }

  role_id <- NULL
  for (name in role_names) {
    role_id <- xbrl.vars$role[xbrl.vars$role$description == name,]$roleId

    if (length(role_id) > 0) {
      break
    }
  }
  if (length(role_id) == 0) {
    stop(paste("Role id names could not be found", xbrl.vars$role$description))
  }

  # XBRL includes three hierarchies of concepts: definition, presentation
  # and calculation. Hierarchies are stored as links in definition, presentation
  # and calculation tables. Columns fromElementId and toElementId represent
  # parent and child.
  relations <-
    xbrl.vars$calculation %>%
    filter(roleId == role_id) %>%
    select(fromElementId, toElementId, order)

  elements <-
    data.frame(
      elementId = with(relations, unique(c(fromElementId, toElementId))),
      stringsAsFactors = FALSE
    ) %>%
    left_join(xbrl.vars$element, by = c("elementId")) %>%
    left_join(relations, by = c("elementId" = "toElementId")) %>%
    left_join(xbrl.vars$label, by = c("elementId")) %>%
    filter(labelRole == "http://www.xbrl.org/2003/role/label") %>%
    transmute(elementId, parentId = fromElementId, order, balance, labelString)

  level <- 1
  df1 <- elements %>%
    filter(is.na(parentId)) %>%
    mutate(id = "") %>%
    arrange(desc(balance))

  # search the tree
  while({
    level_str <-
      unname(unlist(lapply(split(df1$id, df1$id), function(x) {
        sprintf("%s%02d", x, 1:length(x))
      })))

    elements[elements$elementId %in% df1$elementId, "level"] <- level
    to_update <- elements[elements$elementId %in% df1$elementId, "elementId"]
    elements[
      #order(match(elements$elementId, to_update))[1:length(level_str)],
      order(match(elements$elementId, df1$elementId))[1:length(level_str)],
      "id"] <- level_str

    df1 <- elements %>%
      filter(parentId %in% df1$elementId) %>%
      arrange(order) %>%
      select(elementId, parentId) %>%
      left_join(elements, by=c("parentId"="elementId")) %>%
      arrange(id)
    nrow(df1) > 0})
  {
    level <- level + 1
  }

  # Create hierarchy on labelString
  elementsHier <-
    elements %>%
    dplyr::mutate(
      terminal = !elementId %in% parentId,
      element = paste(
        substring( paste(rep("&nbsp;", 10), collapse = ""), 1, (level-1)*2*6),
        labelString
      )
    )

  statement_table <- data.frame()
  statement_hierarchy <- data.frame()

  if (statement == "balance_sheet") {

    statement_table <- elementsHier %>%
      left_join(xbrl.vars$fact, by = "elementId") %>%
      left_join(xbrl.vars$context, by = "contextId") %>%
      filter(is.na(dimension1)) %>% # dimension1 NA significa que no pertenecen a un subcontexto
      filter(!is.na(endDate)) %>%
      mutate( fact = as.numeric(fact) * 10 ^ as.numeric(decimals) ) %>%
      dplyr::arrange(id) %>%
      select(id, level, parentId, elementId, labelString, element, value = fact, balance, endDate)

    statement_hierarchy <- statement_table %>%
      select(-(id)) %>%
      distinct() %>%
      spread(endDate, value) # Organiza todos los endDate en columnas y como valores pone fact

  } else {

    # Gather facts from last start date
    statement_table <- elementsHier %>%
      left_join(xbrl.vars$fact, by = "elementId") %>%
      left_join(xbrl.vars$context, by = "contextId") %>%
      filter(is.na(dimension1)) %>%
      filter(!is.na(endDate)) %>%
      mutate( fact = as.numeric(fact) * 10 ^ as.numeric(decimals) ) %>%
      dplyr::arrange(id) %>%
      select(id, level, parentId, elementId, labelString, element, value = fact, balance, startDate, endDate) %>%
      spread(startDate, value)

    # Get all startDates and filter the lastest ones
    datesCol <- !is.na(as.Date(names(statement_table), format="%Y-%m-%d"))
    for (i in (1:nrow(statement_table))) {
      curr <- statement_table[i, datesCol]

      if (length(curr[!is.na(curr)]) > 1 ) {
        times <- names(statement_table)[datesCol][!is.na(curr)]
        maxTime <- max(as.POSIXct(times, format="%Y-%m-%d")) == as.POSIXct(names(statement_table)[datesCol], format="%Y-%m-%d")
        max <- c(rep(c(FALSE), length(statement_table) - length(names(statement_table)[datesCol])), maxTime)
        statement_table[i, max] <- NA
      }

    }

    statement_hierarchy <- statement_table %>%
      gather(names(statement_table)[datesCol], key = "startDate", value = "value", na.rm = TRUE) %>%
      select(-(id), -(startDate)) %>%
      distinct() %>%
      spread(endDate, value)

  }

  invisible(statement_hierarchy)

}

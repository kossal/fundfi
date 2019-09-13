#' Create finantial report
#'
#' Create specified finantial report from XBRL list.
#'
#' Creates a data.frame containing a finantial
#' report using a XBRL list returned by XBRL
#' library. It uses the calculation data.frame
#' to unite elements with facts, contexts and labels.
#' Income statements and cash flows sometimes have
#' diferent start dates for a element with
#' the same end date, in this case it will use
#' the last start date. Each company uses diferent
#' descriptions for the same roles (balance sheet,
#' income statement and cash flow). This library
#' tries it best using regex to get every posible
#' description but of course there will be exceptions
#' or the user might want to use a diferent concept.
#' For this case, you can use the parameter custom.description
#' to use your own description.
#' @param xbrl.vars XBRL list of data.frames
#' @param statement Type of statement. Can be balance_sheet, income_statement and cash_flow
#' @param custom.description Character to fetch roleId using description from roles data.frame
#' @return data.frame as a financial report
#' @keywords Finantial Statement
#' @export
#' @examples
#' getStatement(getXBRL(c("PG", type="10-Q"))$PG, statement = "income_statement")
#' @import dplyr
getStatement <- function(xbrl.vars = NULL, statement = "balance_sheet", custom.description = NULL) {

  BALANCE_SHEET <- "balance_sheet"
  INCOME_STATEMENT <- "income_statement"
  CASH_FLOW <- "cash_flow"

  if (is.null(xbrl.vars)) {
    stop("You have to provide a list of data.frames with XBRL format.")
  }

  # Try to get roleId using diferent descriptions based on statement
  role_names <- list()
  description <- character(0)
  role_id <- NULL
  if (is.null(custom.description)) {

    # Get names of statement specified and use regex of description role
    if (statement == BALANCE_SHEET) {

      role_names <- list(
        stringr::regex("^(condensed|consolidated|condensed consolidated) balance sheets?", ignore_case = TRUE)
      )

    } else if (statement == INCOME_STATEMENT) {

      role_names <- list(
        stringr::regex("^(statements?|condensed statements?|consolidated statements?|condensed consolidated statements?) of (operations|income|earnings)", ignore_case = TRUE),
        stringr::regex("^(condensed|consolidated|condensed consolidated) (operations?|earnings?|income) statements?", ignore_case = TRUE)
      )

    } else if (statement == CASH_FLOW) {

      role_names <- list(
        stringr::regex("^(statements?|condensed statements?|consolidated statements?|condensed consolidated statements?) of cash flows?", ignore_case = TRUE)
      )

    } else {
      stop(paste("There is no", statement, "option."))
    }

  } else {
    # Use custom.description to get roleId
    role_id <- xbrl.vars$role[xbrl.vars$role$description == custom.description, "roleId"]

    if (nchar(role_id) == 0) {
      stop(paste("Role id names could not be found using custom.description", custom.description))
    }
  }

  if (is.null(role_id)) {

    # Loop all regex from statement until description is found
    for (pattern in role_names) {

      # Get matching description
      matchingDesc <- stringr::str_subset(xbrl.vars$role$description, pattern)

      # Drop matches with (Parenthetical)
      noParenthetical <- !stringr::str_detect(matchingDesc, stringr::regex("parenthetical", ignore_case = TRUE))

      description <- matchingDesc[noParenthetical][1]

      if (!is.na(description)) {
        break;
      }

    }

    if (is.na(description)) {
      stop(paste("Descriptions could not be found for", statement, ". Maybe consider using custom.description"))
    }

    role_id <- xbrl.vars$role[xbrl.vars$role$description == description, "roleId"][1]

    if (is.na(role_id)) {
      stop(paste("Role id names could not be found using", description))
    }

  }

  # Start building statement
  # Get elements from the role_id in calculations
  calc <- xbrl.vars$calculation[xbrl.vars$calculation$roleId == role_id,] %>%
    mutate(order = as.numeric(order))

  if (nrow(calc) == 0 & !is.null(custom.description)) {
    stop(paste("No data was found using:", custom.description))
  } else if (nrow(calc) == 0) {
    stop(paste("No data was found using:", description))
  }

  # Get first level
  hier <- calc %>%
    anti_join(calc, by=c("fromElementId" = "toElementId")) %>%
    select(elementId = fromElementId) %>%
    distinct()

  # Arrange assest as first element
  if (statement == "balance_sheet") {
    hier <- arrange(hier, elementId)
  }

  # Create id and level columns
  l <- 1
  hier <- hier %>%
    transmute(id = sprintf("%02d", 1:nrow(hier)), level = l, elementId)

  # Create id and levels columns for all childs
  repeat {

    # Get childs of last level
    tempHier <- hier %>%
      filter(level == l) %>%
      inner_join(calc, by=c("elementId" = "fromElementId"))

    # If no child exists, stop loop
    if (nrow(tempHier) == 0) {
      break
    }

    # Create id and level columns for childs andd append it to hier
    l <- l + 1
    tempHier <- tempHier %>%
      select(id, parentId = elementId, elementId = toElementId, order) %>%
      group_by(parentId) %>%
      arrange(order, .by_group = TRUE) %>%
      ungroup() %>%
      select(-(parentId)) %>%
      transmute(id = sprintf("%s%02d", id, order), level = l, elementId)

    hier <- rbind(hier, tempHier)

  }

  rm(l, tempHier)

  res <- arrange(hier, id)

  # Join hier to calculation to get parentId
  # Join with element to get balance
  # Join with fact to get values, unitId, decimals and contextId
  # Join with context to get dates
  # Join with labels to get labelString
  # Create concept as label string plus leading spaces by level
  elements <- hier %>%
    left_join(calc, by=c("elementId" = "toElementId")) %>%
    select(id, level, parentId = fromElementId, elementId)

  elementsWLabels <- elements %>%
    left_join(xbrl.vars$element, by="elementId") %>%
    left_join(xbrl.vars$fact, by="elementId") %>%
    left_join(xbrl.vars$context, by="contextId") %>%
    left_join(xbrl.vars$label, by="elementId") %>%
    filter(is.na(dimension1), lang == "en-US", labelRole == "http://www.xbrl.org/2003/role/label") %>%
    mutate(fact = as.numeric(fact) * 10 ^ as.numeric(decimals)) %>%
    mutate(concept = sprintf("%s%s", strrep("   ", level - 1), labelString)) %>%
    select(id, level, parentId, elementId, balance, unitId, decimals, labelString, concept, fact, startDate, endDate)

  # Income statements and cash flows have diferent start dates
  # for each endDate
  temp <- data.frame()
  if (statement != "balance_sheet") {

    temp <- elementsWLabels %>%
      group_by(elementId, endDate) %>%
      filter(startDate == min(as.Date(startDate))) %>%
      ungroup() %>%
      ungroup()

  } else {
    temp <- elementsWLabels
  }

  elementsPretty <- temp %>%
    select(-(startDate)) %>%
    distinct() %>%
    tidyr::spread(key = endDate, value = fact)

  invisible(elementsPretty)

}

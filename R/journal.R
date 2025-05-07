#### Journal related utilities
#'   readJournal
#'
#' This function reads the Journal table.
#' It returns all entries from the journal starting from windowDate
#'@param windowDate an integer, with the format YYYYmmdd. If not provided then it will return all records from current date - 300 days.
#'@returns a tibble with the following fields: \code{ entryId	theme	date
#' sym	close	change	mkt_price	mkt_change
#' text}
#'@examples
#'\dontrun{
#'readJournal()
#'}
#'@export
readJournal <- function(windowDate = NA) {
  if (is.na(windowDate)) windowDate = as.numeric(format(Sys.Date() - 300,"%Y%m%d"))
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  journal = DBI::dbGetQuery(conn, "SELECT * FROM Journal WHERE date >= ?", list(windowDate))
  DBI::dbDisconnect(conn)
  journal
}

#'   readJournalMaxEntryId
#'
#' This function retrieves the maximal entryId number. This is a unique key in table Journal.
#'@returns an integer
#'@examples
#'\dontrun{
#'readJournalMaxEntryId()
#'}
#'@export
readJournalMaxEntryId <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  maxId <- DBI::dbGetQuery(conn, "SELECT MAX(entryId) FROM Journal")
  DBI::dbDisconnect(conn)
  as.numeric(maxId)
}

#'   writeJournalEntry
#'
#' This function appends its argument into Journal table. No checks are performed.
#'@returns number of lines appended, if no error it returns 1. It returns 0 if there is an error
#'@param entry a tibble, it should have the following fields: \code{ entryId	theme	date
#' sym	close	change	mkt_price	mkt_change}
#'@export
writeJournalEntry <- function(entry) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  status <- DBI::dbAppendTable(conn, "Journal", entry)
  DBI::dbDisconnect(conn)
  status
}

#'   modifyJournalEntry
#'
#' This function updates an entry into Journal table.
#'
#' It checks first that close is numeric and change is character. Otherwise it just sends the update request to DB.
#'@returns number of lines modified, if no error it returns 1. It returns 0 if there is an error
#'@param entryId, integer - entry key to modify into the Journal
#'@param theme, string
#'@param date, string
#'@param sym, string - symbol to be updated
#'@param close, numeric, equal to last close value for the symbol
#'@param change, character - percentage change between last day and penultimate day
#'@param mkt_price, double
#'@param mkt_change, character - percentage change between last day and penultimate day
#'@param text, string - comment, remark
#'@export
modifyJournalEntry <- function(entryId, theme, date, sym, close, change, mkt_price, mkt_change, text) {

  ### Initialize update string and params list
  sql <- ""
  params <- list()

  if (is.character(sym) && length(sym) > 1) {
    sql <- "UPDATE Journal SET sym = ?"
    params = list(sym)
  }

  if (is.numeric(close)) {
    if (length(sql) == 0) {
      sql <- "UPDATE Journal SET close = ?"
      params <- list(close)
    }
    else {
      sql <- paste(sql, ", close = ?")
      params <- append(params, close)
    }
  }

  if (is.character(change) && length(change) > 1) {
    if (length(sql) == 0) {
      sql <- "UPDATE Journal SET change = ?"
      params <- list(change)
    }
    else {
      sql <- paste(sql, ", change = ?")
      params <- append(params, change)
    }
  }

  if (is.numeric(mkt_price)) {
    if (length(sql) == 0) {
      sql <- "UPDATE Journal SET mkt_price = ?"
      params <- list(mkt_price)
    }
    else {
      sql <- paste(sql, ", mkt_price = ?")
      params <- append(params, mkt_price)
    }
  }

  if (is.character(mkt_change) && length(mkt_change) > 1) {
    if (length(sql) == 0) {
      sql <- "UPDATE Journal SET mkt_change = ?"
      params <- list(mkt_change)
    }
    else {
      sql <- paste(sql, ", mkt_change = ?")
      params <- append(params, mkt_change)
    }
  }

  if (is.character(theme) && length(theme) > 1) {
    if (length(sql) == 0) {
      sql <- "UPDATE Journal SET theme = ?"
      params <- list(theme)
    }
    else {
      sql <- paste(sql, ", theme = ?")
      params <- append(params, theme)
    }
  }

  if (is.character(date) && length(date) > 1) {
    if (length(sql) == 0) {
      sql <- "UPDATE Journal SET date = ?"
      params <- list(date)
    }
    else {
      sql <- paste(sql, ", date = ?")
      params <- append(params, date)
    }
  }

  if (is.character(text) && length(text) > 1) {
    if (length(sql) == 0) {
      sql <- "UPDATE Journal SET text = ?"
      params <- list(text)
    }
    else {
      sql <- paste(sql, ", text = ?")
      params <- append(params, text)
    }
  }

  if (length(sql) != 0) {
    sql <- paste(sql, "WHERE entryID =?;")
    conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
    status <- DBI::dbExecute(conn, sql, params)
    DBI::dbDisconnect(conn)
    status
  }

  else return(0)
}

#'   deleteJournalEntry
#'
#' This function removes an entry from Journal table.
#'
#' No verification is done prior sending request to DB, i.e. if entryId exists corresponding line is removed otherwise it returns 0
#'@returns number of lines deleted, if no error it returns 1. It returns 0 if there is an error
#'@param entryId, integer - entry key for the entry to be removed
#'@export
deleteJournalEntry <- function(entryId) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  status <- DBI::dbExecute(conn, "DELETE FROM Journal WHERE entryId = ?;",
                           params=list(entryId))
  DBI::dbDisconnect(conn)
  status
}


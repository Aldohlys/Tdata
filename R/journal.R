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
modifyJournalEntry <- function(entryId, theme=NULL, date=NULL, sym=NULL, close=NULL, change=NULL, mkt_price=NULL, mkt_change=NULL, text=NULL) {

  ### Initialize update string and params list
  sql <- ""
  params <- list()

  ### This verifies that it has the required type and it is not NULL (default value)
  if (is.character(sym)) {
    sql <- "UPDATE Journal SET sym = ?"
    params = list(sym)
  }

  ### This verifies that it has the required type and it is not NULL (default value)
  if (is.numeric(close)) {
    if (nchar(sql) == 0) {
      sql <- "UPDATE Journal SET close = ?"
      params <- list(close)
    }
    else {
      sql <- paste(sql, ", close = ?")
      t_log_debug(sql)
      params <- append(params, close)
      t_log_debug(paste(unlist(params), collapse=" "))
    }
  }

  ### This verifies that it has the required type and it is not NULL (default value)
  if (is.character(change)) {
    if (nchar(sql) == 0) {
      sql <- "UPDATE Journal SET change = ?"
      params <- list(change)
    }
    else {
      sql <- paste(sql, ", change = ?")
      t_log_debug(sql)
      params <- append(params, change)
      t_log_debug(paste(unlist(params), collapse=" "))
      }
  }

  ### This verifies that it has the required type and it is not NULL (default value)
  if (is.numeric(mkt_price)) {
    if (nchar(sql) == 0) {
      sql <- "UPDATE Journal SET mkt_price = ?"
      params <- list(mkt_price)
    }
    else {
      sql <- paste(sql, ", mkt_price = ?")
      t_log_debug(sql)
      params <- append(params, mkt_price)
      t_log_debug(paste(unlist(params), collapse=" "))
    }
  }

  ### This verifies that it has the required type and it is not NULL (default value)
  if (is.character(mkt_change)) {
    if (nchar(sql) == 0) {
      sql <- "UPDATE Journal SET mkt_change = ?"
      params <- list(mkt_change)
    }
    else {
      sql <- paste(sql, ", mkt_change = ?")
      t_log_debug(sql)
      params <- append(params, mkt_change)
      t_log_debug(paste(unlist(params), collapse=" "))
    }
  }

  ### This verifies that it has the required type and it is not NULL (default value)
  if (is.character(theme)) {
    if (nchar(sql) == 0) {
      sql <- "UPDATE Journal SET theme = ?"
      params <- list(theme)
    }
    else {
      sql <- paste(sql, ", theme = ?")
      t_log_debug(sql)
      params <- append(params, theme)
      t_log_debug(paste(unlist(params), collapse=" "))
    }
  }

  ### This verifies that it has the required type and it is not NULL (default value)
  if (is.character(date)) {
    if (nchar(sql) == 0) {
      sql <- "UPDATE Journal SET date = ?"
      params <- list(date)
    }
    else {
      sql <- paste(sql, ", date = ?")
      t_log_debug(sql)
      params <- append(params, date)
      t_log_debug(paste(unlist(params), collapse=" "))
    }
  }

  ### This verifies that it has the required type and it is not NULL (default value)
  if (is.character(text)) {
    if (nchar(sql) == 0) {
      sql <- "UPDATE Journal SET text = ?"
      params <- list(text)
    }
    else {
      sql <- paste(sql, ", text = ?")
      t_log_debug(sql)
      params <- append(params, text)
      t_log_debug(paste(unlist(params), collapse=" "))
    }
  }

  if (nchar(sql) != 0) {
    sql <- paste(sql, "WHERE entryID =?;")
    params <- append(params, entryId)
    t_log_debug(sql)
    t_log_debug(paste(unlist(params), collapse=" "))
    conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
    on.exit(DBI::dbDisconnect(conn), add=TRUE)

    ### In case DB cannot be accesssed - locked for instance
    tryCatch(DBI::dbExecute(conn, sql, params), error = function(e) {
      t_log_error("Error while trying to update Journal DB: ", e)
      return(0)}
      )
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


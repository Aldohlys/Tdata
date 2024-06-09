#### Journal related utilities
#'   readJournal
#'
#' This function reads the Journal table.
#' It returns all entries from the journal
#'@returns a tibble with the following fields: \code{ entryId	theme	date
#' sym	close	change	mkt_price	mkt_change
#' text}
#'@examples
#'\dontrun{
#'readJournal()
#'}
#'@export
readJournal <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  journal = DBI::dbReadTable(conn, "Journal")
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
#'@param close, numeric, equal to last close value for the symbol
#'@param change, character - percentage change between last day and penultimate day
#'@param text, string - comment, remark
#'@export
modifyJournalEntry <- function(entryId, close, change, text) {
  if( (!(is.numeric(close))) | (!(is.character(change))) ) display_error_message("Please provide numerical value for close and percentage for change")
  else {
    conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
    status <- DBI::dbExecute(conn, "UPDATE Journal SET close = ?, change =?, text = ? WHERE entryId = ?;",
                             params=list(close, change, text, entryId))
    DBI::dbDisconnect(conn)
    status
  }

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


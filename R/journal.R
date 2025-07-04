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
  ## Look at current date - 365 (1 year ago)
  if (is.na(windowDate)) windowDate = suppressWarnings(as.integer(format(Sys.Date() - 365,"%Y%m%d")))
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
  suppressWarnings(as.integer(maxId))
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
  status <- safe_db_append(conn, "Journal", entry)
  DBI::dbDisconnect(conn)
  status
}

#'   modifyJournalEntry
#'
#' This function updates an entry into Journal table.
#'
#' Using internal db_validation functions, it performs extensive checks on data to make sure it can be stored into Journal DB.
#'@returns number of lines modified, i.e. if no error it returns 1. It returns 0 if there is an error
#'@param entryId, integer - entry key to modify into the Journal
#'@param theme, character
#'@param date, integer, or Date or string
#'@param sym, character - symbol to be updated
#'@param close, numeric, equal to last close value for the symbol - maybe a string
#'@param change, character - percentage change between last day and penultimate day
#'@param mkt_price, numeric - may be a string or number
#'@param mkt_change, character - percentage change between last day and penultimate day
#'@param text, character - comment, remark
#'@export
modifyJournalEntry <- function(entryId, theme=NULL, date=NULL, sym=NULL, close=NULL, change=NULL, mkt_price=NULL, mkt_change=NULL, text=NULL) {

  ### This code does not use safe_db_append/write
  ### Checks must therefore be done before sending SQL update statement
  # Build full list first
  all_args <- list(
    theme = theme, date = date, sym = sym, close = close,
    change = change, mkt_price = mkt_price, mkt_change = mkt_change, text = text
  )
  # Filter out NULLs
  data <- validate_db_types(data = Filter(Negate(is.null), all_args), "Journal")

  ### Initialize update string and params list
  sql <- ""
  params <- list()
  t_log_debug("entryId: {entryId}")

  #### Local functions
  validate_arg <- function(x) {
    !is.null(x) && !is.na(x)
  }

  build_sql <- function(sql, params, mod_param) {
    if (validate_arg(data[[mod_param]])) {
      if (nchar(sql) == 0) {
        sql <- paste0("UPDATE Journal SET ", mod_param, " = ?")
        t_log_debug("param: {mod_param} sql:{sql}")
        params <- list(data[[mod_param]])
        t_log_debug("param: {mod_param} params_list: {paste(unlist(params) , collapse=\" \")}")
      }
      else {
        sql <- paste(sql, ", ", mod_param ,"= ?")
        t_log_debug("param: {mod_param} sql:{sql}")
        params <- append(params, list(data[[mod_param]]))
        t_log_debug("param: {mod_param} params_list: {paste(unlist(params) , collapse=\" \")}")
      }
    }
    return(list(sql=sql, params=params))
  }

  ###########  Build SQL statement including all arguments
  for (mod_param in names(data)) {
    result <- build_sql(sql, params, mod_param)

    sql <- result$sql
    params <- result$params
  }

  if (nchar(sql) != 0) {

    ## COmplete SQL statement
    sql <- paste(sql, "WHERE entryID = ?;")
    t_log_debug(sql)

    ### Complete params as well
    params <- append(params, list(entryId))
    t_log_debug("EntryId added: {paste(unlist(params), collapse=\" \")}")

    conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
    on.exit(DBI::dbDisconnect(conn), add=TRUE)

    ### In case DB cannot be accessed - locked for instance
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
  t_log_debug("deleteJournalentry: entryId {entryId}")
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  status <- DBI::dbExecute(conn, "DELETE FROM Journal WHERE entryId = ?;",
                           params=list(entryId))
  DBI::dbDisconnect(conn)
  status
}


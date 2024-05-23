
#####  strategies.E
##### All utilities related to strategies

#' getStrategies
#'
#' This function is used by RReporting functions and allows to obtain all valid strategies from DB
#'
#'@return A vector of characters sorted by alphabetical order, equal to the list of current strategies
#'@export
getStrategies = function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  strategies = DBI::dbReadTable(conn, "Strategies")
  DBI::dbDisconnect(conn)

  return(sort(strategies$Name))
}



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


#' getParam
#'
#' This function returns the value of requested parameter in table Param.
#'@param name string, if unknown then function returns NULL
#'@returns a string, equals to value in table Param
#'@export
getParam <- function(name) {
  ### Look at DB
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  record <- DBI::dbGetQuery(conn, "SELECT Value FROM Param WHERE Name = ?", params=list(name))
  if (nrow(record) == 0) {
    logger::log_info("Param {name} does not exist in Param table")
    return(NULL)
  }

  value = as.character(record)
  return(value)
}

#' setParam
#'
#' This function returns the value of requested parameter in table Param.
#'@param name string, may be present or not in table Param
#'@param value string - if equals NA or NULL then the record is removed. Defautl value is NULL
#'@returns -1 if a parameter is removed, 0 if parameter updated, 1, if new parameter added
#'@export
setParam <- function(name, value=NULL) {
  ### Look at DB
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  record <- DBI::dbGetQuery(conn, "SELECT Value FROM Param WHERE Name = ?", params=list(name))

  ### Case where parameter does not exist
  if (nrow(record) == 0) {
    logger::log_info("Param {name} does not exist in Param table")

    ### No value provided -> no action
    if (is.null(value) || is.na(value)) {
      Tbasics::display_message("setParam function needs a value different from NULL or NA !")
      logger::log_info("Impossible to set Param, {name} does not exist in Param table and no value provided")
      return(0)
    }

    ### Value provided -> insert new parameter with a value
    else {
      rows_inserted <- DBI::dbExecute(conn, "INSERT INTO Param (name, value) VALUES (?, ?)",
                                  params = list(name, value))
      if (rows_inserted == 1) logger::log_debug("Param {name} inserted in Param table, new value is: {value}")
      else logger::log_debug("Param {name} not correctly inserted in Param table")
      return(rows_inserted)
    }
  }

  ### Case where parameter name exists
  else {
    logger::log_info("Param {name} value in Param table: {record$value}")

    ### remove parameter from table
    if (is.null(value) || is.na(value)) {
      rows_deleted = DBI::dbExecute(conn, "DELETE FROM Param WHERE name = ?", params = list(name))
      if (rows_deleted == 1) logger::log_debug("Param {name} removed from Param table")
      else logger::log_debug("Param {name} not correctly removed from Param table")
      return(-rows_deleted)
    }

    ### set parameter to new value
    rows_updated <- DBI::dbExecute(conn, "UPDATE Param SET value = ? WHERE name = ?",
                              params = list(value, name))
    if (rows_updated == 1) logger::log_debug("Param {name} modified in Param table, new value is: {value}")
    else logger::log_debug("Param {name} not correctly modified in Param table")
    return(rows_updated)
  }

}

#' listParam
#'
#' This function lists selected parameters in table Param with their vales. If no argument provided, all params are listed
#'@returns a data frame including all names and values, sorted by name
#'@export
listParam <- function() {

  ### Look at DB
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add=TRUE)

  df <- DBI::dbGetQuery(conn, "SELECT * FROM Param ORDER BY Name")

  return(df)
}

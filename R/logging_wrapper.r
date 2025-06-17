#' Update log threshold for Tdata
#'
#' @param level New log level threshold
#' @param console_only Update only console threshold
#' @param file_only Update only file threshold
#' @keywords internal
#' @noRd
set_log_level <- function(level, console_only = FALSE, file_only = FALSE) {
  Tlogger::update_log_level("Tdata", level, console_only, file_only)
}

#' @title Log a debug message
#' @description Convenience function to log at DEBUG level
#' @param msg Message to log
#' @noRd
#' @keywords internal
t_log_debug <- function(msg,...) {
  # Evaluate glue string in calling environment
  evaluated_msg <- glue::glue(msg, .envir = parent.frame())
  logger::log_debug(evaluated_msg, ..., namespace = "Tdata")
}

#' @title Log an info message
#' @description Convenience function to log at INFO level
#' @param msg Message to log
#' @keywords internal
#' @noRd
t_log_info <- function(msg, ...) {
  # Evaluate glue string in calling environment
  evaluated_msg <- glue::glue(msg, .envir = parent.frame())
  logger::log_info(evaluated_msg, ..., namespace = "Tdata")
}

#' @title Log an error message
#' @description Convenience function to log at ERROR level
#' @param msg Message to log
#' @keywords internal
#' @noRd
t_log_error <- function(msg, ...) {
  # Evaluate glue string in calling environment
  evaluated_msg <- glue::glue(msg, .envir = parent.frame())
  logger::log_error(evaluated_msg, ..., namespace = "Tdata")
}

#' @title Log a warning message
#' @description Convenience function to log at WARN level
#' @param msg Message to log
#' @keywords internal
#' @noRd
t_log_warn <- function(msg, ...) {
  # Evaluate glue string in calling environment
  evaluated_msg <- glue::glue(msg, .envir = parent.frame())
  logger::log_warn(evaluated_msg, ..., namespace = "Tdata")
}



#' #' @title Log an exception with details
#' #'
#' #' @description Log an error with exception details
#' #'
#' #' @param e Error object from tryCatch
#' #' @param message Additional message to log
#' #' @param context Named list of contextual information
#' #' @return Invisibly returns TRUE
#' tdata_log_exception <- function(e, message = "Exception occurred", context = NULL) {
#'   # Wrapper for Tbasics
#'   t_log_exception(e, message, context)
#' }
#'
#' #' @title Create a function wrapper that logs execution time
#' #'
#' #' @description Wrap a function to log its execution time
#' #'
#' #' @param func Function to wrap
#' #' @param func_name Optional function name (defaults to function name)
#' #' @return Wrapped function with timing
#' tdata_log_execution_time <- function(func, func_name = NULL) {
#'     # Wrapper for Tbasics
#'     t_log_execution_time(func, func_name)
#' }
#'

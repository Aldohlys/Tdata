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

# Logging is done directly with logger::log_* functions with namespace="Tdata"
# Example usage:
#   logger::log_info("Message {variable}", namespace="Tdata")
#   logger::log_debug("Debug info", namespace="Tdata")
#   logger::log_warn("Warning", namespace="Tdata")
#   logger::log_error("Error occurred", error_obj, namespace="Tdata")
#
# Tlogger's formatter handles glue interpolation and supports:
# - Simple strings
# - Glue syntax with {variable} interpolation
# - Multiple arguments (passed as additional params)
# - Complex objects (formatted with pander)


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

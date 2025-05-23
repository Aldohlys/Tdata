#' @title Log a debug message
#'
#' @description Convenience function to log at DEBUG level
#'
#' @param message Message to log
#' @param context Named list of contextual information
#' @return Invisibly returns TRUE
tdata_log_debug <- function(message, context = NULL) {
  argts <- paste(names(context), context, sep="=", collapse=", ")
  logger::log_debug(message, argts , namespace = "Tdata")
}


#' @title Log an info message
#'
#' @description Convenience function to log at INFO level
#'
#' @param message Message to log
#' @param context Named list of contextual information
#' @return Invisibly returns TRUE
tdata_log_info <- function(message, context = NULL) {
  argts <- paste(names(context), context, sep="=", collapse=", ")
  logger::log_info(message, argts , namespace = "Tdata")
}

#' @title Log a warning message
#'
#' @description Convenience function to log at WARN level
#'
#' @param message Message to log
#' @param context Named list of contextual information
#' @return Invisibly returns TRUE
tdata_log_warn <- function(message, context = NULL) {
  argts <- paste(names(context), context, sep="=", collapse=", ")
  logger::log_warn(message, argts , namespace = "Tdata")
}

#' @title Log an error message
#'
#' @description Convenience function to log at ERROR level
#'
#' @param message Message to log
#' @param context Named list of contextual information
#' @return Invisibly returns TRUE
tdata_log_error <- function(message, context = NULL) {
  argts <- paste(names(context), context, sep="=", collapse=", ")
  logger::log_error(message, argts , namespace = "Tdata")
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

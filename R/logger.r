#' @title Tdata Logging Functions
#' @description Simple logging functionality for Tdata package
#'
#' @importFrom futile.logger flog.trace flog.debug flog.info flog.warn flog.error flog.fatal flog.threshold flog.appender flog.layout appender.console appender.file layout.simple

#' @title Set up logging for Tdata package
#'
#' @description Initializes the logging system with the specified level and output options
#'
#' @param level Logging level (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)
#' @param log_dir Optional directory for log files, NULL for current working directory/logs
#' @param log_file Optional specific file path for log output, overrides daily_log if provided
#' @param daily_log Whether to automatically create a daily log file (YYYYMMDD format)
#' @param control_ibinsync Whether to control ib_insync logging separately
#' @param ibinsync_level Logging level for ib_insync if control_ibinsync is TRUE#' @return Invisibly returns TRUE if successful
#' @export
# Ajoutez ce paramètre à votre fonction tdata_setup_logger
tdata_setup_logger <- function(level = "INFO",
                               log_dir = NULL,  # paramètre pour le répertoire de logs
                               log_file = NULL,
                               daily_log = TRUE,  # paramètre pour les logs quotidiens
                               control_ibinsync = TRUE,
                               ibinsync_level = "ERROR") {

  # Supprimer tous les appenders existants
  futile.logger::flog.appender(NULL)

  # Si daily_log est activé et qu'aucun fichier spécifique n'est fourni
  if (daily_log && is.null(log_file)) {
    # Définir le répertoire de logs par défaut si non spécifié
    if (is.null(log_dir)) {
      # Utilisez un répertoire logs dans le répertoire de travail actuel
      log_dir <- file.path(getwd(), "logs")
    }

    # Créer le répertoire si nécessaire
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }

    # Créer un nom de fichier avec la date du jour
    today_date <- format(Sys.Date(), "%Y%m%d")
    log_file <- file.path(log_dir, paste0("tdata_", today_date, ".log"))

    futile.logger::flog.info(paste("Daily log file:", log_file), name= "INFO")
  }

  # Le reste de votre fonction reste identique
  log_level <- switch(toupper(level),
                      "TRACE" = futile.logger::TRACE,
                      "DEBUG" = futile.logger::DEBUG,
                      "INFO" = futile.logger::INFO,
                      "WARN" = futile.logger::WARN,
                      "ERROR" = futile.logger::ERROR,
                      "FATAL" = futile.logger::FATAL,
                      futile.logger::INFO)

  # Définir le niveau de log
  futile.logger::flog.threshold(log_level)

  # Définir l'appender console
  futile.logger::flog.appender(futile.logger::appender.console())

  futile.logger::flog.layout(futile.logger::layout.format(
    "[~t] [R] [~l] (~n) ~m"
  ))

  # Ajouter appender fichier si spécifié
  if (!is.null(log_file)) {
    # Ajouter l'appender fichier
    futile.logger::flog.appender(futile.logger::appender.file(log_file), name = "file")
  }

  # Configurer également le logging Python si reticulate est disponible
  if (requireNamespace("reticulate", quietly = TRUE)) {
    tryCatch({
      # Vérifier que Python est disponible
      if (reticulate::py_available()) {
        # Importer le module fin_logger
        fin_logger <- reticulate::import("fin_logger", convert = FALSE)

        # Configurer le niveau de log Python global
        if (reticulate::py_has_attr(fin_logger, "set_all_loggers_level")) {
          fin_logger$set_all_loggers_level(level)
          futile.logger::flog.debug(paste("Python logging level set to", level))
        }

        # Configurer logging Python avec fichier
        if (!is.null(log_file) && reticulate::py_has_attr(fin_logger, "setup_logging")) {
          fin_logger$setup_logging(level, log_file)
          futile.logger::flog.debug(paste("Python log file set to", log_file))
        }

        # Configurer spécifiquement ib_insync si demandé
        if (control_ibinsync && reticulate::py_has_attr(fin_logger, "configure_ibinsync_logging")) {
          fin_logger$configure_ibinsync_logging(ibinsync_level)
          futile.logger::flog.debug(paste("ib_insync logging level set to", ibinsync_level))
        }
      } else {
        futile.logger::flog.warn("Python is not available")
      }
    }, error = function(e) {
      # Ne pas bloquer en cas d'erreur avec Python
      futile.logger::flog.warn(paste("Python logging configuration skipped:", e$message))
    })
  }

  # Log un message de confirmation
  futile.logger::flog.info(paste("Logging initialized at level", level))

  invisible(TRUE)
}

#' @title Log a message with context
#'
#' @description Log a message at the specified level with optional context
#'
#' @param level Logging level (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)
#' @param message Message to log
#' @param context Named list of contextual information
#' @param module Module name for categorizing logs
#' @return Invisibly returns TRUE
#' @export
tdata_log <- function(level, message, context = NULL, module = "tdata") {
  # Format context if provided
  if (!is.null(context) && is.list(context) && length(context) > 0) {
    context_str <- paste(
      sapply(names(context), function(n) {
        # Obtenir la valeur pour cette clé
        val <- context[[n]]

        # Si la valeur est un vecteur de longueur > 1, le convertir en chaîne
        if (length(val) > 1) {
          val <- paste0("(", paste(val, collapse = ","), ")")
        }

        # Retourner "clé=valeur"
        paste0(n, "=", val)
      }),
      collapse = ", "
    )
    full_message <- paste0(message, " {", context_str, "}")
  } else {
    full_message <- message
  }

  # Log at appropriate level
  switch(toupper(level),
         "TRACE" = futile.logger::flog.trace(full_message, name = module),
         "DEBUG" = futile.logger::flog.debug(full_message, name = module),
         "INFO" = futile.logger::flog.info(full_message, name = module),
         "WARN" = futile.logger::flog.warn(full_message, name = module),
         "ERROR" = futile.logger::flog.error(full_message, name = module),
         "FATAL" = futile.logger::flog.fatal(full_message, name = module),
         futile.logger::flog.info(full_message, name = module))

  ###Log aussi dans Python si disponible - mais doublon sur la Console
  # if (requireNamespace("reticulate", quietly = TRUE)) {
  #   tryCatch({
  #     if (reticulate::py_available()) {
  #       fin_logger <- reticulate::import("fin_logger", convert = FALSE)
  #       if (reticulate::py_has_attr(fin_logger, "log_with_context")) {
  #         fin_logger$log_with_context(level, message, context, module)
  #       }
  #     }
  #   }, error = function(e) {
  #     # Ignorer les erreurs Python lors du logging
  #   })
  # }

  invisible(TRUE)
}

#' @title Log a debug message
#'
#' @description Convenience function to log at DEBUG level
#'
#' @param message Message to log
#' @param context Named list of contextual information
#' @param module Module name for categorizing logs
#' @return Invisibly returns TRUE
#' @export
tdata_log_debug <- function(message, context = NULL, module = "tdata") {
  tdata_log("DEBUG", message, context, module)
}

#' @title Log an info message
#'
#' @description Convenience function to log at INFO level
#'
#' @param message Message to log
#' @param context Named list of contextual information
#' @param module Module name for categorizing logs
#' @return Invisibly returns TRUE
#' @export
tdata_log_info <- function(message, context = NULL, module = "tdata") {
  tdata_log("INFO", message, context, module)
}

#' @title Log a warning message
#'
#' @description Convenience function to log at WARN level
#'
#' @param message Message to log
#' @param context Named list of contextual information
#' @param module Module name for categorizing logs
#' @return Invisibly returns TRUE
#' @export
tdata_log_warn <- function(message, context = NULL, module = "tdata") {
  tdata_log("WARN", message, context, module)
}

#' @title Log an error message
#'
#' @description Convenience function to log at ERROR level
#'
#' @param message Message to log
#' @param context Named list of contextual information
#' @param module Module name for categorizing logs
#' @return Invisibly returns TRUE
#' @export
tdata_log_error <- function(message, context = NULL, module = "tdata") {
  tdata_log("ERROR", message, context, module)
}

#' @title Log an exception with details
#'
#' @description Log an error with exception details
#'
#' @param e Error object from tryCatch
#' @param message Additional message to log
#' @param context Extra context information
#' @param module Module name for categorizing logs
#' @return Invisibly returns TRUE
#' @export
tdata_log_exception <- function(e, message = "Exception occurred", context = NULL, module = "tdata") {
  # Add exception details to context
  err_context <- c(
    list(
      error_message = e$message,
      error_call = as.character(e$call)[1]
    ),
    context
  )

  # Log error with context
  tdata_log("ERROR", message, err_context, module)

  # Get stack trace if available
  if (requireNamespace("utils", quietly = TRUE)) {
    stack_trace <- utils::capture.output(utils::traceback(5))
    if (length(stack_trace) > 1) { # Skip first line which is just "No traceback available"
      stack_str <- paste(stack_trace[-1], collapse = "\n")
      tdata_log("DEBUG", "Stack trace", list(trace = stack_str), module)
    }
  }

  invisible(TRUE)
}

#' @title Create a function wrapper that logs execution time
#'
#' @description Wrap a function to log its execution time
#'
#' @param func Function to wrap
#' @param module Module name for log
#' @param func_name Optional function name (defaults to function name)
#' @return Wrapped function with timing
#' @export
tdata_log_execution_time <- function(func, module = "tdata", func_name = NULL) {
  # Get function name if not provided
  if (is.null(func_name)) {
    func_name <- deparse(substitute(func))
  }

  # Create wrapper function
  function(...) {
    # Start timer
    start_time <- Sys.time()

    # Try to execute function
    result <- tryCatch({
      # Log start
      tdata_log_debug(paste("Starting", func_name), NULL, module)

      # Call original function
      func_result <- func(...)

      # Calculate duration
      end_time <- Sys.time()
      duration_ms <- round(as.numeric(difftime(end_time, start_time, units = "secs")) * 1000, 2)

      # Log completion
      tdata_log_debug(paste("Function", func_name, "completed"),
                      list(duration_ms = duration_ms), module)

      # Return result
      func_result
    },
    error = function(e) {
      # Calculate duration on error
      end_time <- Sys.time()
      duration_ms <- round(as.numeric(difftime(end_time, start_time, units = "secs")) * 1000, 2)

      # Log error with duration
      tdata_log_exception(e, paste("Error in", func_name),
                          list(duration_ms = duration_ms), module)

      # Re-throw error
      stop(e)
    })

    return(result)
  }
}


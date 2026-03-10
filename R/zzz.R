#' @title Package startup functions for Tdata


#' @title Python module for Tdata
#' @description Access to tdata_py Python functionality (lazy-loaded via active binding).
#'   Accessing \code{tdata_py} for the first time triggers Python initialization.
#' @name tdata_py
#' @export
NULL

# Internal mutable state for lazy Python init (environment contents stay mutable after namespace lock)
.tdata_state <- new.env(parent = emptyenv())
.tdata_state$value <- NULL
.tdata_state$initialized <- FALSE

#' @title Setup Tdata package in .onLoad
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onLoad <- function(libname, pkgname) {

  # Skip during R CMD check
  if (Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != "") {
    return(invisible())
  }

  # Global logging
  .init_package_logging(pkgname)

  # Create active binding: accessing tdata_py triggers lazy Python init
  ns <- asNamespace(pkgname)
  # Remove any pre-existing regular binding (e.g. from old installed version)
  if (exists("tdata_py", envir = ns, inherits = FALSE) &&
      !bindingIsActive("tdata_py", ns)) {
    rm("tdata_py", envir = ns)
  }
  makeActiveBinding("tdata_py", function() {
    if (!.tdata_state$initialized) {
      .tdata_state$initialized <- .init_python_environment(pkgname)
    }
    .tdata_state$value
  }, env = ns)

  invisible()
}


#' @title Initialize minimal logging
#' @keywords internal
.init_package_logging <- function(pkgname) {

  pkg_config <- Tlogger::get_config_namespace(namespace = pkgname)

  # Validate config values are scalar
  console_level <- pkg_config$console_level
  file_level <- pkg_config$file_level

  # Ensure single values (take first if vector)
  if (length(console_level) != 1) {
    console_level <- console_level[1]
    warning("console_level was not scalar, using first value: ", console_level)
  }

  if (length(file_level) != 1) {
    file_level <- file_level[1]
    warning("file_level was not scalar, using first value: ", file_level)
  }

  # Provide defaults if NULL/missing
  console_level <- console_level %||% "INFO"
  file_level <- file_level %||% "DEBUG"

  Tlogger::setup_namespace_logging(
      pkgname,
      console_level = console_level,
      file_level = file_level
  )

  invisible(TRUE)

}

#' @title Add directory to Python path if not already present
#' @param python_dir Directory to add to Python path
#' @keywords internal
add_python_path_if_needed <- function(python_dir) {
  reticulate::py_run_string(sprintf("
import sys
if '%s' not in sys.path:
    sys.path.insert(0, '%s')
", python_dir, python_dir))
}

#' @title Initialize Python environment
#' @keywords internal
.init_python_environment <- function(pkgname) {

  tryCatch({
    # Set RETICULATE_PYTHON environment variable directly if you know the path
    ### In Renviron.site: RETICULATE_PYTHON="C:/Users/aldoh/miniconda3/envs/r-reticulate/python.exe"

    # Initialize Python environment quietly
    python_initialized <- FALSE
    methods <- list(
      function() reticulate::use_condaenv("r-reticulate", required = FALSE),
      function() reticulate::use_virtualenv("r-reticulate", required = FALSE),
      function() reticulate::use_python(reticulate::py_discover_config()$python, required = FALSE)
    )

    for (method in methods) {
      if (!python_initialized) {
        tryCatch({
          method()
          python_initialized <- TRUE
          break
        }, error = function(e) {
          # Silently continue to next method
        })
      }
    }

    if (!python_initialized) {
      logger::log_error("Could not initialize any Python environment", namespace="Tdata")
      return(FALSE)
    }

    # Find Python directory
    python_dir <- system.file("python", package = pkgname)
    if (!dir.exists(python_dir)) {
      logger::log_error(sprintf("Python directory not found in %s", path), namespace="Tdata")
      return(FALSE)
    }

    # If necessary add python dir to Python path so we can import tdata_py
    add_python_path_if_needed(python_dir)

    # Force reload modules
    reticulate::py_run_string('
import sys
modules_to_reload = ["tdata_py"]
for name in list(sys.modules.keys()):
    if name.startswith("tdata_py."):  ### searches for all tdata_py submodules
        modules_to_reload.append(name)
for module in modules_to_reload:  ### This removes modules_to_reload from cache - then import must be done
    if module in sys.modules:
        del sys.modules[module]
')

    # CRITICAL FIX: Set Python locale to use period decimals (not comma)
    # French/Swiss locale causes JSON parse errors when Python data is sent to browser
    logger::log_info("Setting Python LC_NUMERIC locale to C (period decimals)", namespace="Tdata")
    tryCatch({
      reticulate::py_run_string('
import locale
old_locale = locale.getlocale(locale.LC_NUMERIC)
locale.setlocale(locale.LC_NUMERIC, "C")
print(f"Python LC_NUMERIC changed from {old_locale} to C")
')
    }, error = function(e) {
      logger::log_warn(sprintf("Could not set Python locale: %s", e$message), namespace="Tdata")
    })

    # Import the package
    tdata_py <- reticulate::import("tdata_py", delay_load = TRUE)

    # Assign to the actual package namespace
    ## This is the way to go so it works with both library(Tdata) and box::use(Tdata[tdata_py])
    .tdata_state$value <- tdata_py

    # Verify assignment
    logger::log_debug(sprintf("tdata_py assigned to namespace: %s",
                        environmentName(asNamespace(pkgname))), namespace="Tdata")

    # Log success
    logger::log_info("Python environment initialized successfully, not loaded yet", namespace="Tdata")

        return(TRUE)

  }, error = function(e) {
    # Then use directly Tbasics with specified module
    logger::log_error(sprintf("Failed to initialize Python environment: %s",
                           e$message), namespace="Tdata")

    # Display detailed Python error
    tryCatch({
      reticulate::py_last_error()
    }, error = function(e2) {
      # If even py_last_error fails then ignore
    })

    return(FALSE)
  })
}

#' @title Package attach message display
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Skip during R CMD check
  if (Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != "") {
    return(invisible())
  }

  # Messages de bienvenue
  packageStartupMessage("Welcome ", pkgname, " version ", utils::packageVersion(pkgname), " !")

  invisible()
}

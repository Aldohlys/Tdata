#' @title Package startup functions for Tdata


#' @title Python module for Tdata
#' @description Access to tdata_py Python functionality
#' @export
tdata_py <- NULL  # Will be assigned in .onLoad()

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

  # Python logging
  .init_python_environment(pkgname)

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
      t_log_error("Could not initialize any Python environment")
      return(FALSE)
    }

    # Find Python directory
    python_dir <- system.file("python", package = pkgname)
    if (!dir.exists(python_dir)) {
      t_log_error(sprintf("Python directory not found in %s", path))
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
    t_log_info("Setting Python LC_NUMERIC locale to C (period decimals)")
    tryCatch({
      reticulate::py_run_string('
import locale
old_locale = locale.getlocale(locale.LC_NUMERIC)
locale.setlocale(locale.LC_NUMERIC, "C")
print(f"Python LC_NUMERIC changed from {old_locale} to C")
')
    }, error = function(e) {
      t_log_warn(sprintf("Could not set Python locale: %s", e$message))
    })

    # Import the package
    tdata_py <- reticulate::import("tdata_py", delay_load = TRUE)

    # Assign to the actual package namespace
    ## This is the way to go so it works with both library(Tdata) and box::use(Tdata[tdata_py])
    assign("tdata_py", tdata_py, envir = asNamespace(pkgname))

    # Verify assignment
    t_log_debug(sprintf("tdata_py assigned to namespace: %s",
                        environmentName(asNamespace(pkgname))))

    # Log success
    t_log_info("Python environment initialized successfully, not loaded yet")

        return(TRUE)

  }, error = function(e) {
    # Then use directly Tbasics with specified module
    t_log_error(sprintf("Failed to initialize Python environment: %s",
                           e$message))

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

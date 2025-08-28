#' @title Package startup functions for Tdata

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
      function() reticulate::use_virtualenv("r-reticulate", required = FALSE),
      function() reticulate::use_condaenv("r-reticulate", required = FALSE),
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

    # Import the package
    tdata_py <- reticulate::import("tdata_py", delay_load = FALSE)

    # Debug: Check available attributes (optionnel avec logging)
    available_attrs <- reticulate::py_list_attributes(tdata_py)
    t_log_debug(sprintf("Available attributes %s in tdata_py for %s",
                           paste(available_attrs, collapse = ", "),
                           pkgname))
    # Add this debug line to your .onLoad to confirm assignment location
    t_log_debug(sprintf("Assigning tdata_py to %s %s",
                         class(parent.env(environment())),
                        environmentName(parent.env(environment()))))

    # Assign to package environment
    assign("tdata_py", tdata_py, envir = parent.env(environment()))

    # Log success
    t_log_info("Python environment initialized successfully")

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

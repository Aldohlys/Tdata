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
    # Initialize logging namespace for Tdata
    tryCatch({
      # Make sure root logger is initialized
      if (is.null(futile.logger::flog.threshold())) {
        Tbasics::t_init_logging()
      }

      # Configure Tdata namespace for proper module display
      root_threshold <- futile.logger::flog.threshold()
      root_layout <- futile.logger::flog.layout()
      root_appender <- futile.logger::flog.appender()

      futile.logger::flog.threshold(root_threshold, name = pkgname)
      futile.logger::flog.layout(root_layout, name = pkgname)
      futile.logger::flog.appender(root_appender, name = pkgname)

      # Log initialization
      Tbasics::t_log_info(paste(pkgname, "logging configured"), module = pkgname)
    }, error = function(e) {
      warning(sprintf("Failed to configure %s logging: %s", pkgname, e$message))
    })
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
      Tbasics::t_log_error("Could not initialize any Python environment", module = pkgname)
      return(FALSE)
    }

    # Find Python directory
    python_dir <- system.file("python", package = pkgname)
    if (!dir.exists(python_dir)) {
      Tbasics::t_log_error("Python directory not found", list(path = python_dir), module = pkgname)
      return(FALSE)
    }

    # If necessary add python dir to Python path so we can import tdata_py
    add_python_path_if_needed(python_dir)

    # Force reload modules
    reticulate::py_run_string('
import sys
modules_to_reload = ["tdata_py"]
for name in list(sys.modules.keys()):
    if name.startswith("tdata_py."):
        modules_to_reload.append(name)
for module in modules_to_reload:
    if module in sys.modules:
        del sys.modules[module]
')

    # Import the package
    tdata_py <- reticulate::import("tdata_py", delay_load = FALSE)

    # Debug: Check available attributes (optionnel avec logging)
    available_attrs <- reticulate::py_list_attributes(tdata_py)
    Tbasics::t_log_debug("Available attributes in tdata_py",
                           list(attributes = paste(available_attrs, collapse = ", ")),
                           module = pkgname)

    # Assign to package environment
    assign("tdata_py", tdata_py, envir = parent.env(environment()))

    # Log success - UTILISER TBASICS DIRECTEMENT AVEC MODULE
    Tbasics::t_log_info("Python environment initialized successfully", module = pkgname)
    Tbasics::t_log_info("Tdata package loaded successfully", module = pkgname)

    return(TRUE)

  }, error = function(e) {
    # Utiliser Tbasics directement avec module spécifié
    Tbasics::t_log_error("Failed to initialize Python environment",
                           list(error = e$message),
                           module = pkgname)

    # Afficher l'erreur Python détaillée
    tryCatch({
      reticulate::py_last_error()
    }, error = function(e2) {
      # Si même py_last_error échoue, ignorer
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

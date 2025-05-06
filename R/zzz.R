# .onLoad <- function(libname, pkgname) {

#
#   ### This is where to find Python by default
#   # First ensure we're using the conda environment
#   reticulate::use_condaenv("r-reticulate")
#
#   reticulate::py_run_file(system.file("python/getContractValue.py", package="Tdata"))
#    ## suppressMessages(.GlobalEnv$mydb <- pool::dbPool(drv = RSQLite::SQLite(),dbname = config::get("DB")))
# }


# Ajouter le chemin Python uniquement s'il n'est pas déjà présent dans sys.path
add_python_path_if_needed <- function(python_dir) {
  # Normaliser le chemin (convertir les backslashes en forward slashes)
  python_dir <- gsub("\\\\", "/", python_dir)

  # Importer sys explicitement avant d'y accéder
  reticulate::py_run_string("import sys")

  # Vérifier si le chemin est déjà dans sys.path
  path_exists <- reticulate::py_eval(sprintf("'%s' in sys.path", python_dir))

  if (!path_exists) {
    # Ajouter le chemin à sys.path car il n'existe pas encore
    reticulate::py_run_string(sprintf("import sys; sys.path.append('%s')", python_dir))
    message("Added Python path: ", python_dir)
    return(TRUE)
  } else {
    message("Python path already exists: ", python_dir)
    return(FALSE)
  }
}

.onLoad <- function(libname, pkgname) {
  tryCatch({
    # Set RETICULATE_PYTHON environment variable directly if you know the path
    if (Sys.getenv("RETICULATE_PYTHON") == "") {
      python_executable <- "C:/Users/aldoh/miniconda3/envs/r-reticulate/python.exe"
      if (file.exists(python_executable)) {
        Sys.setenv(RETICULATE_PYTHON = python_executable)
      }
    }

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
        }, error = function(e) {
          # Silently continue to next method
        })
      }
    }

    if (!python_initialized) {
      stop("Could not initialize any Python environment")
    }

    # Find Python directory
    ### system.file is a devtools shim that works both during package development
    ### and also once package is installed by end user
    ### in both cases it will provide the actual path
    python_dir <- system.file("python", package = pkgname)
    if (!dir.exists(python_dir)) {
      stop("Python directory not found: ", python_dir)
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

    # Debug: Check available attributes
    # available_attrs <- reticulate::py_list_attributes(tdata_py)
    # message("\nAvailable attributes in tdata_py: ", paste(available_attrs, collapse = ", "))

    # Assign to package environment
    assign("tdata_py", tdata_py, envir = parent.env(environment()))

    # Initialiser le package avec le logging par défaut
    # Configurer le logging
    tdata_setup_logger("WARN", daily_log = TRUE, control_ibinsync = TRUE, ibinsync_level = "ERROR")
    tdata_log_info("Tdata package loaded")

  }, error = function(e) {
    warning(sprintf("\nFailed to initialize Python environment: %s", e$message))
    reticulate::py_last_error()
  })
}
#> Start up message will be displayed only when library Tdata is loaded by user, not when calling individual function with ::
#> Hence -onLoad is preferred way
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome Tdata version ", utils::packageVersion(pkgname)," !" )
}


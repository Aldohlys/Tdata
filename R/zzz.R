# .onLoad <- function(libname, pkgname) {
#   ### system.file is a devtools shim that works both during package development
#   ### and also once package is installed by end user
#   ### in both cases it will provide the actual path
#
#   ### This is where to find Python by default
#   # First ensure we're using the conda environment
#   reticulate::use_condaenv("r-reticulate")
#
#   reticulate::py_run_file(system.file("python/getContractValue.py", package="Tdata"))
#    ## suppressMessages(.GlobalEnv$mydb <- pool::dbPool(drv = RSQLite::SQLite(),dbname = config::get("DB")))
# }

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
    python_dir <- system.file("python", package = pkgname)
    if (!dir.exists(python_dir)) {
      stop("Python directory not found: ", python_dir)
    }

    # # Verify tdata_py subdirectory exists
    # tdata_py_dir <- file.path(python_dir, "tdata_py")
    # if (!dir.exists(tdata_py_dir)) {
    #   stop("tdata_py directory not found: ", tdata_py_dir)
    # }
    #
    # # Verify __init__.py exists with correct name
    # init_py_path <- file.path(tdata_py_dir, "__init__.py")
    # if (!file.exists(init_py_path)) {
    #   warning("__init__.py file not found in tdata_py directory. Check for incorrect filenames like *init*.py")
    #
    #   # Try to find and rename *init*.py if it exists
    #   potential_init <- list.files(tdata_py_dir, pattern = "init", full.names = TRUE)
    #   if (length(potential_init) > 0) {
    #     message("Found potential init file: ", potential_init[1])
    #     message("Trying to copy to __init__.py")
    #     file.copy(potential_init[1], init_py_path)
    #   } else {
    #     stop("No init file found in tdata_py directory")
    #   }
    # }

    # Add python dir to Python path so we can import tdata_py
    reticulate::py_run_string(sprintf("import sys; sys.path.append('%s')", python_dir))

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


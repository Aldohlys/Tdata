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
    # This can avoid some shell spawning behavior
    if (Sys.getenv("RETICULATE_PYTHON") == "") {
      # Try to find Python without spawning shells if possible
      if (file.exists("C:/Users/aldoh/miniconda3/envs/r-reticulate/python.exe")) {
        Sys.setenv(RETICULATE_PYTHON = "C:/Users/aldoh/miniconda3/envs/r-reticulate/python.exe")
      }
    }

    # Then check if reticulate can find Python at all
    python_path <- reticulate::py_discover_config(required_module = NULL)
    packageStartupMessage("Python path discovered: ", python_path$python)

    # Try to initialize conda/Python environment
    python_initialized <- FALSE

    # Try using virtualenv first (simpler, often more reliable)
    tryCatch({
      reticulate::use_virtualenv("r-reticulate", required = FALSE)
      python_initialized <- TRUE
      packageStartupMessage("Successfully initialized virtualenv environment")
    }, error = function(e) {
      packageStartupMessage("Virtualenv initialization failed, trying conda...")
    })

    # If virtualenv failed, try conda
    if (!python_initialized) {
      tryCatch({
        reticulate::use_condaenv("r-reticulate", required = FALSE)
        python_initialized <- TRUE
        packageStartupMessage("Successfully initialized conda environment")
      }, error = function(e) {
        packageStartupMessage("Conda initialization failed, trying default Python...")
      })
    }

    # If both failed, try using system Python
    if (!python_initialized) {
      tryCatch({
        reticulate::use_python(python_path$python, required = FALSE)
        python_initialized <- TRUE
        packageStartupMessage("Using system Python")
      }, error = function(e) {
        stop("Could not initialize any Python environment")
      })
    }

    # Once we have a Python environment, load the script
    script_path <- system.file("python/getContractValue.py", package = "Tdata")
    if (!file.exists(script_path)) {
      stop("Required Python script not found: ", script_path)
    }

    # Redirect stdout temporarily to capture and filter Python output
    old_stdout <- reticulate::py_capture_output({
      reticulate::py_run_file(script_path)
    }, type = "stdout")

    # Only display important Python output, filter out connection errors
    filtered_output <- old_stdout
    if (nchar(filtered_output) > 0 && !grepl("refused|connection|API", filtered_output, ignore.case = TRUE)) {
      packageStartupMessage(filtered_output)
    }

  }, error = function(e) {
    warning(sprintf("Failed to initialize Python environment: %s\nFallback procedures may be used.",
                    e$message))
  })

  # Add startup message outside of tryCatch to ensure it's always displayed
  packageStartupMessage("Tdata Python integration initialization completed")
}

#> Start up message will be displayed only when library Tdata is loaded by user, not when calling individual function with ::
#> Hence -onLoad is preferred way
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome Tdata version ", utils::packageVersion(pkgname)," !" )
}


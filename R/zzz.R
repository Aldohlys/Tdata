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
    # First check if reticulate can find Python at all
    python_path <- reticulate::py_discover_config()
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

    reticulate::py_run_file(script_path)

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


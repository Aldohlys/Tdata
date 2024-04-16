.onLoad <- function(libname, pkgname) {
  ### system.file is a devtools shim that works both during package development
  ### and also once package is installed by end user
  ### in both cases it will provide the actual path
  reticulate::py_run_file(system.file("python/getContractValue.py",package="Tdata"))
   ## suppressMessages(.GlobalEnv$mydb <- pool::dbPool(drv = RSQLite::SQLite(),dbname = config::get("DB")))
}

#> Start up message will be displayed only when library Tdata is loaded by user, not when calling individual function with ::
#> Hence -onLoad is preferred way
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome Tdata package!")
}


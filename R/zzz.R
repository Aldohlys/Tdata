.onLoad <- function(libname, pkgname) {
  ### system.file is a devtools shim that works both during package development
  ### and also once package is installed by end user
  ### in both cases it will provide the actual path
  reticulate::py_run_file(system.file("python/getContractValue.py",package="Tutils"))
  .GlobalEnv$mydb <- pool::dbPool(drv = RSQLite::SQLite(),dbname = config::get("DB"))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome Tutils package!")
}

.onExit <- function() {
  pool::poolClose(.GlobalEnv$mydb)
}

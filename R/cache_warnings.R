# Surface stale-cache warnings buffered by the Python layer (TODO #27, Part 1).
#
# When getChains()/getOptionStrikes() find a chain/strike parquet cache older
# than cache_ttl_days, they delete it (forcing a fresh IBKR fetch) and append a
# human-readable note to a module-global buffer in chains_manager.py. Layer 1
# (detect + delete) lives in Python; this is Layer 2 — the R caller drains that
# buffer and surfaces the notes, so a silent at-load-time deletion becomes
# visible instead of going unnoticed.

#' Surface and clear buffered stale-cache warnings from the Python layer
#'
#' Drains the Python stale-cache warning buffer: logs each note (namespace
#' "Tdata") and shows them via [Tbasics::display_message()] — a modal when a
#' Shiny app is running, a console message otherwise — then clears the buffer.
#'
#' Designed to be wired via `on.exit()` in the Tdata functions that fetch
#' option chains/strikes, but safe to call anywhere. It is a no-op when the
#' buffer is empty or the Python helpers are unavailable (older `tdata_py`),
#' and never raises — surfacing a warning must not break the data path.
#'
#' @param tp Optional `tdata_py` module handle. Defaults to the package's lazy
#'   `tdata_py` active binding (the correct choice for all production callers);
#'   exposed so tests can inject a directly-imported module and avoid depending
#'   on the active binding's init state in an isolated test working directory.
#' @return Character vector of surfaced warnings (invisibly); `character(0)`
#'   when nothing was buffered.
#' @export
surface_cache_warnings <- function(tp = NULL) {
  if (is.null(tp)) {
    tp <- tryCatch(tdata_py, error = function(e) NULL)
  }
  if (is.null(tp)) return(invisible(character(0)))

  warnings <- tryCatch(
    as.character(tp$get_stale_warnings()),
    error = function(e) character(0)
  )
  if (length(warnings) == 0) return(invisible(character(0)))

  for (msg in warnings) {
    logger::log_warn(msg, namespace = "Tdata")
  }
  tryCatch(
    Tbasics::display_message(paste(warnings, collapse = "\n")),
    error = function(e) NULL
  )
  tryCatch(tp$clear_stale_warnings(), error = function(e) NULL)

  invisible(warnings)
}

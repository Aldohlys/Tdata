get_euribor_data <- function() {
  # Define the exact ECB data codes for EURIBOR rates
  euribor_codes <- c(
    "1M" = "FM.M.U2.EUR.RT.MM.EURIBOR1MD_.HSTA",
    "3M" = "FM.M.U2.EUR.RT.MM.EURIBOR3MD_.HSTA",
    "6M" = "FM.M.U2.EUR.RT.MM.EURIBOR6MD_.HSTA",
    "12M" = "FM.M.U2.EUR.RT.MM.EURIBOR12MD_.HSTA"
  )

  # Container for results
  results <- list()

  # Fetch each EURIBOR rate
  for (name in names(euribor_codes)) {
    code <- euribor_codes[name]
    cat("Fetching", name, "EURIBOR data...\n")

    # Get data from ECB
    data <- tryCatch({
      ecb::get_data(code)
    }, error = function(e) {
      cat("Error fetching", name, ":", e$message, "\n")
      return(NULL)
    })

    if (!is.null(data)) {
      # Add to results
      results[[name]] <- data
      cat("Success!\n")
    }
  }

  return(results)
}

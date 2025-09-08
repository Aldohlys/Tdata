#' Populate ConvertToCHF table with historical data from 2021
#' Uses getYahooData to fetch historical currency rates
populate_chf_historical_data <- function() {

  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Get active currencies from Currencies table - exclude CHF, USD, and EUR
  # USD handled separately (no YahooName), EUR handled with direct pair
  active_currencies <- DBI::dbGetQuery(conn, "
  SELECT Name, YahooName, DirectConversion
  FROM Currencies
  WHERE Active = 'Yes'
  AND Name NOT IN ('CHF', 'USD', 'EUR')
  AND Name IS NOT NULL
  AND YahooName IS NOT NULL
  AND YahooName != ''
  ") |>
  dplyr::filter(!is.na(Name), !is.na(YahooName), Name != "", YahooName != "") |>
  dplyr::filter(complete.cases(Name, YahooName))

  start_date <- as.Date("2021-01-01")
  end_date <- Sys.Date()

  logger::log_info("Starting CHF historical data population from {start_date} to {end_date}", namespace = "Tdata")

  # Helper function for chunked data fetching
  get_chunked_data <- function(ticker, start_date, end_date, chunk_months = 6) {
    all_data <- data.frame()
    current_date <- start_date

    while (current_date < end_date) {
      # Use base R date arithmetic
      chunk_end <- seq(current_date, length = 2, by = paste(chunk_months, "months"))[2]
      chunk_end <- min(chunk_end, end_date)

      logger::log_info("Fetching {ticker} from {current_date} to {chunk_end}", namespace = "Tdata")

      chunk_data <- getYahooData(ticker,
                                 from_date = current_date,
                                 to_date = chunk_end)

      if (nrow(chunk_data) > 0 && !all(is.na(chunk_data$Adjusted))) {
        all_data <- rbind(all_data, chunk_data)
      }

      current_date <- chunk_end + 1
      Sys.sleep(1)  # Be respectful to Yahoo
    }

    return(all_data)
  }

  # Check which currencies already exist in ConvertToCHF
  existing_currencies <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT currency FROM ConvertToCHF
  ")$currency

  logger::log_info("Existing currencies in ConvertToCHF: {paste(existing_currencies, collapse=', ')}", namespace = "Tdata")

  # Get CHF/USD rates using chunked approach (only if USD not already present)
  if (!"USD" %in% existing_currencies) {
    logger::log_info("Fetching CHF/USD historical data in chunks...", namespace = "Tdata")
    chf_usd_data <- get_chunked_data("CHFUSD=X", start_date, end_date)

    if (nrow(chf_usd_data) == 0) {
      stop("Cannot retrieve CHF/USD historical data!")
    }

    logger::log_info("Retrieved {nrow(chf_usd_data)} CHF/USD data points", namespace = "Tdata")

    # Process USD to CHF conversion with validation
    usd_to_chf <- chf_usd_data |>
      dplyr::filter(!is.na(Adjusted), Adjusted > 0) |>  # Remove invalid rates
      dplyr::mutate(
        currency = "USD",
        chf_value = round(1 / Adjusted, 4),  # Invert CHF/USD to get USD/CHF
        date = as.integer(format(date, "%Y%m%d"))
      ) |>
      dplyr::filter(!is.na(chf_value), chf_value > 0) |>  # Validate CHF values
      dplyr::select(date, currency, chf_value)

    # Insert USD rates
    if (nrow(usd_to_chf) > 0) {
      safe_db_append(conn, "ConvertToCHF", usd_to_chf)
      logger::log_info("Inserted {nrow(usd_to_chf)} USD to CHF records", namespace = "Tdata")
    }
  } else {
    logger::log_info("USD records already exist, skipping USD processing", namespace = "Tdata")
    # Still need CHF/USD data for cross-rate calculations
    chf_usd_data <- get_chunked_data("CHFUSD=X", start_date, end_date)
  }

  # Process EUR with direct EUR/CHF pair using chunked approach (only if EUR not already present)
  if (!"EUR" %in% existing_currencies) {
    logger::log_info("Fetching EUR/CHF historical data in chunks...", namespace = "Tdata")
    eur_data <- get_chunked_data("EURCHF=X", start_date, end_date)

    if (nrow(eur_data) > 0) {
      logger::log_info("Retrieved {nrow(eur_data)} EUR/CHF data points", namespace = "Tdata")

      eur_to_chf <- eur_data |>
        dplyr::filter(!is.na(Adjusted), Adjusted > 0) |>  # Remove invalid rates
        dplyr::mutate(
          currency = "EUR",
          chf_value = round(Adjusted, 4),  # Direct EUR/CHF rate
          date = as.integer(format(date, "%Y%m%d"))
        ) |>
        dplyr::filter(!is.na(chf_value), chf_value > 0) |>  # Validate CHF values
        dplyr::select(date, currency, chf_value)

      if (nrow(eur_to_chf) > 0) {
        safe_db_append(conn, "ConvertToCHF", eur_to_chf)
        logger::log_info("Inserted {nrow(eur_to_chf)} EUR to CHF records (direct pair)", namespace = "Tdata")
      }
    } else {
      logger::log_warn("No EUR/CHF data available", namespace = "Tdata")
    }
  } else {
    logger::log_info("EUR records already exist, skipping EUR processing", namespace = "Tdata")
  }

  # Process remaining currencies via USD cross-rates
  # Active currencies now only contains CAD, HKD, etc. (not CHF, USD, EUR)

  if (nrow(active_currencies) == 0) {
    logger::log_info("No additional currencies to process via cross-rates", namespace = "Tdata")
  } else {
    logger::log_info("Processing {nrow(active_currencies)} additional currencies via USD cross-rates", namespace = "Tdata")
  }

  for (i in 1:nrow(active_currencies)) {
    # Skip rows with any NA values immediately
    if (any(is.na(active_currencies[i, ]))) {
      next
    }

    curr_name <- active_currencies$Name[i]
    yahoo_name <- active_currencies$YahooName[i]
    direct_conversion <- active_currencies$DirectConversion[i]

    # Additional safety check
    if (any(is.na(c(curr_name, yahoo_name))) || any(c(curr_name, yahoo_name) == "")) {
      logger::log_warn("Skipping invalid currency entry", namespace = "Tdata")
      next
    }

    logger::log_info("Processing {curr_name} ({yahoo_name})...", namespace = "Tdata")

    # Get historical data for this currency using chunked approach
    currency_data <- tryCatch({
      get_chunked_data(yahoo_name, start_date, end_date)
    }, error = function(e) {
      logger::log_warn("Error fetching data for {curr_name}: {e$message}", namespace = "Tdata")
      data.frame()  # Return empty data frame on error
    })

    if (nrow(currency_data) == 0) {
      logger::log_warn("No data found for {curr_name}", namespace = "Tdata")
      next
    }

    logger::log_info("Retrieved {nrow(currency_data)} data points for {curr_name}", namespace = "Tdata")

    # Convert to CHF via USD cross-rate with validation
    currency_chf <- currency_data |>
      dplyr::filter(!is.na(Adjusted), Adjusted > 0) |>  # Remove invalid rates
      dplyr::mutate(date_int = as.integer(format(date, "%Y%m%d"))) |>
      # Join with CHF/USD rates
      dplyr::inner_join(
        chf_usd_data |>
          dplyr::filter(!is.na(Adjusted), Adjusted > 0) |>
          dplyr::mutate(date_int = as.integer(format(date, "%Y%m%d"))) |>
          dplyr::select(date_int, chf_usd_rate = Adjusted),
        by = "date_int"
      ) |>
      dplyr::mutate(
        currency = curr_name,
        # Apply inversion based on DirectConversion field
        usd_rate = dplyr::case_when(
          direct_conversion == "No" ~ 1 / Adjusted,  # Invert if DirectConversion = No
          TRUE ~ Adjusted  # Use direct rate if DirectConversion = Yes
        ),
        chf_value = round(usd_rate / chf_usd_rate, 4),  # Convert via USD cross-rate
        date = date_int
      ) |>
      dplyr::filter(!is.na(chf_value), chf_value > 0, is.finite(chf_value)) |>  # Validate results
      dplyr::select(date, currency, chf_value)

    # Insert this currency's data
    if (nrow(currency_chf) > 0) {
      safe_db_append(conn, "ConvertToCHF", currency_chf)
      logger::log_info("Inserted {nrow(currency_chf)} {curr_name} to CHF records", namespace = "Tdata")
    } else {
      logger::log_warn("No valid CHF conversion data for {curr_name}", namespace = "Tdata")
    }

    # Small delay to be respectful to Yahoo
    Sys.sleep(0.5)
  }

  # Summary report
  summary <- DBI::dbGetQuery(conn, "
    SELECT
      currency,
      COUNT(*) as record_count,
      MIN(date) as first_date,
      MAX(date) as last_date
    FROM ConvertToCHF
    GROUP BY currency
    ORDER BY currency
  ")

  print(summary)
  logger::log_info("CHF historical data population completed!", namespace = "Tdata")
}

# Execute the population (uncomment to run)
# populate_chf_historical_data()

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
  get_chunked_data <- function(ticker, start_date, end_date, chunk_months = 1) {
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

      #Chunk validation in get_chunked_data:
      if (nrow(chunk_data) > 0 && !all(is.na(chunk_data$Adjusted))) {
        logger::log_info("Chunk success: {nrow(chunk_data)} rows", namespace = "Tdata")
        all_data <- rbind(all_data, chunk_data)
      } else {
        logger::log_warn("Chunk failed or empty: {current_date} to {chunk_end}", namespace = "Tdata")
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


# After the main populate function, manually fetch the problem period:
manual_fetch_problem_dates <- function() {
  problem_start <- as.Date("2024-07-08")
  problem_end <- as.Date("2025-01-08")

  # Try smaller chunks (1 month each) for this problem period
  current_date <- problem_start
  while (current_date < problem_end) {
    chunk_end <- seq(current_date, length = 2, by = "1 months")[2]
    chunk_end <- min(chunk_end, problem_end)

    logger::log_info("Retry problem chunk: {current_date} to {chunk_end}", namespace = "Tdata")

    chunk_data <- getYahooData("USDHKD=X", from_date = current_date, to_date = chunk_end)
    if (nrow(chunk_data) > 0) {
      logger::log_info("Success: {nrow(chunk_data)} rows", namespace = "Tdata")
    }

    current_date <- chunk_end + 1
    Sys.sleep(2)  # Longer delay
  }
}

fill_missing_march_april_2025 <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  problem_start <- as.Date("2025-03-19")
  problem_end <- as.Date("2025-04-19")

  # Get CHF/USD data for cross-rate calculations
  chf_usd_ref <- DBI::dbGetQuery(conn, "
    SELECT date, chf_value as usd_to_chf_rate
    FROM ConvertToCHF
    WHERE currency = 'USD'
    AND date BETWEEN 20250318 AND 20250420
  ")

  currencies <- c("CHFUSD=X", "USDCAD=X", "USDHKD=X")

  for (ticker in currencies) {
    logger::log_info("Processing {ticker} for March-April 2025 gap", namespace = "Tdata")

    # Fetch data day by day for this period
    current_date <- problem_start
    all_data <- data.frame()

    while (current_date <= problem_end) {
      daily_data <- getYahooData(ticker,
                                 from_date = current_date,
                                 to_date = current_date)

      if (nrow(daily_data) > 0 && !is.na(daily_data$Adjusted[1])) {
        all_data <- rbind(all_data, daily_data)
        logger::log_info("Day {current_date}: success", namespace = "Tdata")
      }

      current_date <- current_date + 1
      Sys.sleep(0.5)  # Small delay between daily requests
    }

    if (nrow(all_data) > 0) {
      # Process the data based on ticker
      if (ticker == "CHFUSD=X") {
        # USD to CHF
        processed_data <- all_data |>
          dplyr::mutate(
            currency = "USD",
            chf_value = round(1 / Adjusted, 4),
            date = as.integer(format(date, "%Y%m%d"))
          ) |>
          dplyr::select(date, currency, chf_value)

      } else {
        # CAD or HKD via cross-rate
        curr_name <- if (ticker == "USDCAD=X") "CAD" else "HKD"
        direct_conversion <- "No"  # Both need inversion

        processed_data <- all_data |>
          dplyr::mutate(date_int = as.integer(format(date, "%Y%m%d"))) |>
          dplyr::left_join(chf_usd_ref, by = c("date_int" = "date")) |>
          dplyr::filter(!is.na(usd_to_chf_rate)) |>
          dplyr::mutate(
            currency = curr_name,
            usd_rate = 1 / Adjusted,  # Invert since DirectConversion = "No"
            chf_value = round(usd_rate * usd_to_chf_rate, 4),
            date = date_int
          ) |>
          dplyr::select(date, currency, chf_value)
      }

      # Insert the data
      safe_db_append(conn, "ConvertToCHF", processed_data)
      logger::log_info("Inserted {nrow(processed_data)} {ticker} records for March-April gap", namespace = "Tdata")
    }
  }
}

fill_usd_march_april_gap <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  problem_start <- as.Date("2025-03-19")
  problem_end <- as.Date("2025-04-19")

  logger::log_info("Filling USD data for March-April 2025 gap", namespace = "Tdata")

  current_date <- problem_start
  all_usd_data <- data.frame()

  while (current_date <= problem_end) {
    # Skip weekends
    if (weekdays(current_date) %in% c("Saturday", "Sunday")) {
      current_date <- current_date + 1
      next
    }

    daily_data <- tryCatch({
      getYahooData("CHFUSD=X",
                   from_date = current_date,
                   to_date = current_date)
    }, error = function(e) {
      logger::log_warn("Failed to fetch {current_date}: {e$message}", namespace = "Tdata")
      data.frame()
    })

    if (nrow(daily_data) > 0 && !is.na(daily_data$Adjusted[1])) {
      all_usd_data <- rbind(all_usd_data, daily_data)
      logger::log_info("USD {current_date}: success ({daily_data$Adjusted[1]})", namespace = "Tdata")
    } else {
      logger::log_warn("USD {current_date}: no data", namespace = "Tdata")
    }

    current_date <- current_date + 1
    Sys.sleep(2)  # 2-second delay between requests
  }

  # Process USD data
  if (nrow(all_usd_data) > 0) {
    usd_processed <- all_usd_data |>
      dplyr::filter(!is.na(Adjusted), Adjusted > 0) |>
      dplyr::mutate(
        currency = "USD",
        chf_value = round(1 / Adjusted, 4),  # Invert CHF/USD to get USD/CHF
        date = as.integer(format(date, "%Y%m%d"))
      ) |>
      dplyr::select(date, currency, chf_value)

    safe_db_append(conn, "ConvertToCHF", usd_processed)
    logger::log_info("Inserted {nrow(usd_processed)} USD records", namespace = "Tdata")
  }

  # Check coverage
  coverage <- DBI::dbGetQuery(conn, "
    SELECT COUNT(*) as usd_count
    FROM ConvertToCHF
    WHERE currency = 'USD'
    AND date BETWEEN 20250319 AND 20250419
  ")

  logger::log_info("USD coverage for March-April: {coverage$usd_count} records", namespace = "Tdata")
}

fill_cad_march_april_gap <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Get USD reference data for the period
  usd_ref <- DBI::dbGetQuery(conn, "
    SELECT date, chf_value as usd_chf_rate
    FROM ConvertToCHF
    WHERE currency = 'USD'
    AND date BETWEEN 20250319 AND 20250419
  ")

  problem_start <- as.Date("2025-03-19")
  problem_end <- as.Date("2025-04-19")

  logger::log_info("Filling CAD data for March-April 2025 gap", namespace = "Tdata")

  current_date <- problem_start
  all_cad_data <- data.frame()

  while (current_date <= problem_end) {
    if (weekdays(current_date) %in% c("Saturday", "Sunday")) {
      current_date <- current_date + 1
      next
    }

    daily_data <- tryCatch({
      getYahooData("USDCAD=X", from_date = current_date, to_date = current_date)
    }, error = function(e) {
      logger::log_warn("Failed to fetch CAD {current_date}: {e$message}", namespace = "Tdata")
      data.frame()
    })

    if (nrow(daily_data) > 0 && !is.na(daily_data$Adjusted[1])) {
      all_cad_data <- rbind(all_cad_data, daily_data)
      logger::log_info("CAD {current_date}: success ({daily_data$Adjusted[1]})", namespace = "Tdata")
    }

    current_date <- current_date + 1
    Sys.sleep(2)
  }

  # Process CAD data with USD cross-rates
  if (nrow(all_cad_data) > 0) {
    cad_processed <- all_cad_data |>
      dplyr::mutate(date_int = as.integer(format(date, "%Y%m%d"))) |>
      dplyr::inner_join(usd_ref, by = c("date_int" = "date")) |>
      dplyr::mutate(
        currency = "CAD",
        usd_rate = 1 / Adjusted,  # Invert USDCAD since DirectConversion = "No"
        chf_value = round(usd_rate * usd_chf_rate, 4),
        date = date_int
      ) |>
      dplyr::select(date, currency, chf_value)

    safe_db_append(conn, "ConvertToCHF", cad_processed)
    logger::log_info("Inserted {nrow(cad_processed)} CAD records", namespace = "Tdata")
  }
}

fill_hkd_march_april_gap <- function() {
  # Similar structure for HKD using "USDHKD=X"
  # Same logic but with currency = "HKD"

  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Get USD reference data for the period
  usd_ref <- DBI::dbGetQuery(conn, "
    SELECT date, chf_value as usd_chf_rate
    FROM ConvertToCHF
    WHERE currency = 'USD'
    AND date BETWEEN 20250319 AND 20250419
  ")

  problem_start <- as.Date("2025-03-19")
  problem_end <- as.Date("2025-04-19")

  logger::log_info("Filling HKD data for March-April 2025 gap", namespace = "Tdata")

  current_date <- problem_start
  all_hkd_data <- data.frame()

  while (current_date <= problem_end) {
    if (weekdays(current_date) %in% c("Saturday", "Sunday")) {
      current_date <- current_date + 1
      next
    }

    daily_data <- tryCatch({
      getYahooData("USDHKD=X", from_date = current_date, to_date = current_date)
    }, error = function(e) {
      logger::log_warn("Failed to fetch HKD {current_date}: {e$message}", namespace = "Tdata")
      data.frame()
    })

    if (nrow(daily_data) > 0 && !is.na(daily_data$Adjusted[1])) {
      all_hkd_data <- rbind(all_hkd_data, daily_data)
      logger::log_info("HKD {current_date}: success ({daily_data$Adjusted[1]})", namespace = "Tdata")
    }

    current_date <- current_date + 1
    Sys.sleep(2)
  }

  # Process HKD data with USD cross-rates
  if (nrow(all_hkd_data) > 0) {
    hkd_processed <- all_hkd_data |>
      dplyr::mutate(date_int = as.integer(format(date, "%Y%m%d"))) |>
      dplyr::inner_join(usd_ref, by = c("date_int" = "date")) |>
      dplyr::mutate(
        currency = "HKD",
        usd_rate = 1 / Adjusted,  # Invert USDHKD since DirectConversion = "No"
        chf_value = round(usd_rate * usd_chf_rate, 4),
        date = date_int
      ) |>
      dplyr::select(date, currency, chf_value)

    safe_db_append(conn, "ConvertToCHF", hkd_processed)
    logger::log_info("Inserted {nrow(hkd_processed)} HKD records", namespace = "Tdata")
  }

}

fill_eur_march_april_gap <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), config::get("DB"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  problem_start <- as.Date("2024-07-05")
  problem_end <- as.Date("2025-07-08")

  logger::log_info("Filling EUR data for March-April 2025 gap with weekly chunks", namespace = "Tdata")

  current_date <- problem_start
  all_eur_data <- data.frame()

  while (current_date <= problem_end) {
    # Weekly chunks (7 days)
    chunk_end <- min(current_date + 6, problem_end)

    logger::log_info("Fetching EURCHF=X from {current_date} to {chunk_end}", namespace = "Tdata")

    weekly_data <- tryCatch({
      getYahooData("EURCHF=X",
                   from_date = current_date,
                   to_date = chunk_end)
    }, error = function(e) {
      logger::log_warn("Failed to fetch EUR week {current_date}-{chunk_end}: {e$message}", namespace = "Tdata")
      data.frame()
    })

    if (nrow(weekly_data) > 0 && !all(is.na(weekly_data$Adjusted))) {
      all_eur_data <- rbind(all_eur_data, weekly_data)
      logger::log_info("EUR week success: {nrow(weekly_data)} rows", namespace = "Tdata")
    } else {
      logger::log_warn("EUR week failed: {current_date} to {chunk_end}", namespace = "Tdata")
    }

    current_date <- chunk_end + 1
    Sys.sleep(3)  # 3-second delay between weekly chunks
  }

  # Process EUR data (direct EUR/CHF pair)
  if (nrow(all_eur_data) > 0) {
    eur_processed <- all_eur_data |>
      dplyr::filter(!is.na(Adjusted), Adjusted > 0) |>
      dplyr::mutate(
        currency = "EUR",
        chf_value = round(Adjusted, 4),  # Direct EUR/CHF rate
        date = as.integer(format(date, "%Y%m%d"))
      ) |>
      dplyr::select(date, currency, chf_value)

    safe_db_append(conn, "ConvertToCHF", eur_processed)
    logger::log_info("Inserted {nrow(eur_processed)} EUR records", namespace = "Tdata")

    # Check final coverage
    coverage <- DBI::dbGetQuery(conn, "
      SELECT COUNT(*) as eur_count
      FROM ConvertToCHF
      WHERE currency = 'EUR'
      AND date BETWEEN 20250319 AND 20250419
    ")

    logger::log_info("EUR coverage for March-April: {coverage$eur_count} records", namespace = "Tdata")
  }
}


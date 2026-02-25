test_that("modifyJournalEntry filters NULL values correctly", {
  with_mocked_bindings(
    validate_db_types = function(data, table) {
      # Verify that NULLs were filtered out before validation
      expect_false("sym" %in% names(data))
      expect_false("change" %in% names(data))
      expect_true("theme" %in% names(data))
      expect_true("close" %in% names(data))
      return(data)
    },
    {
      # Function should execute successfully
      result <- modifyJournalEntry(
        entryId = 123,
        theme = "Technology",
        sym = NULL,
        close = 150.50,
        change = NULL
      )
      # Should return 1 indicating successful update (1 row affected)
      expect_equal(result, 1)
    }
  )
})

test_that("modifyJournalEntry handles multiple parameters correctly", {
  with_mocked_bindings(
    validate_db_types = function(data, table) {
      # Should receive exactly 4 parameters
      expect_equal(length(data), 4)
      expect_true("theme" %in% names(data))
      expect_true("date" %in% names(data))
      expect_true("close" %in% names(data))
      expect_true("text" %in% names(data))
      # These should be filtered out
      expect_false("sym" %in% names(data))
      expect_false("change" %in% names(data))
      return(data)
    },
    {
      # Should execute successfully
      result <- modifyJournalEntry(
        entryId = 888,
        theme = "Finance",
        date = "20250115",  # Valid date format
        sym = NULL,
        close = 150.50,
        change = NULL,
        text = "Test update"
      )
      # Should return 1 indicating successful update
      expect_equal(result, 1)
    }
  )
})

test_that("modifyJournalEntry returns 0 when no parameters to update", {
  with_mocked_bindings(
    validate_db_types = function(data, table) {
      expect_equal(length(data), 0)
      return(list())
    },
    {
      result <- modifyJournalEntry(entryId = 123)
      expect_equal(result, 0)  # This should still return 0 for no updates
    }
  )
})

test_that("readJournal returns tibble with correct structure", {
  # Test the basic functionality without mocking DB
  # This will use your actual database but only reads, so it's safe

  # Test with default windowDate (should not error)
  result <- readJournal()

  # Verify it returns a data frame/tibble
  expect_true(is.data.frame(result))

  # Verify expected columns exist (if there's data)
  if (nrow(result) > 0) {
    expected_cols <- c("entryId", "theme", "date", "sym", "close",
                       "change", "mkt_price", "mkt_change", "text")
    expect_true(all(expected_cols %in% names(result)))
  }

  # Test with specific windowDate
  recent_date <- as.integer(format(Sys.Date() - 30, "%Y%m%d"))
  result2 <- readJournal(windowDate = recent_date)
  expect_true(is.data.frame(result2))
})

test_that("readJournalMaxEntryId returns integer", {
  # Test the max entry ID function
  max_id <- readJournalMaxEntryId()

  # Should return an integer (or NA if no entries)
  expect_true(is.integer(max_id) || is.na(max_id))

  # If there are entries, should be positive
  if (!is.na(max_id)) {
    expect_true(max_id > 0)
  }
})

test_that("writeJournalEntry validates input correctly", {
  # Test input validation - use a unique entryId to avoid conflicts
  # Get max ID and add 1 to ensure uniqueness
  max_id <- readJournalMaxEntryId()
  test_id <- if (is.na(max_id)) 1 else max_id + 1

  # Create test entry with correct structure
  test_entry <- data.frame(
    entryId = test_id,
    theme = "Test",
    date = 20250704,
    sym = "AAPL",
    close = 150.00,
    change = "1.5%",
    mkt_price = 151.00,
    mkt_change = "2.0%",
    text = "Test entry"
  )

  # Test that function doesn't error with correct input structure
  result <- writeJournalEntry(test_entry)

  # Should return 1 for successful write
  expect_equal(result, 1)

  # Clean up - delete the test entry we just created
  delete_result <- deleteJournalEntry(test_id)
  expect_equal(delete_result, 1)  # Should successfully delete
})


test_that("deleteJournalEntry accepts integer entryId", {
  # Simple test - verify function accepts integer and returns numeric result
  # Use a non-existent ID so nothing actually gets deleted
  non_existent_id <- 99999999

  result <- deleteJournalEntry(entryId = non_existent_id)

  # Should return numeric result (0 for no rows affected, 1 for success)
  expect_true(is.numeric(result))
  expect_true(result >= 0)  # Should be 0 or positive
})

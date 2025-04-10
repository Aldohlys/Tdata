#' Simple comparison of function execution speeds
#'
#' @param func1 First function to compare
#' @param func2 Second function to compare
#' @param times Number of repetitions (default: 100)
#' @param ... Arguments to pass to both functions
#' @return A data frame with timing results
#' @export
compare_speed_simple <- function(func1, func2, times = 100, ...) {
  # Time first function
  start1 <- Sys.time()
  for (i in 1:times) {
    func1(...)
  }
  end1 <- Sys.time()
  time1 <- as.numeric(end1 - start1)

  # Time second function
  start2 <- Sys.time()
  for (i in 1:times) {
    func2(...)
  }
  end2 <- Sys.time()
  time2 <- as.numeric(end2 - start2)

  # Create results summary
  results <- data.frame(
    function_name = c("func1", "func2"),
    total_time = c(time1, time2),
    avg_time = c(time1/times, time2/times),
    relative_speed = c(1, time1/time2)
  )

  # Print a simple summary
  cat("Function speed comparison (", times, "iterations):\n")
  cat("func1 took:", round(time1, 4), "seconds\n")
  cat("func2 took:", round(time2, 4), "seconds\n")

  if (time1 < time2) {
    cat("func1 is", round(time2/time1, 2), "times faster than func2\n")
  } else if (time2 < time1) {
    cat("func2 is", round(time1/time2, 2), "times faster than func1\n")
  } else {
    cat("Both functions have similar performance\n")
  }

  return(results)
}

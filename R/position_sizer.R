#' Size a credit option combo using Monte Carlo simulation
#'
#' Determines optimal lot size for credit option strategies (short premium)
#' where risk is undefined. Uses Expected Shortfall (ES) risk budgeting and
#' IVP/HVP regime analysis. Not for debit strategies (max risk = debit paid).
#'
#' @param legs Data frame with columns: strike, right (C/P), pos (+1/-1),
#'   dte, mul, sig (per-leg IV)
#' @param spot Current underlying price
#' @param credit Net credit received per unit (must be > 0)
#' @param iv Annualized implied volatility (decimal or percentage)
#' @param hv Annualized historical volatility (decimal or percentage)
#' @param ivp IV percentile rank (0-100)
#' @param hvp HV percentile rank (0-100)
#' @param max_loss Risk budget per trade in dollars (default 600)
#' @param r Risk-free rate (default 0)
#' @param div Dividend yield (default 0)
#' @param n_sims Number of Monte Carlo simulations (default 200000)
#' @param profit_target_pct Exit at this fraction of max profit (default 0.50)
#' @param dte_cut Mandatory exit at this DTE (default 21)
#' @param df_student Student-t degrees of freedom (default 5)
#' @return Named list with lots, regime, scenarios, recommendation, inputs
#' @export
sizePosition <- function(legs, spot, credit, iv, hv, ivp, hvp,
                         max_loss = 600.0,
                         r = 0.0,
                         div = 0.0,
                         n_sims = 200000L,
                         profit_target_pct = 0.50,
                         dte_cut = 21L,
                         df_student = 5.0) {

  # Convert legs data.frame to list of dicts for Python
  legs_list <- lapply(seq_len(nrow(legs)), function(i) {
    list(
      strike = legs$strike[i],
      right  = legs$right[i],
      pos    = as.integer(legs$pos[i]),
      dte    = as.integer(legs$dte[i]),
      mul    = legs$mul[i],
      sig    = legs$sig[i]
    )
  })

  py <- tdata_py

  result <- py$size_position(
    legs             = legs_list,
    spot             = spot,
    credit           = credit,
    iv               = iv,
    hv               = hv,
    ivp              = ivp,
    hvp              = hvp,
    max_loss         = max_loss,
    r                = r,
    div              = div,
    n_sims           = as.integer(n_sims),
    profit_target_pct = profit_target_pct,
    dte_cut          = as.integer(dte_cut),
    df_student       = df_student
  )

  # Use [[ ]] for exact key matching (no R $ partial matching)
  if (is.null(result) || !is.null(result[["error"]])) {
    error_msg <- result[["error"]]
    if (is.null(error_msg)) error_msg <- "Unknown error in position sizer"
    logger::log_error("sizePosition failed: {error_msg}", namespace = "Tdata")
    return(NULL)
  }

  result
}

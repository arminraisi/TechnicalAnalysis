# Stochastic RSI: rolling RSI min/max normalized to 0..1.
# %K is smoothed over k_period and %D over d_period; full-length NA-aligned outputs.
# Requires rsi.R and sma.R to have been sourced first.
stoch_rsi <- function(data, period, k_period, d_period) {
  if (!exists("rsi", mode = "function") || !exists("sma", mode = "function"))
    stop("source rsi.R and sma.R first")
  check_period <- function(x) length(x) == 1L && is.numeric(x) && !is.na(x) &&
    is.finite(x) && x >= 1 && x == floor(x)
  if (!check_period(period) || !check_period(k_period) || !check_period(d_period))
    stop("all periods must be positive integers")
  rsi_values <- rsi(data, period)
  n <- length(rsi_values)
  raw_k <- rep(NA_real_, n)
  # The RSI normalization window is 'period' values.
  for (i in seq_len(n)) {
    if (i < period + period) next
    window <- rsi_values[(i - period + 1L):i]
    if (anyNA(window)) next
    low <- min(window); high <- max(window)
    raw_k[i] <- if (high == low) 0 else (rsi_values[i] - low) / (high - low)
  }
  rolling_mean <- function(x, width) {
    out <- rep(NA_real_, length(x))
    for (j in seq_along(x)) {
      if (j < width) next
      win <- x[(j - width + 1L):j]
      if (!anyNA(win)) out[j] <- sma(win, width)[1]
    }
    out
  }
  k_line <- rolling_mean(raw_k, k_period)
  d_line <- rolling_mean(k_line, d_period)
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}

# Simple Moving Average; returns n-period+1 rolling values.
sma <- function(data, period) {
  if (!is.numeric(data) || any(!is.finite(data))) stop("data must be finite numeric")
  if (length(period) != 1L || !is.numeric(period) || is.na(period) ||
      !is.finite(period) || period < 1 || period != floor(period)) stop("period must be a positive integer")
  n <- length(data)
  if (n < period) stop("Data length should be greater than or equal to the period")
  sma_values <- numeric(n - period + 1L)
  for (i in seq_len(length(sma_values))) {
    sma_values[i] <- sum(data[i:(i + period - 1L)]) / period
  }
  return(sma_values)
}

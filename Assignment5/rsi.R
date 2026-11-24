# RSI using Wilder smoothing and an initial period-change average.
rsi <- function(data, period) {
  if (!is.numeric(data) || any(!is.finite(data))) stop("data must be finite numeric")
  if (length(period) != 1L || !is.numeric(period) || is.na(period) ||
      !is.finite(period) || period < 1 || period != floor(period)) stop("period must be a positive integer")
  n <- length(data)
  rsi_values <- rep(NA_real_, n)
  if (n <= period) return(rsi_values)
  changes <- diff(data)
  gains <- pmax(changes, 0)
  losses <- pmax(-changes, 0)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  value <- function(g, l) {
    if (l == 0 && g == 0) return(50)
    if (l == 0) return(100)
    100 - 100 / (1 + g / l)
  }
  rsi_values[period + 1L] <- value(avg_gain, avg_loss)
  if (n > period + 1L) for (i in (period + 2L):n) {
    avg_gain <- (avg_gain * (period - 1L) + gains[i - 1L]) / period
    avg_loss <- (avg_loss * (period - 1L) + losses[i - 1L]) / period
    rsi_values[i] <- value(avg_gain, avg_loss)
  }
  return(rsi_values)
}

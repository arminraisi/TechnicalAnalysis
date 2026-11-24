# Exponential moving average; seeded with the first input value.
ema <- function(data, period) {
  if (!is.numeric(data) || any(!is.finite(data))) stop("data must be finite numeric")
  if (length(period) != 1L || !is.numeric(period) || is.na(period) ||
      !is.finite(period) || period < 1 || period != floor(period)) stop("period must be a positive integer")
  n <- length(data)
  if (n == 0L) return(numeric(0))
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(n)
  ema_values[1] <- data[1]
  if (n > 1L) for (i in 2:n) {
    ema_values[i] <- (data[i] - ema_values[i - 1L]) * multiplier + ema_values[i - 1L]
  }
  return(ema_values)
}

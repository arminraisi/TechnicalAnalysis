# MACD: difference of short/long EMA, EMA signal and histogram.
# Requires ema.R to have been sourced first.
macd <- function(data, short_period, long_period, signal_period) {
  if (!exists("ema", mode = "function")) stop("source ema.R first")
  if (length(short_period) != 1L || length(long_period) != 1L ||
      !is.numeric(short_period) || !is.numeric(long_period) ||
      is.na(short_period) || is.na(long_period) ||
      short_period >= long_period) stop("short_period must be smaller than long_period")
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line
  result <- list(macd_line = macd_line, signal_line = signal_line,
                 histogram = histogram)
  return(result)
}

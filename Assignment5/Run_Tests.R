# Base R regression tests for BDA400 Assignment 5. Run from this folder.
for (f in c("sma.R","ema.R","macd.R","stdev.R","linreg.R","rsi.R",
            "stoch_rsi.R","crossover.R","crossunder.R")) source(f)
near <- function(actual, expected, tol = 1e-9) {
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = tol)))
}
near(sma(c(1,2,3,4), 2), c(1.5,2.5,3.5))
near(ema(c(2,4,6), 3), c(2,3,4.5))
m <- macd(c(2,4,6), 2, 3, 2)
near(m$macd_line, ema(c(2,4,6),2)-ema(c(2,4,6),3))
near(m$histogram, m$macd_line-m$signal_line)
near(stdev(c(2,4,6)), sqrt(8/3))
fit <- linreg(c(1,3,5,7,9), 3, 1)
near(fit$slope, 2)
near(fit$predicted_values, c(3,5,7))
near(rsi(1:20, 14)[15:20], rep(100,6))
near(rsi(rep(1,20), 14)[15:20], rep(50,6))
stopifnot(all(is.na(rsi(1:5, 14))))
sr <- stoch_rsi(100 + cumsum(rep(c(1,2,-1,-2,3,-2),12)),14,3,3)
stopifnot(length(sr$k_line)==72, length(sr$d_line)==72,
          all(sr$k_line[!is.na(sr$k_line)]>=0 & sr$k_line[!is.na(sr$k_line)]<=1))
stopifnot(identical(crossover(c(1,2,1),c(2,1,2)),c(FALSE,TRUE,FALSE)))
stopifnot(identical(crossunder(c(1,2,1),c(2,1,2)),c(FALSE,FALSE,TRUE)))
cat("All base-R indicator tests passed.\n")

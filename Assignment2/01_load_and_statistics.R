# BDA400 Assignment 2: Stock import and descriptive statistics
# Run from the Assignment2 directory.
required_packages <- c("quantmod", "TTR")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(library(quantmod))
suppressPackageStartupMessages(library(TTR))

load_stock_data <- function(portfolio_file = "portfolio.txt", from = Sys.Date() - 365) {
  if (!file.exists(portfolio_file)) stop("Portfolio file not found: ", portfolio_file)
  symbols <- unique(trimws(readLines(portfolio_file, warn = FALSE)))
  symbols <- symbols[nzchar(symbols) & !startsWith(symbols, "#")]
  if (!length(symbols)) stop("No stock symbols found")
  result <- list()
  for (symbol in symbols) {
    tryCatch({
      raw <- quantmod::getSymbols(symbol, src = "yahoo", from = from, auto.assign = FALSE, warnings = FALSE)
      df <- data.frame(Date = as.Date(zoo::index(raw)), zoo::coredata(raw), check.names = FALSE)
      names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")[seq_len(ncol(df))]
      if (!nrow(df)) stop("No rows returned")
      result[[symbol]] <- df
    }, error = function(e) warning(sprintf("Unable to load %s: %s", symbol, conditionMessage(e))))
  }
  if (!length(result)) stop("No stock data could be downloaded. Check internet connectivity and symbols.")
  result
}

# Statistical mode: most frequent observed close; ties return the smallest value.
# For continuously valued prices this is a descriptive frequency mode, not a density estimate.
statistical_mode <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  frequencies <- table(x)
  as.numeric(names(frequencies)[which.max(frequencies)])
}

calculate_statistics <- function(stock_df, symbol, n = 20L) {
  if (!"Close" %in% names(stock_df)) stop("Close column is required")
  close <- as.numeric(stock_df$Close)
  valid <- close[is.finite(close)]
  if (!length(valid)) stop("No valid closing prices for ", symbol)
  ma <- TTR::SMA(close, n = n)
  data.frame(Symbol = symbol, Observations = length(valid),
             Mean = mean(valid), Mode = statistical_mode(valid),
             Median = median(valid), StdDev = if (length(valid) > 1) sd(valid) else NA_real_,
             LatestClose = tail(valid, 1), LatestSMA20 = if (all(is.na(ma))) NA_real_ else tail(na.omit(ma), 1),
             stringsAsFactors = FALSE)
}

calculate_all_statistics <- function(stock_data, n = 20L) {
  do.call(rbind, lapply(names(stock_data), function(symbol)
    calculate_statistics(stock_data[[symbol]], symbol, n)))
}

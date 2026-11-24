# Linear regression on the selected trailing window and offset.
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource) || any(!is.finite(regressionSource)))
    stop("regressionSource must be finite numeric")
  check_int <- function(x, minimum) length(x) == 1L && is.numeric(x) &&
    !is.na(x) && is.finite(x) && x >= minimum && x == floor(x)
  if (!check_int(regressionLength, 2)) stop("regressionLength must be an integer >= 2")
  if (!check_int(regressionOffset, 0)) stop("regressionOffset must be a nonnegative integer")
  n <- length(regressionSource)
  if (regressionLength > n) stop("regressionLength cannot exceed source length")
  if (regressionOffset >= regressionLength) stop("regressionOffset must be less than regressionLength")
  # Offset excludes that many most-recent observations while maintaining window length.
  if (regressionLength + regressionOffset > n)
    stop("regressionLength + regressionOffset cannot exceed source length")
  start_index <- n - regressionOffset - regressionLength + 1L
  end_index <- n - regressionOffset
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept
  result <- list(slope = slope, intercept = intercept,
                 predicted_values = predicted_values)
  return(result)
}

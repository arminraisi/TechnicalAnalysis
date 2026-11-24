# TRUE when arr1 crosses from >= arr2 to < arr2 at an index.
crossunder <- function(arr1, arr2) {
  if (!is.numeric(arr1) || !is.numeric(arr2) ||
      any(!is.finite(arr1)) || any(!is.finite(arr2)))
    stop("both arrays must be finite numeric vectors")
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  n <- length(arr1)
  crossunder_signals <- rep(FALSE, n)
  if (n > 1L) for (i in 2:n) {
    crossunder_signals[i] <- (arr1[i] < arr2[i] && arr1[i-1L] >= arr2[i-1L])
  }
  return(crossunder_signals)
}

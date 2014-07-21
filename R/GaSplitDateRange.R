#'@export
GaSplitDateRange <- function(dateRange, N) {  
  # If N = 0 then split date range into single days
  # If N = 1, then the date range returned will be of length 1
  #   i.e. it will be the same or of shorter length than the original.
  #
  # Set new start dates
  maxN <- as.numeric(max(GaEndDate(dateRange)) - min(GaStartDate(dateRange))) + 1
  if(N <= 0 | N > maxN) {
    N <- maxN
  }
  start <- min(GaStartDate(dateRange))
  end <- max(GaEndDate(dateRange))
  start <- seq(
    from = start,
    to = end + 1,
    length.out = N + 1
  )[-(N + 1)]
  # Set new end dates
  end <- c(
    start[-1] - 1,
    end
  )
  #GaDateRange(dateRange) <- GaDateRange(start, end)
  dateRange <- GaDateRange(start, end)
  return(dateRange)
}

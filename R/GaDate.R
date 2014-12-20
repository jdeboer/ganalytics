#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

#'GaSplitDateRange
#'Splits a gaDateRange object into N pieces. Useful for splitting a query into
#'smaller chunks in order to overcome sampling.
#'@param dateRange the gaDateRange object to be split
#'@param N the number of the separate date ranges to be split into; use 0 for
#'  single days.
#'@export
GaSplitDateRange <- function(dateRange, N) {
  # TO DO
  # Assert:
  # N >= 0 and length(dateRange) == 1
  # 
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
  GaDateRange(dateRange) <- GaDateRange(start, end)
  return(dateRange)
}

GetGaDataByDateRange <- function(query, dates) {
  adply(dates, 1, function(dateRange) {
    GaDateRange(query) <- GaDateRange(dateRange$start, dateRange$end)
    output <- GetGaData(query)
    if(nrow(output)==0){output <- NULL}
    return(output)
  })
}

# GaDateRange
setMethod(
  f = "GaDateRange",
  signature = "gaDateRange",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaDateRange",
  signature = c("character", "character"),
  definition = function(.Object, endDate) {
    startDate <- as.Date(.Object, format = kGaDateInFormat)
    endDate <- as.Date(endDate, format = kGaDateInFormat)
    new("gaDateRange", startDate, endDate)
  }
)

setMethod(
  f = "GaDateRange",
  signature = c("Date", "Date"),
  definition = function(.Object, endDate) {
    startDate <- .Object
    new("gaDateRange", startDate, endDate)
  }
)

setMethod(
  f = "GaStartDate",
  signature = "character",
  definition = function(.Object) {
    as.Date(.Object, format = kGaDateInFormat)
  }
)

setMethod(
  f = "GaEndDate",
  signature = "character",
  definition = function(.Object) {
    as.Date(.Object, format = kGaDateInFormat)
  }
)

setMethod(
  f = "GaStartDate",
  signature = "gaDateRange",
  definition = function(.Object) {
    .Object@startDate
  }
)

setMethod(
  f = "GaEndDate",
  signature = "gaDateRange",
  definition = function(.Object) {
    .Object@endDate
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("gaDateRange", "character"),
  definition = function(.Object, value) {
    startDate <- as.Date(value, format = kGaDateInFormat)
    endDate <- GaEndDate(.Object)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("gaDateRange", "character"),
  definition = function(.Object, value) {
    startDate <- GaStartDate(.Object)
    endDate <- as.Date(value, format = kGaDateInFormat)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("gaDateRange", "Date"),
  definition = function(.Object, value) {
    startDate <- as.Date(value, format = kGaDateInFormat)
    endDate <- GaEndDate(.Object)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("gaDateRange", "Date"),
  definition = function(.Object, value) {
    startDate <- GaStartDate(.Object)
    endDate <- as.Date(value, format = kGaDateInFormat)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaDateRange<-",
  signature = c("gaDateRange", "character"),
  definition = function(.Object, value) {
    if (length(value) != 2) {
      stop("value must contain the start date and end date in a character vecotr of legnth 2.")
    } else {
      startDate <- as.Date(value[1], format = kGaDateInFormat)
      endDate <- as.Date(value[2], format = kGaDateInFormat)
      new("gaDateRange", startDate, endDate)
    }
  }
)


setMethod(
  f = "GaDateRange",
  signature = "gaQuery",
  definition = function(.Object) {
    return(.Object@dateRange)
  }
)

setMethod(
  f = "GaDateRange<-",
  signature = c("gaQuery", "gaDateRange"),
  definition = function(.Object, value) {
    .Object@dateRange <- value
    validObject(.Object)
    return(.Object)
  }
)

setMethod(
  f = "GaDateRange<-",
  signature = c("gaQuery", "character"),
  definition = function(.Object, value) {
    if (length(value) != 2) {
      stop("value must contain the start date and end date in a character vecotr of legnth 2.")
    } else {
      startDate <- as.Date(value[1], format = kGaDateInFormat)
      endDate <- as.Date(value[2], format = kGaDateInFormat)
      newDateRange <- new("gaDateRange", startDate, endDate)
      GaDateRange(.Object) <- newDateRange
      return(.Object)
    }
  }
)


setMethod(
  f = "GaStartDate",
  signature = "gaQuery",
  definition = function(.Object) {
    GaStartDate(.Object@dateRange)
  }
)

setMethod(
  f = "GaEndDate",
  signature = "gaQuery",
  definition = function(.Object) {
    GaEndDate(.Object@dateRange)
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("gaQuery", "character"),
  definition = function(.Object, value) {
    dateRange <- GaDateRange(.Object)
    GaStartDate(dateRange) <- value
    GaDateRange(.Object) <- dateRange
    return(.Object)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("gaQuery", "character"),
  definition = function(.Object, value) {
    dateRange <- GaDateRange(.Object)
    GaEndDate(dateRange) <- value
    GaDateRange(.Object) <- dateRange
    return(.Object)
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("gaQuery", "Date"),
  definition = function(.Object, value) {
    dateRange <- GaDateRange(.Object)
    GaStartDate(dateRange) <- value
    GaDateRange(.Object) <- dateRange
    return(.Object)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("gaQuery", "Date"),
  definition = function(.Object, value) {
    dateRange <- GaDateRange(.Object)
    GaEndDate(dateRange) <- value
    GaDateRange(.Object) <- dateRange
    return(.Object)
  }
)


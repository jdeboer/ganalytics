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
  signature = "dateRange",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaDateRange",
  signature = c("character", "character"),
  definition = function(.Object, endDate) {
    startDate <- as.Date(parse_date(.Object, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- as.Date(parse_date(endDate, output_format = kGaDateInFormat), format = kGaDateInFormat)
    new("dateRange", startDate, endDate)
  }
)

setMethod(
  f = "GaDateRange",
  signature = c("Date", "Date"),
  definition = function(.Object, endDate) {
    startDate <- .Object
    new("dateRange", startDate, endDate)
  }
)

setMethod(
  f = "GaStartDate",
  signature = "character",
  definition = function(.Object) {
    as.Date(parse_date(.Object, output_format = kGaDateInFormat), format = kGaDateInFormat)
  }
)

setMethod(
  f = "GaEndDate",
  signature = "character",
  definition = function(.Object) {
    as.Date(parse_date(.Object, output_format = kGaDateInFormat), format = kGaDateInFormat)
  }
)

setMethod(
  f = "GaStartDate",
  signature = "dateRange",
  definition = function(.Object) {
    .Object@startDate
  }
)

setMethod(
  f = "GaEndDate",
  signature = "dateRange",
  definition = function(.Object) {
    .Object@endDate
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("dateRange", "character"),
  definition = function(.Object, value) {
    startDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- GaEndDate(.Object)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("dateRange", "character"),
  definition = function(.Object, value) {
    startDate <- GaStartDate(.Object)
    endDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("dateRange", "Date"),
  definition = function(.Object, value) {
    startDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- GaEndDate(.Object)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("dateRange", "Date"),
  definition = function(.Object, value) {
    startDate <- GaStartDate(.Object)
    endDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaDateRange<-",
  signature = c("dateRange", "character"),
  definition = function(.Object, value) {
    if (length(value) != 2) {
      stop("value must contain the start date and end date in a character vecotr of legnth 2.")
    } else {
      startDate <- as.Date(parse_date(value[1], output_format = kGaDateInFormat), format = kGaDateInFormat)
      endDate <- as.Date(parse_date(value[2], output_format = kGaDateInFormat), format = kGaDateInFormat)
      new("dateRange", startDate, endDate)
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
  signature = c("gaQuery", "dateRange"),
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
      startDate <- as.Date(parse_date(value[1], output_format = kGaDateInFormat), format = kGaDateInFormat)
      endDate <- as.Date(parse_date(value[2], output_format = kGaDateInFormat), format = kGaDateInFormat)
      newDateRange <- new("dateRange", startDate, endDate)
      GaDateRange(.Object) <- newDateRange
      return(.Object)
    }
  }
)

setMethod("GaDateRange<-", c("gaQuery", "Date"), function(.Object, value) {
  GaDateRange(.Object, as.character(value))
})

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


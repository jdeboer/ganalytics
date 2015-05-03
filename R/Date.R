#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

#'SplitDateRange
#'Splits a gaDateRange object into N pieces. Useful for splitting a query into
#'smaller chunks in order to overcome sampling.
#'@param dateRange the gaDateRange object to be split
#'@param N the number of the separate date ranges to be split into; use 0 for
#'  single days.
#'@export
SplitDateRange <- function(dateRange, N) {
  # TO DO
  # Assert:
  # N >= 0 and length(dateRange) == 1
  # 
  # If N = 0 then split date range into single days
  # If N = 1, then the date range returned will be of length 1
  #   i.e. it will be the same or of shorter length than the original.
  #
  # Set new start dates
  maxN <- as.numeric(max(EndDate(dateRange)) - min(StartDate(dateRange))) + 1
  if(N <= 0 | N > maxN) {
    N <- maxN
  }
  start <- min(StartDate(dateRange))
  end <- max(EndDate(dateRange))
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
  DateRange(dateRange) <- DateRange(start, end)
  return(dateRange)
}

GetDataByDateRange <- function(query, dates) {
  adply(dates, 1, function(dateRange) {
    DateRange(query) <- DateRange(dateRange$start, dateRange$end)
    output <- GetGaData(query)
    if(nrow(output)==0){output <- NULL}
    return(output)
  })
}

# DateRange
setMethod("DateRange", "dateRange", function(.Object) {.Object})

setMethod(
  f = "DateRange",
  signature = c("character", "character"),
  definition = function(.Object, endDate) {
    startDate <- as.Date(parse_date(.Object, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- as.Date(parse_date(endDate, output_format = kGaDateInFormat), format = kGaDateInFormat)
    new("dateRange", startDate, endDate)
  }
)

setMethod(
  f = "DateRange",
  signature = c("Date", "Date"),
  definition = function(.Object, endDate) {
    startDate <- .Object
    new("dateRange", startDate, endDate)
  }
)

setMethod(
  f = "StartDate",
  signature = "character",
  definition = function(.Object) {
    as.Date(parse_date(.Object, output_format = kGaDateInFormat), format = kGaDateInFormat)
  }
)

setMethod(
  f = "EndDate",
  signature = "character",
  definition = function(.Object) {
    as.Date(parse_date(.Object, output_format = kGaDateInFormat), format = kGaDateInFormat)
  }
)

setMethod("StartDate", "dateRange", function(.Object) {.Object@startDate})

setMethod("EndDate", "dateRange", function(.Object) {.Object@endDate})

setMethod(
  f = "StartDate<-",
  signature = c("dateRange", "character"),
  definition = function(.Object, value) {
    startDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- EndDate(.Object)
    DateRange(startDate, endDate)
  }
)

setMethod(
  f = "EndDate<-",
  signature = c("dateRange", "character"),
  definition = function(.Object, value) {
    startDate <- StartDate(.Object)
    endDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    DateRange(startDate, endDate)
  }
)

setMethod(
  f = "StartDate<-",
  signature = c("dateRange", "Date"),
  definition = function(.Object, value) {
    startDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- EndDate(.Object)
    DateRange(startDate, endDate)
  }
)

setMethod(
  f = "EndDate<-",
  signature = c("dateRange", "Date"),
  definition = function(.Object, value) {
    startDate <- StartDate(.Object)
    endDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    DateRange(startDate, endDate)
  }
)

setMethod(
  f = "DateRange<-",
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


setMethod("DateRange", ".standardQuery", function(.Object) {.Object@dateRange})

setMethod(
  f = "DateRange<-",
  signature = c(".standardQuery", "dateRange"),
  definition = function(.Object, value) {
    .Object@dateRange <- value
    validObject(.Object)
    .Object
  }
)

setMethod(
  f = "DateRange<-",
  signature = c(".standardQuery", "character"),
  definition = function(.Object, value) {
    if (length(value) != 2) {
      stop("value must contain the start date and end date in a character vecotr of legnth 2.")
    } else {
      startDate <- as.Date(parse_date(value[1], output_format = kGaDateInFormat), format = kGaDateInFormat)
      endDate <- as.Date(parse_date(value[2], output_format = kGaDateInFormat), format = kGaDateInFormat)
      newDateRange <- new("dateRange", startDate, endDate)
      DateRange(.Object) <- newDateRange
      .Object
    }
  }
)

setMethod("DateRange<-", c(".standardQuery", "Date"), function(.Object, value) {
  DateRange(.Object, as.character(value))
})

setMethod("StartDate", ".standardQuery", function(.Object) {StartDate(.Object@dateRange)})

setMethod("EndDate", ".standardQuery", function(.Object) {EndDate(.Object@dateRange)})

setMethod(
  f = "StartDate<-",
  signature = c(".standardQuery", "character"),
  definition = function(.Object, value) {
    dateRange <- DateRange(.Object)
    StartDate(dateRange) <- value
    DateRange(.Object) <- dateRange
    .Object
  }
)

setMethod(
  f = "EndDate<-",
  signature = c(".standardQuery", "character"),
  definition = function(.Object, value) {
    dateRange <- DateRange(.Object)
    EndDate(dateRange) <- value
    DateRange(.Object) <- dateRange
    .Object
  }
)

setMethod(
  f = "StartDate<-",
  signature = c(".standardQuery", "Date"),
  definition = function(.Object, value) {
    dateRange <- DateRange(.Object)
    StartDate(dateRange) <- value
    DateRange(.Object) <- dateRange
    .Object
  }
)

setMethod(
  f = "EndDate<-",
  signature = c(".standardQuery", "Date"),
  definition = function(.Object, value) {
    dateRange <- DateRange(.Object)
    EndDate(dateRange) <- value
    DateRange(.Object) <- dateRange
    .Object
  }
)

# For backwards compatibility
#'@export GaStartDate
GaStartDate <- StartDate
#'@export GaEndDate
GaEndDate <- EndDate
#'@export GaDateRange
GaDateRange <- DateRange
#'@export GaStartDate<-
`GaStartDate<-` <- `StartDate<-`
#'@export GaEndDate<-
`GaEndDate<-` <- `EndDate<-`
#'@export GaDateRange<-
`GaDateRange<-` <- `DateRange<-`


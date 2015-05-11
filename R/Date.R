#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @importFrom plyr adply
NULL

#'SplitDateRange
#'
#'Splits a gaDateRange object into N pieces. Useful for splitting a query into
#'smaller chunks in order to overcome sampling.
#'
#'@param dateRange the gaDateRange object to be split
#'@param N the number of the separate date ranges to be split into; use 0 for
#'  single days.
#'
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

# StartDate and EndDate

#' @describeIn DateRange
setMethod(
  f = "StartDate",
  signature = "character",
  definition = function(object) {
    as.Date(parse_date(object, output_format = kGaDateInFormat), format = kGaDateInFormat)
  }
)

#' @describeIn DateRange
setMethod(
  f = "EndDate",
  signature = "character",
  definition = function(object) {
    as.Date(parse_date(object, output_format = kGaDateInFormat), format = kGaDateInFormat)
  }
)

#' @describeIn DateRange
setMethod("StartDate", "dateRange", function(object) {object@startDate})

#' @describeIn DateRange
setMethod("EndDate", "dateRange", function(object) {object@endDate})

#' @describeIn DateRange
setMethod("StartDate", ".standardQuery", function(object) {StartDate(object@dateRange)})

#' @describeIn DateRange
setMethod("EndDate", ".standardQuery", function(object) {EndDate(object@dateRange)})

#' @describeIn DateRange
setMethod(
  f = "StartDate<-",
  signature = c("dateRange", "character"),
  definition = function(object, value) {
    startDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- EndDate(object)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange
setMethod(
  f = "EndDate<-",
  signature = c("dateRange", "character"),
  definition = function(object, value) {
    startDate <- StartDate(object)
    endDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange
setMethod(
  f = "StartDate<-",
  signature = c("dateRange", "Date"),
  definition = function(object, value) {
    startDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- EndDate(object)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange
setMethod(
  f = "EndDate<-",
  signature = c("dateRange", "Date"),
  definition = function(object, value) {
    startDate <- StartDate(object)
    endDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange
setMethod(
  f = "StartDate<-",
  signature = c(".standardQuery", "character"),
  definition = function(object, value) {
    dateRange <- DateRange(object)
    StartDate(dateRange) <- value
    DateRange(object) <- dateRange
    object
  }
)

#' @describeIn DateRange
setMethod(
  f = "EndDate<-",
  signature = c(".standardQuery", "character"),
  definition = function(object, value) {
    dateRange <- DateRange(object)
    EndDate(dateRange) <- value
    DateRange(object) <- dateRange
    object
  }
)

#' @describeIn DateRange
setMethod(
  f = "StartDate<-",
  signature = c(".standardQuery", "Date"),
  definition = function(object, value) {
    dateRange <- DateRange(object)
    StartDate(dateRange) <- value
    DateRange(object) <- dateRange
    object
  }
)

#' @describeIn DateRange
setMethod(
  f = "EndDate<-",
  signature = c(".standardQuery", "Date"),
  definition = function(object, value) {
    dateRange <- DateRange(object)
    EndDate(dateRange) <- value
    DateRange(object) <- dateRange
    object
  }
)

# DateRange

#' @describeIn DateRange
setMethod("DateRange", "dateRange", function(object) {object})

#' @describeIn DateRange
setMethod(
  f = "DateRange",
  signature = c("character", "character"),
  definition = function(object, endDate) {
    startDate <- as.Date(parse_date(object, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- as.Date(parse_date(endDate, output_format = kGaDateInFormat), format = kGaDateInFormat)
    new("dateRange", startDate, endDate)
  }
)

#' @describeIn DateRange
setMethod(
  f = "DateRange",
  signature = c("Date", "Date"),
  definition = function(object, endDate) {
    startDate <- object
    new("dateRange", startDate, endDate)
  }
)

#' @describeIn DateRange
setMethod("DateRange", ".standardQuery", function(object) {object@dateRange})

#' @describeIn DateRange
setMethod(
  f = "DateRange<-",
  signature = c("dateRange", "character"),
  definition = function(object, value) {
    if (length(value) != 2) {
      stop("value must contain the start date and end date in a character vecotr of legnth 2.")
    } else {
      startDate <- as.Date(parse_date(value[1], output_format = kGaDateInFormat), format = kGaDateInFormat)
      endDate <- as.Date(parse_date(value[2], output_format = kGaDateInFormat), format = kGaDateInFormat)
      new("dateRange", startDate, endDate)
    }
  }
)

#' @describeIn DateRange
setMethod(
  f = "DateRange<-",
  signature = c(".standardQuery", "dateRange"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange
setMethod(
  f = "DateRange<-",
  signature = c(".standardQuery", ".standardQuery"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange
setMethod(
  f = "DateRange<-",
  signature = c("dateRange", "dateRange"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange
setMethod(
  f = "DateRange<-",
  signature = c("dateRange", ".standardQuery"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange
setMethod(
  f = "DateRange<-",
  signature = c(".standardQuery", "character"),
  definition = function(object, value) {
    if (length(value) != 2) {
      stop("value must contain the start date and end date in a character vecotr of legnth 2.")
    } else {
      startDate <- as.Date(parse_date(value[1], output_format = kGaDateInFormat), format = kGaDateInFormat)
      endDate <- as.Date(parse_date(value[2], output_format = kGaDateInFormat), format = kGaDateInFormat)
      newDateRange <- new("dateRange", startDate, endDate)
      DateRange(object) <- newDateRange
      object
    }
  }
)

#' @describeIn DateRange
setMethod("DateRange<-", c(".standardQuery", "Date"), function(object, value) {
  DateRange(object, as.character(value))
})

#' GaStartDate (Deprecated)
#' For backwards compatibility
#' @export GaStartDate
#' @rdname GaStartDate
#' @param ... arguments passed onto \code{StartDate}
GaStartDate <- function(...){StartDate(...)}

#' GaEndDate (Deprecated)
#' @export GaEndDate
#' @rdname GaEndDate
#' @param ... arguments passed onto \code{EndDate}
GaEndDate <- function(...){EndDate(...)}

#' GaDateRange (Deprecated)
#' @export GaDateRange
#' @rdname GaDateRange
#' @param ... arguments passed onto \code{DateRange}
GaDateRange <- function(...){DateRange(...)}

#' GaStartDate<- (Deprecated)
#' @export GaStartDate<-
#' @rdname GaStartDate
#' @param value passed onto \code{StartDate}
`GaStartDate<-` <- function(..., value){`StartDate<-`(..., value)}

#' GaEndDate<- (Deprecated)
#' @export GaEndDate<-
#' @rdname GaEndDate
#' @param value passed onto \code{EndDate}
`GaEndDate<-` <- function(..., value){`EndDate<-`(..., value)}

#' GaDateRange<- (Deprecated)
#' @export GaDateRange<-
#' @rdname GaDateRange
#' @param value passed onto \code{DateRange}
`GaDateRange<-` <- function(..., value){`DateRange<-`(..., value)}

#' GaSplitDateRange (Deprecated)
#' @export GaSplitDateRange
#' @param ... arguments passed onto \code{SplitDateRange}
GaSplitDateRange <- function(...){SplitDateRange(...)}

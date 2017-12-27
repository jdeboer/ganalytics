#' @include Date-generics.R
#' @include query-classes.R
#' @include init-methods.R
#' @include management-api-classes.R
#' @include date-coerce.R
#' @importFrom plyr adply
#' @importFrom stringr str_split_fixed
#' @importFrom lubridate today
#' @importFrom methods setMethod new as as<-
NULL

#' SplitDateRange
#'
#' Splits a gaDateRange object into N pieces. Useful for splitting a query into
#' smaller chunks in order to overcome sampling.
#'
#' @param dateRange the gaDateRange object to be split
#' @param N the number of the separate date ranges to be split into; use 0 for
#'   single days.
#'
#' @export
SplitDateRange <- function(dateRange, N) {
  # TO DO
  # Assert:
  # length(dateRange) == 1
  #
  # If N = 0 then split date range into single days
  # If N = 1, then the date range returned will be of length 1
  #   i.e. it will be the same or of shorter length than the original.
  #
  # Set new start dates
  assert_that(
    N >= 0L,
    class(dateRange) == "dateRange"
  )
  maxN <- as.numeric(max(EndDate(dateRange)) - min(StartDate(dateRange))) + 1
  if (N <= 0 | N > maxN) {
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
    if (nrow(output) == 0){output <- NULL}
    return(output)
  })
}

# StartDate and EndDate

#' @describeIn DateRange Coerce a character vector into a Google Analytics date
#'   object.
setMethod("StartDate", "character", function(object) {
  as(object, "Date")
})

#' @describeIn DateRange Coerce a character vector into a Google Analytics date
#'   object.
setMethod("EndDate", "character", function(object) {
  as(object, "Date")
})

#' @describeIn DateRange Return the start dates of a date range vector
setMethod("StartDate", "dateRange", function(object) {object@startDate})

#' @describeIn DateRange Return the end dates of a date range vector
setMethod("EndDate", "dateRange", function(object) {object@endDate})

#' @describeIn DateRange Return the start dates of a date range vector
setMethod("StartDate", "Interval", function(object) {StartDate(DateRange(object))})

#' @describeIn DateRange Return the end dates of a date range vector
setMethod("EndDate", "Interval", function(object) {EndDate(DateRange(object))})

#' @describeIn DateRange Return the start dates of a query's date range vector
setMethod("StartDate", ".standardQuery", function(object) {StartDate(object@dateRange)})

#' @describeIn DateRange Return the end dates of a query's date range vector
setMethod("EndDate", ".standardQuery", function(object) {EndDate(object@dateRange)})

#' @describeIn DateRange Get the date when a view first started receiving hits.
setMethod("StartDate", "gaView", function(object) {
  start_date <- as.Date(object$created)
  end_date <- today()
  query <- GaQuery(object)
  StartDate(query) <- start_date
  EndDate(query) <- end_date
  Metrics(query) <- "hits"
  Dimensions(query) <- "date"
  MaxResults(query) <- 1
  TableFilter(query) <- Expr("hits", ">", 0)
  SortBy(query) <- "+date"
  GetGaData(query)$date
})

#' @describeIn DateRange Get the last day a view received hits.
setMethod("EndDate", "gaView", function(object) {
  start_date <- as.Date(object$created)
  end_date <- today()
  query <- GaQuery(object)
  StartDate(query) <- start_date
  EndDate(query) <- end_date
  Metrics(query) <- "hits"
  Dimensions(query) <- "date"
  MaxResults(query) <- 1
  TableFilter(query) <- Expr("hits", ">", 0)
  SortBy(query) <- "-date"
  GetGaData(query)$date
})

#' @describeIn DateRange Set a new start date for a date range.
setMethod(
  f = "StartDate<-",
  signature = c("dateRange", "ANY"),
  definition = function(object, value) {
    startDate <- as(value, "Date")
    endDate <- EndDate(object)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange Set a new end date for a date range.
setMethod(
  f = "EndDate<-",
  signature = c("dateRange", "ANY"),
  definition = function(object, value) {
    startDate <- StartDate(object)
    endDate <- as(value, "Date")
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange Set a new start date for a query.
setMethod(
  f = "StartDate<-",
  signature = c(".standardQuery", "ANY"),
  definition = function(object, value) {
    dateRange <- DateRange(object)
    StartDate(dateRange) <- value
    DateRange(object) <- dateRange
    object
  }
)

#' @describeIn DateRange Set a new end date for a query.
setMethod(
  f = "EndDate<-",
  signature = c(".standardQuery", "ANY"),
  definition = function(object, value) {
    dateRange <- DateRange(object)
    EndDate(dateRange) <- value
    DateRange(object) <- dateRange
    object
  }
)

# DateRange

#' @describeIn DateRange generates a date range object using the supplied
#'   vectors of start date and end dates.
setMethod(
  f = "DateRange",
  signature = c("ANY", "ANY"),
  definition = function(object, endDate) {
    startDate <- as(object, "Date")
    endDate <- as(endDate, "Date")
    new("dateRange", startDate, endDate)
  }
)

#' @describeIn DateRange Returns the date range of the given query or coerces
#'   the supplied object into a dateRange.
setMethod("DateRange", c("ANY", "missing"), function(object) {
  as(object, "dateRange")
})

setMethod("DateRange", c(".standardQuery"), function(object) {
  object@dateRange
})

setMethod("DateRange<-", c(".standardQuery", "ANY"), function(object, value) {
  object@dateRange <- as(value, "dateRange")
  validObject(object)
  object
})

#' @describeIn DateRange Returns the maximum date range of when a view has been
#'   recieving hits.
setMethod("DateRange", "gaView", function(object) {
  start_date <- as.Date(object$created)
  end_date <- today()
  query <- GaQuery(object)
  StartDate(query) <- start_date
  EndDate(query) <- end_date
  Metrics(query) <- "hits"
  Dimensions(query) <- "date"
  MaxResults(query) <- 1
  TableFilter(query) <- Expr("hits", ">", 0)
  SortBy(query) <- "+date"
  start_date <- GetGaData(query)$date
  StartDate(query) <- start_date
  SortBy(query) <- "-date"
  end_date <- GetGaData(query)$date
  DateRange(start_date, end_date)
})

#' @describeIn DateRange Change the date range of the date range object using
#'   the dates supplied in a vector of length 2, where the first element is the
#'   start date and second being the end date.
setMethod(
  f = "DateRange<-",
  signature = c("ANY", "ANY"),
  definition = function(object, value) {
    if (length(value) != 2) {
      as(object, "dateRange") <- as(value, "dateRange")
      object
    } else {
      startDate <- as(value[1], "Date")
      endDate <- as(value[2], "Date")
      new("dateRange", startDate, endDate)
      newDateRange <- new("dateRange", startDate, endDate)
      DateRange(object) <- newDateRange
      object
    }
  }
)

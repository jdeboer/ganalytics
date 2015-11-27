#' @include Date-generics.R
#' @include query-classes.R
#' @include init-methods.R
#' @include management-api-classes.R
#' @include date-coerce.R
#' @importFrom plyr adply
#' @importFrom stringr str_split_fixed
#' @importFrom lubridate today
#' @importFrom methods setMethod new
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
setMethod(
  f = "StartDate",
  signature = "character",
  definition = function(object) {
    as.Date(parse_date(object, output_format = kGaDateInFormat), format = kGaDateInFormat)
  }
)

#' @describeIn DateRange Coerce a character vector into a Google Analytics date
#'   object.
setMethod(
  f = "EndDate",
  signature = "character",
  definition = function(object) {
    as.Date(parse_date(object, output_format = kGaDateInFormat), format = kGaDateInFormat)
  }
)

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
  signature = c("dateRange", "character"),
  definition = function(object, value) {
    startDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- EndDate(object)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange Set a new end date for a date range.
setMethod(
  f = "EndDate<-",
  signature = c("dateRange", "character"),
  definition = function(object, value) {
    startDate <- StartDate(object)
    endDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange Set a new start date for a date range.
setMethod(
  f = "StartDate<-",
  signature = c("dateRange", "Date"),
  definition = function(object, value) {
    startDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- EndDate(object)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange Set a new end date for a date range.
setMethod(
  f = "EndDate<-",
  signature = c("dateRange", "Date"),
  definition = function(object, value) {
    startDate <- StartDate(object)
    endDate <- as.Date(parse_date(value, output_format = kGaDateInFormat), format = kGaDateInFormat)
    DateRange(startDate, endDate)
  }
)

#' @describeIn DateRange Set a new start date for a query.
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

#' @describeIn DateRange Set a new end date for a query.
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

#' @describeIn DateRange Set a new start date for a query.
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

#' @describeIn DateRange Set a new end date for a query.
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

#' @describeIn DateRange returns itself.
setMethod("DateRange", "dateRange", function(object) {object})

#' @describeIn DateRange generates a date range object from a vector of start
#'   and end date character vectors.
setMethod(
  f = "DateRange",
  signature = c("character", "character"),
  definition = function(object, endDate) {
    startDate <- as.Date(parse_date(object, output_format = kGaDateInFormat), format = kGaDateInFormat)
    endDate <- as.Date(parse_date(endDate, output_format = kGaDateInFormat), format = kGaDateInFormat)
    new("dateRange", startDate, endDate)
  }
)

#' @describeIn DateRange generates a date range object using the supplied
#'   vectors of start date and end dates.
setMethod(
  f = "DateRange",
  signature = c("Date", "Date"),
  definition = function(object, endDate) {
    startDate <- object
    new("dateRange", startDate, endDate)
  }
)

#' @describeIn DateRange Returns the date range of the given query.
setMethod("DateRange", ".standardQuery", function(object) {object@dateRange})

#' @describeIn DateRange Coerces a lubridate Interval to a ganalytics dateRange
#'   object.
setMethod("DateRange", "Interval", function(object) {
  as(object, "dateRange")
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
#'   the dates supplied in the character string of length 2, where the first
#'   element is the start date and second being the end date.
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

#' @describeIn DateRange Change the date range of the supplied date range object
#'   using the start and end date specified in the first and second element of
#'   the supplied Date vecotr.
setMethod(
  f = "DateRange<-",
  signature = c("dateRange", "Date"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange Set the date range of a date range object to that of
#'   the second supplied date range.
setMethod(
  f = "DateRange<-",
  signature = c("dateRange", "dateRange"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange Replace the date range with the date range from the
#'   supplied query.
setMethod(
  f = "DateRange<-",
  signature = c("dateRange", ".standardQuery"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange Replace the date range with date range from the
#'   supplied lubridate Interval.
setMethod(
  f = "DateRange<-",
  signature = c("dateRange", "Interval"),
  definition = function(object, value) {
    object <- as(value, "dateRange")
  }
)

#' @describeIn DateRange Replace the date range of a query with a start and end
#'   date specified in the supplied character vector of length 2.
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

#' @describeIn DateRange Replace the date range of the supplied query with the
#'   start and end date specified in the supplied date vector of length 2.
setMethod("DateRange<-", c(".standardQuery", "Date"), function(object, value) {
  as(object, "dateRange") <- as(value, "dateRange")
  object
})

#' @describeIn DateRange Replace the date range of the query with the supplied
#'   ganalytics dateRange object.
setMethod(
  f = "DateRange<-",
  signature = c(".standardQuery", "dateRange"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange Set the date range of a query using the supplied
#'   lubridate Interval
setMethod(
  f = "DateRange<-",
  signature = c(".standardQuery", "Interval"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

#' @describeIn DateRange Set the date range of a query using the date range of
#'   another query.
setMethod(
  f = "DateRange<-",
  signature = c(".standardQuery", ".standardQuery"),
  definition = function(object, value) {
    as(object, "dateRange") <- value
    object
  }
)

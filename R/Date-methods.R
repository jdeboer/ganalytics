#' @include Date-generics.R
#' @include query-classes.R
#' @include management-api-classes.R
#' @include date-coerce.R
#' @importFrom plyr adply
#' @importFrom stringr str_split_fixed
#' @importFrom lubridate today
#' @importFrom methods setMethod new as as<-
NULL

# setMethod(
#   f = "initialize",
#   signature = "dateRange",
#   definition = function(.Object, startDate, endDate) {
#     # If startDate and endDate are provided then
#     # bind every combination of startDate and endDate
#     # into a data.frame, keep only the unique rows,
#     # and use these start and end dates for this object.
#     if (!(missing(startDate) || missing(endDate))) {
#       dates <- do.call(
#         what = rbind,
#         args = mapply(
#           FUN = function(startDate, endDate) {
#             data.frame(
#               startDate = startDate,
#               endDate = endDate,
#               stringsAsFactors = FALSE
#             )
#           },
#           startDate,
#           endDate,
#           SIMPLIFY = FALSE,
#           USE.NAMES = FALSE
#         )
#       )
#       dates <- unique(dates)
#       .Object@startDate <- dates$startDate
#       .Object@endDate <- dates$endDate
#       validObject(.Object)
#     }
#     .Object
#   }
# )

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
SplitDateRange <- function(dateRange, N = 0L) {
  # TO DO
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
  if (N <= 0L | N > maxN) {
    N <- maxN
  }
  start <- min(StartDate(dateRange))
  end <- max(EndDate(dateRange))
  start <- seq(
    from = start,
    to = end + 1L,
    length.out = N + 1L
  )[-(N + 1L)]
  # Set new end dates
  end <- c(
    start[-1L] - 1L,
    end
  )
  DateRange(dateRange) <- DateRange(start, end)
  return(dateRange)
}

GetDataByDateRange <- function(query, dates) {
  adply(dates, 1L, function(dateRange) {
    DateRange(query) <- DateRange(dateRange$start, dateRange$end)
    output <- GetGaData(query)
    if (nrow(output) == 0L){output <- NULL}
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
  MaxResults(query) <- 1L
  TableFilter(query) <- Expr("hits", ">", 0L)
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
  MaxResults(query) <- 1L
  TableFilter(query) <- Expr("hits", ">", 0L)
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

#' @describeIn DateRange Returns the date range of the given query.
setMethod("DateRange", c(".standardQuery", "missing"), function(object) {
  object@dateRange
})

#' @describeIn DateRange Modify the date range of the given query.
setMethod("DateRange<-", c(".standardQuery", "ANY"), function(object, value) {
  object@dateRange <- as(value, "dateRange")
  validObject(object)
  object
})

#' @describeIn DateRange Returns the maximum date range of when a view has been
#'   receiving hits.
setMethod("DateRange", "gaView", function(object) {
  start_date <- as.Date(object$created)
  end_date <- today()
  query <- GaQuery(object)
  StartDate(query) <- start_date
  EndDate(query) <- end_date
  Metrics(query) <- "hits"
  Dimensions(query) <- "date"
  MaxResults(query) <- 1L
  TableFilter(query) <- Expr("hits", ">", 0L)
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
    if (length(value) != 2L) {
      as(object, "dateRange") <- as(value, "dateRange")
      object
    } else {
      startDate <- as(value[1L], "Date")
      endDate <- as(value[2L], "Date")
      new("dateRange", startDate, endDate)
      newDateRange <- new("dateRange", startDate, endDate)
      DateRange(object) <- newDateRange
      object
    }
  }
)

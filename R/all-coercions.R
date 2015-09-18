#' @include utils.R
#' @include management-api-classes.R
#' @include gtm-api-classes.R
#' @include init-methods.R
#' @importFrom assertthat assert_that
#' @importFrom stringr str_split str_split_fixed
NULL

# Coercing to logical
setAs(from = ".dimOperand", to = "logical",
      def = function(from, to) {
        YesNo <- c("Yes" = TRUE, "No" = FALSE)
        index <- pmatch(tolower(from), tolower(names(YesNo)))
        YesNo[index]
      }
)

# Coercion to matrix
setAs(
  from = "gaQuery",
  to = "matrix",
  def = function(from) {
    views <- as(from, "viewId")
    dateRange <- as(from, "dateRange")
    startDates <- dateRange@startDate
    endDates <- dateRange@endDate
    viewsDatesSegments <- do.call(
      what = rbind,
      args = lapply(
        X = views,
        FUN = function(viewId) {
          data.frame(
            startDate = startDates,
            endDate = endDates,
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(startDate, endDate, viewId) {
        metrics <- as(from, ".metrics")
        dimensions <- as(from, ".dimensions")
        sortBy <- as(from, ".sortBy")
        tableFilter <- as(from, ".tableFilter")
        segments <- as(from, ".gaSegment")
        c(
          "ids" = as(viewId, "character"),
          "start-date" = as.character(startDate),
          "end-date" = as.character(endDate),
          "metrics" = as(metrics, "character"),
          "dimensions" = if (length(dimensions) >= 1) {
            as(dimensions, "character")
          },
          "sort" = if (length(sortBy) >= 1) {
            as(sortBy, "character")
          },
          "filters" = if (length(tableFilter) >= 1) {
            as(tableFilter, "character")
          },
          "segment" = if (length(segments) >= 1) {
            as(segments, "character")
          },
          "samplingLevel" = as(from@samplingLevel, "character")
        )
      },
      viewsDatesSegments$startDate,
      viewsDatesSegments$endDate,
      viewsDatesSegments$viewId
    )
  }
)

setAs(
  from = "mcfQuery",
  to = "matrix",
  def = function(from) {
    views <- as(from, "viewId")
    dateRange <- as(from, "dateRange")
    startDates <- dateRange@startDate
    endDates <- dateRange@endDate
    viewsDates <- do.call(
      what = rbind,
      args = lapply(
        X = views,
        FUN = function(viewId) {
          data.frame(
            startDate = startDates,
            endDate = endDates,
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(startDate, endDate, viewId) {
        metrics <- as(from, ".metrics")
        dimensions <- as(from, ".dimensions")
        sortBy <- as(from, ".sortBy")
        tableFilter <- as(from, ".tableFilter")
        c(
          "ids" = as(viewId, "character"),
          "start-date" = as.character(startDate),
          "end-date" = as.character(endDate),
          "metrics" = as(metrics, "character"),
          "dimensions" = if (length(dimensions) >= 1) {
            as(dimensions, "character")
          },
          "sort" = if (length(sortBy) >= 1) {
            as(sortBy, "character")
          },
          "filters" = if (length(tableFilter) >= 1) {
            as(tableFilter, "character")
          },
          "samplingLevel" = as(from@samplingLevel, "character")
        )
      },
      viewsDates$startDate,
      viewsDates$endDate,
      viewsDates$viewId
    )
  }
)

setAs(
  from = "rtQuery",
  to = "matrix",
  def = function(from) {
    views <- do.call(
      what = rbind,
      args = lapply(
        X = as(from, "viewId"),
        FUN = function(viewId) {
          data.frame(
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(viewId) {
        metrics <- as(from, ".metrics")
        dimensions <- as(from, ".dimensions")
        sortBy <- as(from, ".sortBy")
        tableFilter <- as(from, ".tableFilter")
        c(
          "ids" = as(viewId, "character"),
          "metrics" = as(metrics, "character"),
          "dimensions" = if (length(dimensions) >= 1) {
            as(dimensions, "character")
          },
          "sort" = if (length(sortBy) >= 1) {
            as(sortBy, "character")
          },
          "filters" = if (length(tableFilter) >= 1) {
            as(tableFilter, "character")
          }
        )
      },
      views$viewId
    )
  }
)

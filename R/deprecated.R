#' @importFrom assertthat assert_that
NULL

# Backwards compatibility for deprecated functions

#' GaNot
#'
#' Deprecated, use Not or ! instead.
#'
#' @param ... arguments passed to \code{Not}
#'
#' @rdname GaNot
#' @keywords internal
#' @export GaNot
GaNot <- function(...){
  .Deprecated("Not")
  Not(...)
}

#' GaOr
#'
#' Deprecated, use Or or | instead.
#'
#' @param ... arguments passed to \code{Or}
#'
#' @rdname GaOr
#' @keywords internal
#' @export GaOr
GaOr <- function(...){
  .Deprecated("Or")
  Or(...)
}

#' GaAnd
#'
#' Deprecated, use And or & instead.
#'
#' @param ... arguments passed to \code{And}
#'
#' @rdname GaAnd
#' @keywords internal
#' @export GaAnd
GaAnd <- function(...){
  .Deprecated("And")
  And(...)
}

#' GaStartDate
#'
#' Deprecated, use StartDate instead.
#'
#' @param ... arguments passed onto \code{StartDate}
#'
#' @rdname GaStartDate
#' @keywords internal
#' @export GaStartDate
GaStartDate <- function(...){
  .Deprecated("StartDate")
  StartDate(...)
}

#' GaEndDate
#'
#' Deprecated, use EndDate instead.
#'
#' @param ... arguments passed onto \code{EndDate}
#'
#' @rdname GaEndDate
#' @keywords internal
#' @export GaEndDate
GaEndDate <- function(...){
  .Deprecated("EndDate")
  EndDate(...)
}

#' GaDateRange
#'
#' Deprecated, use DateRange instead.
#'
#' @param ... arguments passed onto \code{DateRange}
#'
#' @rdname GaDateRange
#' @keywords internal
#' @export GaDateRange
GaDateRange <- function(...){
  .Deprecated("DateRange")
  DateRange(...)
}

#' GaStartDate<-
#'
#' Deprecated, use StartDate<- instead.
#'
#' @param value passed onto \code{StartDate}
#'
#' @rdname GaStartDate
#' @keywords internal
#' @export GaStartDate<-
`GaStartDate<-` <- function(..., value){
  .Deprecated("StartDate<-")
  `StartDate<-`(..., value)
}

#' GaEndDate<-
#'
#' Deprecated, use EndDate instead.
#'
#' @param value passed onto \code{EndDate}
#'
#' @rdname GaEndDate
#' @keywords internal
#' @export GaEndDate<-
`GaEndDate<-` <- function(..., value){
  .Deprecated("EndDate<-")
  `EndDate<-`(..., value)
}

#' GaDateRange<-
#'
#' Deprecated, use DateRange instead.
#'
#' @param value passed onto \code{DateRange}
#'
#' @rdname GaDateRange
#' @keywords internal
#' @export GaDateRange<-
`GaDateRange<-` <- function(..., value){
  .Deprecated("DateRange<-")
  `DateRange<-`(..., value)
}

#' GaSplitDateRange
#'
#' Deprecated, use SplitDateRange instead.
#'
#' @param ... arguments passed onto \code{SplitDateRange}
#'
#' @keywords internal
#' @export GaSplitDateRange
GaSplitDateRange <- function(...){
  .Deprecated("SplitDateRange")
  SplitDateRange(...)
}

#' GaSegment
#'
#' Deprecated, use Segment instead.
#'
#' @param ... arguments passed onto \code{Segment}
#'
#' @rdname GaSegment
#' @keywords internal
#' @export GaSegment
GaSegment <- function(...){
  .Deprecated("Segment")
  Segment(...)
}

#' GaSegment<-
#'
#' Deprecated, use Segment<- instead.
#'
#' @param value passed onto \code{Segment}
#'
#' @rdname GaSegment
#' @keywords internal
#' @export GaSegment<-
`GaSegment<-` <- function(..., value){
  .Deprecated("Segment<-")
  `Segment<-`(..., value)
}

#' GaFilter
#'
#' Deprecated, use TableFilter instead.
#'
#' @param ... arguments passed onto \code{TableFilter}
#'
#' @rdname GaFilter
#' @keywords internal
#' @export GaFilter
GaFilter <- function(...){
  .Deprecated("TableFilter")
  TableFilter(...)
}

#' GaFilter<-
#'
#' Deprecated, use TableFilter<- instead.
#'
#' @param value passed onto \code{TableFilter<-}
#'
#' @rdname GaFilter
#' @keywords internal
#' @export GaFilter<-
`GaFilter<-` <- function(..., value){
  .Deprecated("TableFilter<-")
  `TableFilter<-`(..., value)
}

#' GaProfileId
#'
#' Deprecated, use GaView instead.
#'
#' @param ... arguments passed onto \code{GaView}
#'
#' @rdname GaProfileId
#' @keywords internal
#' @export GaProfileId
GaProfileId <- function(...){
  .Deprecated("GaView")
  GaView(...)
}

#' GaProfileId<-
#'
#' Deprecated, use GaView instead.
#'
#' @param value passed onto \code{GaView<-}
#'
#' @rdname GaProfileId
#' @keywords internal
#' @export GaProfileId<-
`GaProfileId<-` <- function(..., value){
  .Deprecated("GaView<-")
  `GaView<-`(..., value)
}

#' GaSamplingLevel
#'
#' Deprecated, use SamplingLevel instead.
#'
#' @param ... arguments passed onto \code{SamplingLevel}
#'
#' @export GaSamplingLevel
#' @keywords internal
#' @rdname GaSamplingLevel
GaSamplingLevel <- function(...){
  .Deprecated("SamplingLevel")
  SamplingLevel(...)
}

#' GaSamplingLevel<-
#'
#' Deprecated, use SamplingLevel<- instead.
#'
#' @param value passed onto \code{SamplingLevel<-}
#'
#' @export GaSamplingLevel<-
#' @keywords internal
#' @rdname GaSamplingLevel
`GaSamplingLevel<-` <- function(..., value){
  .Deprecated("SamplingLevel<-")
  `SamplingLevel<-`(..., value)
}

#' GaMaxResults
#'
#' Deprecated, use MaxResults instead.
#'
#' @param ... arguments passed onto \code{MaxResults}
#'
#' @export GaMaxResults
#' @keywords internal
#' @rdname GaMaxResults
GaMaxResults <- function(...){
  .Deprecated("MaxResults")
  MaxResults(...)
}

#' GaMaxResults<-
#'
#' Deprecated, use MaxResults<- instead.
#'
#' @param value passed onto \code{MaxResults<-}
#'
#' @export GaMaxResults<-
#' @keywords internal
#' @rdname GaMaxResults
`GaMaxResults<-` <- function(..., value){
  .Deprecated("MaxResults<-")
  `MaxResults<-`(..., value)
}

#' GaSortBy
#'
#' Deprecated, use SortBy instead.
#'
#' @param ... arguments passed onto \code{SortBy}
#'
#' @rdname GaSortBy
#' @keywords internal
#' @export GaSortBy
GaSortBy <- function(...){
  .Deprecated("SortBy")
  SortBy(...)
}

#' GaSortBy<-
#'
#' Deprecated, use SortBy<- instead.
#'
#' @param value passed onto \code{SortBy<-}
#'
#' @rdname GaSortBy
#' @keywords internal
#' @export GaSortBy<-
`GaSortBy<-` <- function(..., value){
  .Deprecated("SortBy<-")
  `SortBy<-`(..., value)
}

#' GaDimensions
#'
#' Deprecated, use \code{Dimensions} instead.
#'
#' @param ... arguments passed onto \code{Dimensions}
#'
#' @rdname GaDimensions
#' @keywords internal
#' @export GaDimensions
GaDimensions <- function(...){
  .Deprecated("Dimensions")
  Dimensions(...)
}

#' GaMetrics.
#'
#' Deprecated, use \code{Metrics} instead.
#'
#' @param ... arguments passed onto \code{Metrics}
#'
#' @rdname GaMetrics
#' @keywords internal
#' @export GaMetrics
GaMetrics <- function(...){
  .Deprecated("Metrics")
  Metrics(...)
}

#' GaDimensions<-
#'
#' Deprecated, use \code{Dimensions<-} instead.
#'
#' @param value passed onto \code{Dimensions}
#'
#' @rdname GaDimensions
#' @keywords internal
#' @export GaDimensions<-
`GaDimensions<-` <- function(..., value){
  .Deprecated("Dimensions<-")
  `Dimensions<-`(..., value)
}

#' GaMetrics<-
#'
#' Deprecated, use \code{Metrics<-} instead.
#'
#' @param value passed onto \code{Metrics}
#'
#' @rdname GaMetrics
#' @keywords internal
#' @export GaMetrics<-
`GaMetrics<-` <- function(..., value){
  .Deprecated("Metrics<-")
  `Metrics<-`(..., value)
}

#' GaSequence
#'
#' Deprecated, use \code{Sequence} instead.
#'
#' @param ... passed onto \code{Sequence}
#'
#' @rdname GaSequence
#' @keywords internal
#' @export GaSequence
GaSequence <- function(...) {
  .Deprecated("Sequence")
  Sequence(...)
}

#' GaScopeLevel
#'
#' Deprecated, use \code{ScopeLevel} instead
#'
#' @param ... passed onto \code{ScopeLevel}
#'
#' @rdname GaScopeLevel
#' @keywords internal
#' @export GaScopeLevel
GaScopeLevel <- function(...) {
  .Deprecated("ScopeLevel")
  ScopeLevel(...)
}

#' GaScopeLevel<-
#'
#' Deprecated, use \code{ScopeLevel<-} instead.
#'
#' @param value passed onto \code{ScopeLevel<-}
#'
#' @rdname GaScopeLevel
#' @keywords internal
#' @export GaScopeLevel<-
`GaScopeLevel<-` <- function(..., value) {
  .Deprecated("ScopeLevel<-")
  `ScopeLevel<-`(..., value)
}

#' GaCondition
#'
#' Deprecated, use \code{Include} or \code{Exclude} instead.
#'
#' @param ... passed onto \code{Include} or \code{Exclude}
#' @param negation used to select either Include or Exclude (if negation is
#'   FALSE).
#'
#' @rdname GaCondition
#' @keywords internal
#' @export GaCondition
GaCondition <- function(..., negation = FALSE) {
  if (isTRUE(negation)) {
    .Deprecated("Exclude")
    Exclude(...)
  } else {
    .Deprecated("Include")
    Include(...)
  }
}

#' GaSegmentFilters
#'
#' Deprecated, use \code{PerSession} or \code{PerUser} instead.
#'
#' @param ... passed onto \code{PerSession} or \code{PerUser}
#' @param scope used to select either PerUser or PerSession.
#'
#' @rdname GaSegmentFilters
#' @keywords internal
#' @export GaSegmentFilters
GaSegmentFilters <- function(..., scope = "sessions") {
  assert_that(length(scope) == 1)
  assert_that(scope %in% c("sessions", "users"))
  if (scope == "sessions") {
    .Deprecated("PerSession")
    PerSession(...)
  } else {
    .Deprecated("PerUser")
    PerUser(...)
  }
}


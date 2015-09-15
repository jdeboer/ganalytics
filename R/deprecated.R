# Backwards compatibility for deprecated functions

#' GaNot (Deprecated, use Not or ! instead).
#'
#' @param ... arguments passed to \code{Not}
#'
#' @rdname GaNot
#' @export GaNot
GaNot <- function(...){
  .Deprecated("Not")
  Not(...)
}

#' GaOr (Deprecated, use Or or | instead).
#'
#' @param ... arguments passed to \code{Or}
#'
#' @rdname GaOr
#' @export GaOr
GaOr <- function(...){
  .Deprecated("Or")
  Or(...)
}

#' GaAnd (Deprecated, use And or & instead).
#'
#' @param ... arguments passed to \code{And}
#'
#' @rdname GaAnd
#' @export GaAnd
GaAnd <- function(...){
  .Deprecated("And")
  And(...)
}

#' GaStartDate (Deprecated)
#'
#' @param ... arguments passed onto \code{StartDate}
#'
#' @rdname GaStartDate
#' @export GaStartDate
GaStartDate <- function(...){
  .Deprecated("StartDate")
  StartDate(...)
}

#' GaEndDate (Deprecated)
#'
#' @param ... arguments passed onto \code{EndDate}
#'
#' @rdname GaEndDate
#' @export GaEndDate
GaEndDate <- function(...){
  .Deprecated("EndDate")
  EndDate(...)
}

#' GaDateRange (Deprecated)
#'
#' @param ... arguments passed onto \code{DateRange}
#'
#' @rdname GaDateRange
#' @export GaDateRange
GaDateRange <- function(...){
  .Deprecated("DateRange")
  DateRange(...)
}

#' GaStartDate<- (Deprecated)
#'
#' @param value passed onto \code{StartDate}
#'
#' @rdname GaStartDate
#' @export GaStartDate<-
`GaStartDate<-` <- function(..., value){
  .Deprecated("StartDate<-")
  `StartDate<-`(..., value)
}

#' GaEndDate<- (Deprecated)
#'
#' @param value passed onto \code{EndDate}
#'
#' @rdname GaEndDate
#' @export GaEndDate<-
`GaEndDate<-` <- function(..., value){
  .Deprecated("EndDate<-")
  `EndDate<-`(..., value)
}

#' GaDateRange<- (Deprecated)
#'
#' @param value passed onto \code{DateRange}
#'
#' @rdname GaDateRange
#' @export GaDateRange<-
`GaDateRange<-` <- function(..., value){
  .Deprecated("DateRange<-")
  `DateRange<-`(..., value)
}

#' GaSplitDateRange (Deprecated)
#'
#' @param ... arguments passed onto \code{SplitDateRange}
#'
#' @export GaSplitDateRange
GaSplitDateRange <- function(...){
  .Deprecated("SplitDateRange")
  SplitDateRange(...)
}

#' GaSegment (Deprecated).
#'
#' @param ... arguments passed onto \code{Segment}
#'
#' @rdname GaSegment
#' @export GaSegment
GaSegment <- function(...){
  .Deprecated("Segment")
  Segment(...)
}

#' GaSegment<- (Deprecated).
#'
#' @param value passed onto \code{Segment}
#'
#' @rdname GaSegment
#' @export GaSegment<-
`GaSegment<-` <- function(..., value){
  .Deprecated("Segment<-")
  `Segment<-`(..., value)
}

#' GaFilter (Deprecated).
#'
#' @param ... arguments passed onto \code{TableFilter}
#'
#' @rdname GaFilter
#' @export GaFilter
GaFilter <- function(...){
  .Deprecated("TableFilter")
  TableFilter(...)
}

#' GaFilter<- (Deprecated).
#'
#' @param value passed onto \code{TableFilter<-}
#'
#' @rdname GaFilter
#' @export GaFilter<-
`GaFilter<-` <- function(..., value){
  .Deprecated("TableFilter<-")
  `TableFilter<-`(..., value)
}

#' GaProfileId (Deprecated).
#'
#' @param ... arguments passed onto \code{GaView}
#'
#' @rdname GaProfileId
#' @export GaProfileId
GaProfileId <- function(...){
  .Deprecated("GaView")
  GaView(...)
}

#' GaProfileId<- (Deprecated).
#'
#' @param value passed onto \code{GaView<-}
#'
#' @rdname GaProfileId
#' @export GaProfileId<-
`GaProfileId<-` <- function(..., value){
  .Deprecated("GaView<-")
  `GaView<-`(..., value)
}

#' GaSamplingLevel (Deprecated).
#'
#' @param ... arguments passed onto \code{SamplingLevel}
#'
#' @export GaSamplingLevel
#' @rdname GaSamplingLevel
GaSamplingLevel <- function(...){
  .Deprecated("SamplingLevel")
  SamplingLevel(...)
}

#' GaSamplingLevel<- (Deprecated).
#'
#' @param value passed onto \code{SamplingLevel<-}
#'
#' @export GaSamplingLevel<-
#' @rdname GaSamplingLevel
`GaSamplingLevel<-` <- function(..., value){
  .Deprecated("SamplingLevel<-")
  `SamplingLevel<-`(..., value)
}

#' GaMaxResults (Deprecated).
#'
#' @param ... arguments passed onto \code{MaxResults}
#'
#' @export GaMaxResults
#' @rdname GaMaxResults
GaMaxResults <- function(...){
  .Deprecated("MaxResults")
  MaxResults(...)
}

#' GaMaxResults<- (Deprecated).
#'
#' @param value passed onto \code{MaxResults}
#'
#' @export GaMaxResults<-
#' @rdname GaMaxResults
`GaMaxResults<-` <- function(..., value){
  .Deprecated("MaxResults<-")
  `MaxResults<-`(..., value)
}

#' GaSortBy (Deprecated).
#'
#' @param ... arguments passed onto \code{SortBy}
#'
#' @rdname GaSortBy
#' @export GaSortBy
GaSortBy <- function(...){
  .Deprecated("SortBy")
  SortBy(...)
}

#' GaSortBy<- (Deprecated).
#'
#' @param value passed onto \code{SortBy<-}
#'
#' @rdname GaSortBy
#' @export GaSortBy<-
`GaSortBy<-` <- function(..., value){
  .Deprecated("SortBy<-")
  `SortBy<-`(..., value)
}

#' GaDimensions (Deprecated).
#'
#' Use \code{Dimensions} instead of \code{GaDimensions}
#' @rdname GaDimensions
#' @param ... arguments passed onto \code{Dimensions}
#' @export GaDimensions
GaDimensions <- function(...){
  .Deprecated("Dimensions")
  Dimensions(...)
}

#' GaMetrics (Deprecated).
#'
#' Use \code{Metrics} instead of \code{GaMetrics}
#' @rdname GaMetrics
#' @param ... arguments passed onto \code{Metrics}
#' @export GaMetrics
GaMetrics <- function(...){
  .Deprecated("Metrics")
  Metrics(...)
}

#' Use \code{Dimensions<-} instead of \code{GaDimensions<-}
#' @rdname GaDimensions
#' @param value passed onto \code{Dimensions}
#' @export GaDimensions<-
`GaDimensions<-` <- function(..., value){
  .Deprecated("Dimensions<-")
  `Dimensions<-`(..., value)
}

#' Use \code{Metrics<-} instead of \code{GaMetrics<-}
#' @rdname GaMetrics
#' @param value passed onto \code{Metrics}
#' @export GaMetrics<-
`GaMetrics<-` <- function(..., value){
  .Deprecated("Metrics<-")
  `Metrics<-`(..., value)
}

#' Use \code{Sequence} instead of \code{GaSequence}
#' @rdname GaSequence
#' @param ... passed onto \code{Sequence}
#' @export GaSequence
GaSequence <- function(...) {
  .Deprecated("Sequence")
  Sequence(...)
}

#' Use \code{ScopeLevel} instead of \code{GaScopeLevel}
#' @rdname GaScopeLevel
#' @param ... passed onto \code{ScopeLevel}
#' @export GaScopeLevel
GaScopeLevel <- function(...) {
  .Deprecated("ScopeLevel")
  ScopeLevel(...)
}

#' Use \code{ScopeLevel<-} instead of \code{GaScopeLevel<-}
#' @rdname GaScopeLevel
#' @param value passed onto \code{ScopeLevel<-}
#' @export GaScopeLevel<-
`GaScopeLevel<-` <- function(..., value) {
  .Deprecated("ScopeLevel<-")
  `ScopeLevel<-`(..., value)
}


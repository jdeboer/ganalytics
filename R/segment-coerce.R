#' @include utils.R
NULL

### Review the following coercions using "new"
# Coercion to custom segment classes

setAs(from = ".compoundExpr", to = "gaSegmentSequenceStep", def = function(from, to) {
  new(to, as(from, "andExpr"))
})

setAs(from = ".compoundExpr", to = "gaSegmentSequenceFilter", def = function(from, to) {
  new(to, as(as(from, "gaSegmentSequenceStep"), "andExpr"))
})

# Coercing to gaSegmentConditionFilter
setAs(from = ".compoundExpr", to = "gaSegmentConditionFilter", def = function(from, to) {
  new(to, as(from, "andExpr"))
})

# Coercing to gaSegmentFilterList
setAs(from = ".compoundExpr", to = "gaSegmentFilterList", def = function(from, to) {
  new(to, list(as(from, "gaSegmentConditionFilter")))
})

# Coercion to gaSegmentId

setAs(from = "character", to = "gaSegmentId", def = simpleCoerce)

setAs(from = "numeric", to = "gaSegmentId", def = function(from, to) {
  new(to, as.character(from))
})

setAs(from = "gaUserSegment", to = "gaSegmentId", def = function(from, to) {
  new(to, Segment(from))
})

# Coercing to gaDynSegment

setAs(from = "gaFilter", to = "gaDynSegment", def = simpleCoerceData)

setAs(from = "orExpr", to = "gaDynSegment", def = function(from, to) {
  as(as(from, "andExpr"), to)
})

#Review this coercion method
setAs(from = "andExpr", to = "gaDynSegment", def = function(from, to) {
  new(to, list(SegmentFilters(SegmentConditionFilter(from))))
})

setAs(from = ".expr", to = "gaDynSegment", def = function(from, to) {
  as(as(from, "andExpr"), to)
})

# Coercion to numeric

setAs(
  from = "gaSegmentId",
  to = "numeric",
  def = function(from) {
    as.numeric(
      sub(
        pattern = "ga:([0-9]+)",
        replacement = "\\1",
        x = from@.Data
      )
    )
  },
  replace = function(from, value) {
    initialize(from, as.character(value))
  }
)

#############\/ Transform to method of Segment and Segment<- generic functions
setAs(from = "gaQuery", to = ".gaSegment",
      def = function(from) {
        from@segments
      },
      replace = function(from, value) {
        from@segments <- as(value, ".gaSegment") # Need to define coercions to .gaSegment from char and numeric
        validObject(from)
        from
      }
)


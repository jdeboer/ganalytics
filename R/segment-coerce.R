#' @include utils.R
#' @importFrom methods initialize
NULL

# Coercion to custom segment classes
setAs(from = ".compoundExpr", to = "gaSegmentSequenceStep", def = function(from, to) {
  new(to, as(from, "andExpr"))
})

setAs(from = ".compoundExpr", to = ".gaSegmentFilter", def = function(from, to) {
  as(from, "gaSegmentConditionFilter")
})

setAs(from = "gaSegmentCondition", to = ".gaSegmentFilter", def = function(from, to) {
  as(from, "gaSegmentConditionFilter")
})

setAs(from = "gaSegmentSequenceStep", to = ".gaSegmentFilter", def = function(from, to) {
  as(from, "gaSegmentSequenceFilter")
})

setAs(from = ".compoundExpr", to = "gaSegmentSequenceFilter", def = function(from, to) {
  new(to, list(as(from, "gaSegmentSequenceStep")))
})

# Coercing to gaSegmentConditionFilter
setAs(from = ".compoundExpr", to = "gaSegmentConditionFilter", def = function(from, to) {
  new(to, as(from, "andExpr"))
})

# Coercing to gaSegmentFilterList
setAs(from = ".compoundExpr", to = "gaSegmentFilterList", def = function(from, to) {
  new(to, list(as(from, "gaSegmentConditionFilter")))
})
setAs(from = ".gaSegmentFilter", to = "gaSegmentFilterList", def = function(from, to) {
  new(to, list(from))
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

setAs(from = "gaSegmentFilterList", to = "gaDynSegment", def = function(from, to) {
  new(to, list(from))
})

setAs(from = ".compoundExpr", to = ".gaSegment", def = function(from, to) {
  as(from, "gaDynSegment")
})

setAs(from = "gaFilter", to = ".gaSegment", def = function(from, to) {
  as(from, "gaDynSegment")
})

setAs(from = ".gaSegmentFilter", to = ".gaSegment", def = function(from, to) {
  as(from, "gaDynSegment")
})

setAs(from = "gaSegmentFilterList", to = ".gaSegment", def = function(from, to) {
  as(from, "gaDynSegment")
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

setAs(from = ".gaSegment", to = "gaSegmentList",
      def = function(from) {
        simpleCoerceToList(from, to)
      }
)

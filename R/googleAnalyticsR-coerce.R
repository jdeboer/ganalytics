#' @importClassesFrom googleAnalyticsR dim_fil_ga4 met_fil_ga4 orFiltersForSegment_ga4
#' @importClassesFrom googleAnalyticsR segmentDef_ga4 segmentFilterClause_ga4
#' @importClassesFrom googleAnalyticsR segmentFilter_ga4 segmentSequenceStep_ga4
#' @importClassesFrom googleAnalyticsR sequenceSegment_ga4 simpleSegment_ga4
#' @importFrom methods setAs
NULL

setClass("segment_ga4")
setClass("dynamicSegment_ga4")

get_expression_details <- function(from, var_operators) {
  varName <- as.character(Var(from))
  names(varName) <- sub("^ga:", "", varName)
  operator <- Comparator(from)
  negated <- operator %in% kGa4Ops$negated_operators
  if(negated) operator <- Not(operator)
  operator_lookup_index <- match(as.character(operator), var_operators)
  operator_name <- names(var_operators)[operator_lookup_index]
  operand <- as.character(Operand(from))
  expressions <- character(0)
  minComparisonValue <- character(0)
  maxComparisonValue <- character(0)
  if(operator == "<>") {
    minComparisonValue <- operand[1]
    maxComparisonValue <- operand[2]
  } else if(inherits(from, ".metExpr")) {
    minComparisonValue <- operand
  } else {
    expressions <- operand
  }
  list(
    varName = varName,
    operator = operator,
    operator_name = operator_name,
    negated = negated,
    expressions = expressions,
    minComparisonValue = minComparisonValue,
    maxComparisonValue = maxComparisonValue
  )
}

setAs("gaDimExpr", "dim_fil_ga4", def = function(from, to) {
  dim_operation <- get_expression_details(from, kGa4Ops$dimension_operators)
  x <- list(
    dimensionName = dim_operation$varName,
    not = dim_operation$negated,
    operator = dim_operation$operator_name,
    expressions = as.list(as.character(Operand(from))),
    caseSensitive = FALSE
  )
  class(x) <- "dim_fil_ga4"
  x
})

setAs("gaMetExpr", "met_fil_ga4", def = function(from, to) {
  met_operation <- get_expression_details(from, kGa4Ops$metric_operators)
  x <- list(
    metricName = met_operation$varName,
    not = met_operation$negated,
    operator = met_operation$operator_name,
    comparisonValue = as.character(Operand(from))
  )
  class(x) <- "met_fil_ga4"
  x
})

setAs("gaDimExpr", "segmentFilterClause_ga4", def = function(from, to) {
  exp_details <- get_expression_details(from, kGa4Ops$dimension_operators)
  segmentDimensionFilter <- list(
    dimensionName = exp_details$varName,
    operator = exp_details$operator_name,
    caseSensitive = NULL,
    expressions = exp_details$expressions,
    minComparisonValue = exp_details$minComparisonValue,
    maxComparisonValue = exp_details$maxComparisonValue
  )
  class(segmentDimensionFilter) <- "segmentDimFilter_ga4"
  x <- list(
    not = exp_details$negated,
    dimensionFilter = segmentDimensionFilter,
    metricFilter = NULL
  )
  class(x) <- "segmentFilterClause_ga4"
  x
})

setAs("gaMetExpr", "segmentFilterClause_ga4", def = function(from, to) {
  from <- as(from, "gaSegMetExpr")
  as(from, to)
})

setAs("gaSegMetExpr", "segmentFilterClause_ga4", def = function(from, to) {
  exp_details <- get_expression_details(from, kGa4Ops$metric_operators)
  scope <- c("perHit" = "HIT", "perSession" = "SESSION", "perUser" = "USER")[[ScopeLevel(from)]]
  segmentMetricFilter <- list(
    scope = scope,
    metricName = exp_details$varName,
    operator = exp_details$operator_name,
    comparisonValue = exp_details$minComparisonValue,
    maxComparisonValue = exp_details$maxComparisonValue
  )
  class(segmentMetricFilter) <- "segmentMetFilter_ga4"
  x <- list(
    not = exp_details$negated,
    dimensionFilter = NULL,
    metricFilter = segmentMetricFilter
  )
  class(x) <- "segmentFilterClause_ga4"
  x
})

setAs("orExpr", "orFiltersForSegment_ga4", def = function(from, to) {
  x <- list(
    segmentFilterClauses = lapply(from, as, "segmentFilterClause_ga4")
  )
  class(x) <- "orFiltersForSegment_ga4"
  x
})

setAs("andExpr", "simpleSegment_ga4", def = function(from, to) {
  x <- list(
    orFiltersForSegment = lapply(from, as, "orFiltersForSegment_ga4")
  )
  class(x) <- "simpleSegment_ga4"
  x
})

setAs("gaSegmentSequenceStep", "segmentSequenceStep_ga4", def = function(from, to) {
  matchType <- if(from@immediatelyPrecedes) "IMMEDIATELY_PRECEDES" else "PRECEDES"
  x <- c(
    as(as(from@.Data, "andExpr"), "simpleSegment_ga4"),
    list(matchType = matchType)
  )
  class(x) <- "segmentSequenceStep_ga4"
  x
})

setAs("gaSegmentSequenceFilter", "sequenceSegment_ga4", def = function(from, to) {
  segmentSequenceSteps <- lapply(from, as, "segmentSequenceStep_ga4")
  x <- list(
    segmentSequenceSteps = segmentSequenceSteps,
    firstStepShouldMatchFirstHit = from[[1]]@immediatelyPrecedes
  )
  class(x) <- "sequenceSegment_ga4"
  x
})

setAs("gaSegmentConditionFilter", "segmentFilter_ga4", def = function(from, to) {
  x <- list(
    not = IsNegated(from),
    simpleSegment = as(from, "simpleSegment_ga4"),
    sequenceSegment = NULL
  )
  class(x) <- "segmentFilter_ga4"
  x
})

setAs("gaSegmentSequenceFilter", "segmentFilter_ga4", def = function(from, to) {
  x <- list(
    not = IsNegated(from),
    simpleSegment = NULL,
    sequenceSegment = as(from, "sequenceSegment_ga4")
  )
  class(x) <- "segmentFilter_ga4"
  x
})

setAs("gaDynSegment", "segmentDef_ga4", def = function(from, to) {
  x <- list(
    segmentFilters = lapply(from, as, "segmentFilter_ga4")
  )
  class(x) <- "segmentDef_ga4"
  x
})

setAs("gaSegmentConditionFilter", "segmentDef_ga4", def = function(from, to) {
  as(as(from, "gaDynSegment"), "segmentDef_ga4")
})

setAs("gaSegmentSequenceFilter", "segmentDef_ga4", def = function(from, to) {
  as(as(from, "gaDynSegment"), "segmentDef_ga4")
})

setAs("gaDynSegment", "dynamicSegment_ga4", def = function(from, to) {
  dyn_segment <- list(
    name = from@name,
    userSegment = as(select_segment_filters_with_scope(from, scope = "users"), "segmentDef_ga4"),
    sessionSegment = as(select_segment_filters_with_scope(from, scope = "sessions"), "segmentDef_ga4")
  )
  class(dyn_segment) <- "dynamicSegment_ga4"
  dyn_segment
})

setAs("gaSegmentList", "segment_ga4", def = function(from, to) {
  segment_list <- lapply(seq_along(from), function(segment_i) {
    segment_name <- names(from)[segment_i]
    segment <- from[[segment_i]]
    switch (class(segment),
      gaDynSegment = {
        segment@name = segment_name
        list(dynamicSegment = as(segment, "dynamicSegment_ga4"))
      },
      gaSegmentId = list(segmentId = as(segment, "character"))
    )
  })
  class(segment_list) <- to
  segment_list
})

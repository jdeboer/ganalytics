#' @include utils.R
NULL

# Coercion to .filter subclasses

# Consider having coercision just to .tableFilter where coercion requires
# call to initialise where the object is coerced to a specific type of .tableFilter
# class, i.e GA, RT or MCF subclasses.

setAs(from = "andExpr", to = ".tableFilter", def = function(from) {
  if (all_inherit(unlist(from), ".gaExpr")) {
    as(from, "gaFilter")
  } else if (all_inherit(unlist(from), ".mcfExpr")) {
    as(from, "mcfFilter")
  } else if (all_inherit(unlist(from), ".rtExpr")) {
    as(from, "rtFilter")
  } else stop("Cannot determine type of filter.")
})

setAs(from = "NULL", to = "gaFilter", def = coerceViaList)
setAs(from = "NULL", to = "mcfFilter", def = coerceViaList)
setAs(from = "NULL", to = "rtFilter", def = coerceViaList)

setAs(from = "NULL", to = ".tableFilter", def = coerceViaList)

setAs(from = "andExpr", to = "gaFilter", def = simpleCoerce)
setAs(from = "andExpr", to = "mcfFilter", def = simpleCoerce)
setAs(from = "andExpr", to = "rtFilter", def = simpleCoerce)

setAs(from = "orExpr", to = "gaFilter", def = coerceViaAnd)
setAs(from = "orExpr", to = "mcfFilter", def = coerceViaAnd)
setAs(from = "orExpr", to = "rtFilter", def = coerceViaAnd)
setAs(from = "orExpr", to = ".tableFilter", def = coerceViaAnd)

setAs(from = ".expr", to = "gaFilter", def = coerceViaAnd)
setAs(from = ".expr", to = "mcfFilter", def = coerceViaAnd)
setAs(from = ".expr", to = "rtFilter", def = coerceViaAnd)
setAs(from = ".expr", to = ".tableFilter", def = coerceViaAnd)

setAs(from = "gaSegmentCondition", to = ".tableFilter", def = simpleCoerceData)

#############\/ Transform to method of TableFilter and TableFilter<- generic functions

setAs(from = ".query", to = ".tableFilter",
      def = function(from, to){
        from@filters
      },
      replace = function(from, value) {
        use_class <- class(from@filters)
        from@filters <- as(value, use_class)
        validObject(from)
        from
      }
)


#' @include utils.R
NULL

# Coercing to .comparator subclasses
setAs(from = "character", to = "gaDimComparator", def = simpleCoerce)
setAs(from = "character", to = "gaMetComparator", def = simpleCoerce)
setAs(from = "character", to = "mcfDimComparator", def = simpleCoerce)
setAs(from = "character", to = "mcfMetComparator", def = simpleCoerce)
setAs(from = "character", to = "rtDimComparator", def = simpleCoerce)
setAs(from = "character", to = "rtMetComparator", def = simpleCoerce)

#############\/ Transform to method of Comparator and Comparator<- generic functions

setAs(from = ".expr", to = ".comparator",
      def = function(from, to) {
        from@comparator
      },
      replace = function(from, value) {
        use_class <- class(from@comparator)
        from@comparator <- as(value, use_class)
        validObject(from)
        from
      })

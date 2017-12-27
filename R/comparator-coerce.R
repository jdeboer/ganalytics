#' @include utils.R
NULL

# Coercing to .comparator subclasses
setAs(from = "character", to = "gaDimComparator", def = simpleCoerce)
setAs(from = "character", to = "gaMetComparator", def = simpleCoerce)
setAs(from = "character", to = "mcfDimComparator", def = simpleCoerce)
setAs(from = "character", to = "mcfMetComparator", def = simpleCoerce)
setAs(from = "character", to = "rtDimComparator", def = simpleCoerce)
setAs(from = "character", to = "rtMetComparator", def = simpleCoerce)


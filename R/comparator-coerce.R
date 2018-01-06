#' @include utils.R
#' @importFrom methods setAs
NULL

# Coercing to .comparator subclasses
setAs(from = "character", to = "gaDimComparator", def = simpleCoerce)
setAs(from = "character", to = "gaMetComparator", def = simpleCoerce)
setAs(from = "character", to = "mcfDimComparator", def = simpleCoerce)
setAs(from = "character", to = "mcfMetComparator", def = simpleCoerce)
setAs(from = "character", to = "rtDimComparator", def = simpleCoerce)
setAs(from = "character", to = "rtMetComparator", def = simpleCoerce)
setAs(
  from = "gaDimComparator", to = "character",
  def = function(from, to) {
    as.character(from)
  },
  replace = function(from, value) {
    new(class(from), value)
  }
)
setAs(
  from = "gaMetComparator", to = "character",
  def = function(from, to) {
    as.character(from)
  },
  replace = function(from, value) {
    new(class(from), value)
  }
)

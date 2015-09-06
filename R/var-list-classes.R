#' @include var-classes.R
#' @include utils.R
#' @importFrom methods setClass setClassUnion setValidity prototype new
#' @importFrom assertthat validate_that
NULL

# ---- query dimensions, metrics, and sortby lists ----

#' `.metrics` class.
#'
#' An S4 class to represent a list of metrics.
#'
#' @rdname metrics-class
#' @keywords internal
#'
#' @export
setClass(
  ".metrics",
  contains = "list",
  validity = function(object) {
    if (!all_inherit(object, ".metVar")) {
      return("Must be a list containing objects of class .metVar")
    } else TRUE
    if (length(object) > kGaMax$metrics) {
      return(paste("Maximum of", kGaMax$metrics, "metrics allowed."))
    } else TRUE
  }
)

#' `.dimensions` class.
#'
#' An S4 class to represent a list of dimensions.
#'
#' @rdname dimensions-class
#' @keywords internal
#'
#' @export
setClass(
  ".dimensions",
  contains = "list",
  validity = function(object) {
    if (!all_inherit(object, ".dimVar")) {
      return("Must be a list containing objects of class .dimVar")
    } else TRUE
    if (length(object) > kGaMax$dimensions) {
      return(paste("Maximum of", kGaMax$dimensions, "dimensions allowed."))
    } else TRUE
  }
)

#' `.sortBy` class.
#'
#' An S4 class to represent a list of variables to sort by.
#'
#' @rdname sortBy-class
#' @keywords internal
#'
#' @export
setClass(
  ".sortBy",
  slots = c(
    desc = "logical"
  ),
  prototype = prototype(
    list(),
    desc = logical()
  ),
  contains = "list",
  validity = function(object) {
    if (length(object@.Data) != length(object@desc)) {
      "List vector and desc vector must be of equal lengths"
    } else TRUE
  }
)

#' `.varList` class.
#'
#' An S4 class to represent .
#'
#' @docType class
#' @name .varList-class
#' @rdname varList-class
#' @keywords internal
#'
#' @exportClass .varList
setClassUnion(".varList", c(".metrics", ".dimensions", ".sortBy"))

setValidity(".varList", function(object) {
  if (!all_inherit(object, ".var") & length(object@.Data) > 0) {
    "Must be a list containing objects of class .var"
  } else TRUE
})

#' `gaMetrics` class.
#'
#' An S4 class to represent a list of Core Reporting metrics.
#'
#' @rdname gaMetrics-class
#' @keywords internal
#'
#' @export
setClass(
  "gaMetrics",
  prototype = prototype(
    list(new("gaMetVar"))
  ),
  contains = ".metrics"
)

#' `mcfMetrics` class.
#'
#' An S4 class to represent a list of Multi-channel funnel metrics.
#'
#' @rdname mcfMetrics-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfMetrics",
  prototype = prototype(
    list(new("mcfMetVar"))
  ),
  contains = ".metrics"
)

#' `rtMetrics` class.
#'
#' An S4 class to represent a list of Real-time metrics.
#'
#' @rdname rtMetrics-class
#' @keywords internal
#'
#' @export
setClass(
  "rtMetrics",
  prototype = prototype(
    list(new("rtMetVar"))
  ),
  contains = ".metrics"
)

#' `gaDimensions` class.
#'
#' An S4 class to represent a list of Core Reporting dimensions.
#'
#' @rdname gaDimensions-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDimensions",
  prototype = prototype(
    list(new("gaDimVar"))
  ),
  contains = ".dimensions"
)

#' `mcfDimensions` class.
#'
#' An S4 class to represent a list of Multi-Channel Funnel dimensions.
#'
#' @rdname mcfDimensions-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfDimensions",
  prototype = prototype(
    list(new("mcfDimVar"))
  ),
  contains = ".dimensions"
)

#' `rtDimensions` class.
#'
#' An S4 class to represent a list of Real-Time reporting dimensions.
#'
#' @rdname rtDimensions-class
#' @keywords internal
#'
#' @export
setClass(
  "rtDimensions",
  prototype = prototype(
    list(new("rtDimVar"))
  ),
  contains = ".dimensions"
)

#' `gaSortBy` class.
#'
#' An S4 class to represent Core Reporting sorting lists.
#'
#' @rdname gaSortBy-class
#' @keywords internal
#'
#' @export
setClass("gaSortBy", contains = ".sortBy")

#' `mcfSortBy` class.
#'
#' An S4 class to represent Multi-Channel Funnel sorting lists.
#'
#' @rdname mcfSortBy-class
#' @keywords internal
#'
#' @export
setClass("mcfSortBy", contains = ".sortBy")

#' `rtSortBy` class.
#'
#' An S4 class to represent Real-Time variable sorting lists.
#'
#' @rdname rtSortBy-class
#' @keywords internal
#'
#' @export
setClass("rtSortBy", contains = ".sortBy")

#' `.gaVarList` class.
#'
#' An S4 class to represent Core Reporting variable lists.
#'
#' @docType class
#' @keywords internal
#' @name .gaVarList-class
#' @rdname gaVarList-class
#' @exportClass .gaVarList
setClassUnion(".gaVarList", c("gaMetrics", "gaDimensions", "gaSortBy"))

setValidity(".gaVarList", function(object) {
  if (!all_inherit(object, ".gaVar")) {
    "Must be a list containing objects of class .gaVar"
  } else TRUE
})

#' `mcfVarList` class.
#'
#' An S4 class to represent Multi-Channel Funnel variable lists.
#'
#' @docType class
#' @name .mcfVarList-class
#' @rdname mcfVarList-class
#' @keywords internal
#'
#' @exportClass .mcfVarList
setClassUnion(".mcfVarList", c("mcfMetrics", "mcfDimensions", "mcfSortBy"))

setValidity(".mcfVarList", function(object) {
  if (!all_inherit(object, ".mcfVar")) {
    "Must be a list containing objects of class .mcfVar"
  } else TRUE
})

#' `.rtVarList` class.
#'
#' An S4 class union of Real-Time variable lists.
#'
#' @docType class
#' @name .rtVarList-class
#' @rdname rtVarList-class
#' @keywords internal
#'
#' @exportClass .rtVarList
setClassUnion(".rtVarList", c("rtMetrics", "rtDimensions", "rtSortBy"))

setValidity(".rtVarList", function(object) {
  if (!all_inherit(object, ".rtVar")) {
    "Must be a list containing objects of class .rtVar"
  } else TRUE
})

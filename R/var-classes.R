#' @include utils.R
#' @include globaldata.R
#' @importFrom methods setClass setClassUnion prototype
#' @importFrom assertthat validate_that
NULL

# ---- dimension and metric variables ----

#' `.var` class.
#'
#' An S4 class to represent the name of a single Google Analytics dimension or
#' metric variable.
#'
#' @rdname var-class
#' @keywords internal
#'
#' @export
setClass(
  ".var",
  contains = "character",
  validity = function(object) {
    validate_that(length(object) == 1)
  }
)

#' `gaMetVar` class.
#'
#' An S4 class to represent a valid Google Analytics Core Reporting API metric
#' name.
#'
#' Set to "ga:sessions" by default.
#'
#' @rdname gaMetVar-class
#' @keywords internal
#'
#' @export
setClass(
  "gaMetVar",
  prototype = prototype("ga:sessions"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kGaVars$mets)) {
      TRUE
    } else {
      paste("Invalid GA metric name", object@.Data, sep = ": ")
    }
  }
)

#' `gaDimVar` class.
#'
#' An S4 class to represent a valid Google Analytics Core Reporting API dimension
#' name.
#'
#' Set to "ga:date" by default.
#'
#' @rdname gaDimVar-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDimVar",
  prototype = prototype("ga:date"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kGaVars$dims)) {
      TRUE
    } else {
      paste("Invalid GA dimension name", object@.Data, sep = ": ")
    }
  }
)

#' `mcfMetVar` class.
#'
#' An S4 class to represent a Multi-Channel Funnel metric.
#'
#' Set to "mcf:totalConversions" by default.
#'
#' @rdname mcfMetVar-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfMetVar",
  prototype = prototype("mcf:totalConversions"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kMcfVars$mets)) {
      TRUE
    } else {
      paste("Invalid MCF metric name", object@.Data, sep = ": ")
    }
  }
)

#' `mcfDimVar` class.
#'
#' An S4 class to represent a Multi-Channel Funnel dimension.
#'
#' Set to "mcf:nthDay" by default.
#'
#' @rdname mcfDimVar-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfDimVar",
  prototype = prototype("mcf:nthDay"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kMcfVars$dims)) {
      TRUE
    } else {
      paste("Invalid MCF dimension name", object@.Data, sep = ": ")
    }
  }
)

#' `rtMetVar` class.
#'
#' An S4 class to represent a Real-Time reporting metric.
#'
#' Set to "rt:pageviews" by default.
#'
#' @rdname rtMetVar-class
#' @keywords internal
#'
#' @export
setClass(
  "rtMetVar",
  prototype = prototype("rt:pageviews"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kRtVars$mets)) {
      TRUE
    } else {
      paste("Invalid RT metric name", object@.Data, sep = ": ")
    }
  }
)

#' `rtDimVar` class.
#'
#' An S4 class to represent a Real-Time reporting dimension.
#'
#' Set to "rt:minutesAgo" by default.
#'
#' @rdname rtDimVar-class
#' @keywords internal
#'
#' @export
setClass(
  "rtDimVar",
  prototype = prototype("rt:minutesAgo"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kRtVars$dims)) {
      TRUE
    } else {
      paste("Invalid RT dimension name", object@.Data, sep = ": ")
    }
  }
)

#' `.gaVar` class.
#'
#' An S4 class union to represent a Core Reporting variable.
#'
#' @docType class
#' @name .gaVar-class
#' @rdname gaVar-class
#' @keywords internal
#'
#' @exportClass .gaVar
setClassUnion(".gaVar", c("gaMetVar", "gaDimVar"))

#' `.mcfVar` class.
#'
#' An S4 class to represent a Multi-Channel Funnel variable.
#'
#' @docType class
#' @name .mcfVar-class
#' @rdname mcfVar-class
#' @keywords internal
#'
#' @exportClass .mcfVar
setClassUnion(".mcfVar", c("mcfMetVar", "mcfDimVar"))

#' `.rtVar` class.
#'
#' An S4 class to represent a Real-Time reporting variable.
#'
#' @docType class
#' @name .rtVar-class
#' @rdname rtVar-class
#' @keywords internal
#'
#' @exportClass .rtVar
setClassUnion(".rtVar", c("rtMetVar", "rtDimVar"))

#' `.metVar` class.
#'
#' An S4 class to represent a metric.
#'
#' @docType class
#' @name .metVar-class
#' @rdname metVar-class
#' @keywords internal
#'
#' @exportClass .metVar
setClassUnion(".metVar", c("gaMetVar", "mcfMetVar", "rtMetVar"))

#' `.dimVar` class.
#'
#' An S4 class to represent a dimension.
#'
#' @docType class
#' @name .dimVar-class
#' @rdname dimVar-class
#' @keywords internal
#'
#' @exportClass .dimVar
setClassUnion(".dimVar", c("gaDimVar", "mcfDimVar", "rtDimVar"))

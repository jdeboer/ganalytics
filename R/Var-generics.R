#' Var
#'
#' Google Analytics dimension and metric variables.
#'
#' \code{Var} returns a \code{.var} object which is valid Google Analytics
#' dimension or metric for use with the core reporting, multi-channel-funnel
#' reporting or real-time reporting API.
#'
#' Use \code{Var} to lookup a dimension or metric from the Google Analytics core
#' reporting, multi-channel-funnel reporting, or real-time reporting APIs, for
#' use in defining expressions (of superclass \code{.expr}) or (to be
#' implemented) variable lists (of superclass \code{.varList}) such as query
#' dimensions, metrics or sortBy parameters.
#'
#' \code{Var} accepts either a character, \code{.var}, or \code{.expr} object. A
#' character object will be coerced to a \code{.var} object by looking for a
#' matching dimension or metric from the Core Reporting, Multi-Channel Funnel
#' Reporting, and Real-Time Reporting APIs. Providing an \code{.expr} object
#' will return the dimension or metric used within that Google Analytics
#' expression.
#'
#' @param object An object that inherits from or extends the class \code{.var},
#'   including \code{gaDimVar}, \code{gaMetVar}, \code{mcfDimVar},
#'   \code{mcfMetVar}, \code{rtDimVar}, \code{rtMetVar}, \code{gaExpr},
#'   \code{mcfExpr}, \code{rtExpr}, \code{gaDimensions}, \code{gaMetrics},
#'   \code{mcfDimensions}, \code{mcfMetrics}, \code{rtDimensions} and
#'   \code{rtMetrics}.
#' @param ... A replacement value for \code{object} coerced to class
#'   \code{.var}.
#'
#' @return An object inheriting from the superclass \code{.var}
#'
#' @examples
#' Var("source")
#' dim <- Var("ga:medium")
#' Var(dim)
#' paid_traffic <- Expr(dim, "==", "cpc")
#' Var(paid_traffic)
#'
#' @seealso \itemize{ \item
#'   \href{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}{Core
#'    Reporting API dimensions and metrics} \item
#'   \href{https://developers.google.com/analytics/devguides/reporting/realtime/dimsmets/}{Multi-Channel-Funnel
#'    Reporting API dimensions and metrics} \item
#'   \href{https://developers.google.com/analytics/devguides/reporting/mcf/dimsmets/}{Real-Time
#'    Reporting API dimensions and metrics} }
#'
#' @family vars
#'
#' @export
#' @rdname Var
setGeneric(
  "Var",
  function(object, ...) {standardGeneric("Var")},
  valueClass = ".var"
)

#' Var<-
#'
#' \code{Var<-} sets the value of an object belonging to the superclass \code{.var}
#' or sets the var slot of an expression object belonging to superclass \code{.expr}
#'
#' @param value any object that can be coerced to a valid \code{object} class.
#'
#' @examples
#' expr1 <- Expr("pageviews", '>', 10)
#' Var(expr1) <- "uniquePageviews"
#'
#' @family vars
#'
#' @export
#' @rdname Var
setGeneric(
  "Var<-",
  function(object, value) {
    object <- standardGeneric("Var<-")
    validObject(object)
    object
  }
)

#' \code{GaVar} Gets or creates an object from the superclass \code{.gaVar}
#'
#' @family vars
#'
#' @export
#' @rdname Var
setGeneric(
  "GaVar",
  function(object, ...) {standardGeneric("GaVar")},
  valueClass = ".gaVar"
)

#' \code{GaVar<-} replaces a \code{.var} object or sets the var slot of \code{.expr}
#' object to a Google Analytics Core Reporting API dimension or metric
#' that inherits from the class \code{.gaVar}.
#'
#' @family vars
#'
#' @export
#' @rdname Var
setGeneric(
  "GaVar<-",
  function(object, value) {
    object <- standardGeneric("GaVar<-")
    validObject(object)
    object
  }
)

#' \code{McfVar} Gets or creates an object from the superclass \code{.mcfVar}
#'
#' @family vars
#'
#' @export
#' @rdname Var
setGeneric(
  "McfVar",
  function(object, ...) {standardGeneric("McfVar")},
  valueClass = ".mcfVar"
)

#' \code{McfVar<-} replaces a \code{.var} object or sets the var slot of \code{.expr}
#' object to a Multi Channel Funnel Reporting API dimension or metric
#' that inherits from the class \code{.mcfVar}.
#'
#' @family vars
#'
#' @export
#' @rdname Var
setGeneric(
  "McfVar<-",
  function(object, value) {
    object <- standardGeneric("McfVar<-")
    validObject(object)
    object
  }
)

#' \code{RtVar} gets or creates an object from the superclass \code{.rtVar}
#'
#' @family vars
#'
#' @export
#' @rdname Var
setGeneric(
  "RtVar",
  function(object, ...) {standardGeneric("RtVar")},
  valueClass = ".rtVar"
)

#' \code{RtVar<-} replaces a \code{.var} object or sets the var slot of \code{.expr}
#' object to a Google Analytics Real Time Reporting API dimension or metric
#' that inherits from the class \code{.rtVar}.
#'
#' @family vars
#'
#' @export
#' @rdname Var
setGeneric(
  "RtVar<-",
  function(object, value) {
    object <- standardGeneric("RtVar<-")
    validObject(object)
    object
  }
)

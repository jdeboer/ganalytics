#' GaView.
#'
#' Get the viewId of the query
#'
#' @param object An object to coerce to a gaView class object or to get the
#'   gaView of, such as a query, default view of a web property, or the default
#'   view of the first web property in a Google Analytics account.
#' @param value The optional replacement view if the object supplied is a query,
#'   in which case GaView will return the modified query.
#'
#' @export
#' @rdname GaView
setGeneric(
  "GaView",
  function(object, value) {},
  valueClass = c("viewId", "gaView", ".query"),
  useAsDefault = FALSE
)

#' GaView<-.
#'
#' Set the viewId for the query.
#'
#' @export
#' @rdname GaView
setGeneric(
  "GaView<-",
  function(object, value) {
    object <- standardGeneric("GaView<-")
    validObject(object)
    object
  }
)

#' MaxResults.
#'
#' Get the value set for MaxResults.
#'
#' @param object a query object.
#' @param value replacement value for the max-results parameter of the query.
#'
#' @export
#' @rdname MaxResults
setGeneric(
  "MaxResults",
  function(object, value) {},
  valueClass = "numeric",
  useAsDefault = FALSE
)

#' MaxResults<-.
#'
#' Set the maximum rows returned by a ganalytics query.
#'
#' @export
#' @rdname MaxResults
setGeneric(
  "MaxResults<-",
  function(object, value) {
    object <- standardGeneric("MaxResults<-")
    validObject(object)
    object
  }
)

#' SamplingLevel.
#'
#' Get the sampling level.
#'
#' @param object the query or response to check the sampling level of.
#' @param value if \code{object} is a query, then use  value to set the sampling
#'   level to of that query.
#'
#' @export
#' @rdname SamplingLevel
setGeneric(
  "SamplingLevel",
  function(object, value) {},
  valueClass = c("character", "list"),
  useAsDefault = FALSE
)

#' SamplingLevel<-.
#'
#' Set the sampling level for a ganalytics query.
#'
#' @export
#' @rdname SamplingLevel
setGeneric(
  "SamplingLevel<-",
  function(object, value) {
    object <- standardGeneric("SamplingLevel<-")
    validObject(object)
    object
  }
)

#' GetGaData.
#'
#' Fetch the data for the Google Analytics API query.
#'
#' @param query The query execute and returned the processed response for.
#' @param creds The OAuth2.0 credentials to use for the request.
#' @param ... Other arguments to pass on to lower-level API functions.
#'
#' @export
#' @rdname Query
setGeneric(
  "GetGaData", function(query, creds = NULL, ...) {
    standardGeneric("GetGaData")
  }
)

#' Authentication credentials for Google Analytics API queries.
#'
#' Get or set the authentication credentials for a Google Analytics query object.
#'
#' @param object The object to get the credentials from.
#' @param value The replacement credentials for the supplied query object.
#' @param ... other arguments pass to \code{GoogleApiCreds}.
#'
#' @export
#' @rdname GaCreds
setGeneric(
  "GaCreds",
  function(object = "GANALYTICS", value = NULL, ...) {
    standardGenericric("GaCreds")
  }
)

#' Set the authentication credentials for a Google Analytics query object.
#'
#' @export
#' @rdname GaCreds
setGeneric(
  "GaCreds<-",
  function(object, value) {
    object <- standardGeneric("GaCreds<-")
    validObject(object)
    object
  }
)

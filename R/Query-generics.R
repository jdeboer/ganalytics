#' @importFrom assertthat assert_that
#' @importFrom utils menu
NULL

#' MaxResults
#'
#' Get the value set for MaxResults.
#'
#' @param object A query object.
#' @param value Replacement value for the max-results parameter of the query.
#'
#' @family query object functions
#'
#' @export
#' @rdname MaxResults
setGeneric(
  "MaxResults",
  function(object, value) {standardGeneric("MaxResults")},
  valueClass = "numeric"
)

#' MaxResults<-
#'
#' Set the maximum rows returned by a ganalytics query.
#'
#' @family query object functions
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

#' SamplingLevel
#'
#' Get the sampling level.
#'
#' @param object The query or response to check the sampling level of.
#' @param value If \code{object} is a query, then use  value to set the sampling
#'   level to of that query.
#'
#' @family query object functions
#'
#' @export
#' @rdname SamplingLevel
setGeneric(
  "SamplingLevel",
  function(object, value) {standardGeneric("SamplingLevel")},
  valueClass = c("character", "list")
)

#' SamplingLevel<-
#'
#' Set the sampling level for a ganalytics query.
#'
#' @family query object functions
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

#' GetGaData
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


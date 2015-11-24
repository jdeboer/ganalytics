#' @include Query-generics.R
#' @include GaApiRequest.R
#' @include query-classes.R
#' @importFrom methods setMethod validObject
NULL

#' @describeIn GaCreds Return the query with the supplied authentication
#'   credentials supplied.
setMethod("GaCreds", signature = c(".query", "list"),
          definition = function(object, value) {
            object@creds <- value
            validObject(object)
            object
          })

#' @describeIn GaCreds Return the credentials used within the supplied query.
setMethod("GaCreds", signature = c(".query"),
          definition = function(object) {
            object@creds
          })

#' @describeIn GaCreds Create a set of authentication credentials using the
#'   supplied application name.
setMethod("GaCreds", signature = c("character"),
          definition = function(object, ...) {
            GoogleApiCreds(appname = object, ...)
          })

#' @describeIn GaCreds Return default authentication credentials.
setMethod("GaCreds", signature = c("missing"),
          definition = function(object, ...) {
            GoogleApiCreds()
          })

#' @describeIn GaCreds Replace the authentication credentials of a query.
setMethod("GaCreds<-", signature = c(".query", "list"),
          definition = function(object, value) {
            object@creds <- value
            object
          })

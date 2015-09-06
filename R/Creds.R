#' @include all-generics.R
#' @include GaApiRequest.R
#' @importFrom methods setMethod validObject
NULL

#' @describeIn GaCreds
setMethod("GaCreds", signature = c(".query", "list"),
          definition = function(object, value) {
            object@creds <- value
            validObject(object)
            object
          })

#' @describeIn GaCreds
setMethod("GaCreds", signature = c(".query"),
          definition = function(object) {
            object@creds
          })

#' @describeIn GaCreds
setMethod("GaCreds", signature = c("character"),
          definition = function(object, ...) {
            GoogleApiCreds(appname = object, ...)
          })

#' @describeIn GaCreds
setMethod("GaCreds", signature = c("missing"),
          definition = function(object, ...) {
            GoogleApiCreds()
          })

#' @describeIn GaCreds
setMethod("GaCreds<-", signature = c(".query", "list"),
          definition = function(object, value) {
            object@creds <- value
            validObject(object)
            object
          })

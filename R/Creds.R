#'@include ganalytics-package.R
#'@include all-generics.R
#'@include GaApiRequest.R
NULL

#'@export
setMethod("GaCreds", signature = c(".query", "list"),
          definition = function(object, creds) {
            object@creds <- creds
            validObject(object)
            object
          })

setMethod("GaCreds", signature = c(".query"),
          definition = function(object) {
            object@creds
          })

setMethod("GaCreds", signature = c("character"),
          definition = function(object, ...) {
            GoogleApiCreds(appname = object, ...)
          })

setMethod("GaCreds", signature = c("missing"),
          definition = function(object, ...) {
            GoogleApiCreds()
          })

setMethod("GaCreds<-", signature = c(".query", "list"),
          definition = function(object, value) {
            object@creds <- value
            validObject(object)
            object
          })

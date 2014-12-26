#' @include ga-management-classes.R

GaGetManagementData <- function(object, creds) {
  scope <- "https://www.googleapis.com/auth/analytics.readonly"
  request <- c("management", GaGetRequest(object))
  ga_api_request(creds = creds, request = request, scope = scope)
}

setMethod("GaGetRequest", "NULL", function(object) {
  character(0)
})

setMethod("GaGetRequest", "Resource", function(object) {
  c(GaGetRequest(object@belongs_to), object@request, object@id)
})

setMethod("GaGetRequest", "Collection", function(object) {
  c(GaGetRequest(object@belongs_to), object@request)
})

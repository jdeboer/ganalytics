# -- GaProfileId ----

setMethod(
  f = "GaProfileId",
  signature = "gaProfileId",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaProfileId",
  signature = "character",
  definition = function(.Object) {
    new(Class = "gaProfileId", .Object)
  }
)

setMethod(
  f = "GaProfileId",
  signature = "numeric",
  definition = function(.Object) {
    GaProfileId(
      as.character(.Object)
    )
  }
)


setMethod(
  f = "GaProfileId",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@profileId
  }
)

setMethod(
  f = "GaProfileId<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@profileId <- GaProfileId(value)
    validObject(.Object)
    return(.Object)
  }
)

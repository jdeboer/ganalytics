# -- GaQuery ----

GaQuery <- function(
  profileId,
  authFile = "defaultGA.token",
  startDate = Sys.Date() - 8,
  endDate = Sys.Date() - 1,
  metrics = "ga:visits",
  dimensions = "ga:date",
  sortBy = NULL,
  filters = NULL,
  segment = NULL,
  maxResults = kGaMaxResults
) {
  new("gaQuery",
      profileId = GaProfileId(profileId),
      dateRange = GaDateRange(
        as.Date(startDate),
        as.Date(endDate)
      ),
      metrics = GaMetrics(metrics),
      dimensions = GaDimensions(dimensions),
      sortBy = GaSortBy(sortBy),
      filters = GaFilter(filters),
      segment = GaSegment(segment),
      maxResults = maxResults,
      authFile = authFile
  )
}


setMethod(
  f = "GaMaxResults",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@maxResults
  }
)

setMethod(
  f = "GaMaxResults<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@maxResults <- as.numeric(value)
    validObject(.Object)
    return(.Object)
  }
)

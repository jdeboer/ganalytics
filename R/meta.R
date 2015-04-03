#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
#' @include GaApiRequest.R
NULL

#' GaMetaUpdate
#' Update the metadata file
#' Update the package system file containing meta data about valid dimensions and metrics, etc.
#' @return a data.frame
#' @export
GaMetaUpdate <- function() {
  scope <- "https://www.googleapis.com/auth/analytics.readonly"
  creds <- GoogleApiCreds()
  request <- c("metadata", "ga", "columns")
  meta_data <- ga_api_request(creds = creds, request = request, scope = scope)
  vars <- meta_data$items[names(meta_data$items) != "attributes"]
  attributes <- meta_data$items$attributes
  attributes <- mutate(
    attributes,
    allowedInSegments = as.logical(match(allowedInSegments, table = c("false", "true")) - 1),
    minTemplateIndex = as.numeric(minTemplateIndex),
    maxTemplateIndex = as.numeric(maxTemplateIndex),
    premiumMinTemplateIndex = as.numeric(premiumMinTemplateIndex),
    premiumMaxTemplateIndex = as.numeric(premiumMaxTemplateIndex)
  )
  df <- cbind(vars, attributes)
  kGaVars <- dlply(df, "type", function(vars) {vars$id})
  kGaVars <- rename(kGaVars, replace = c("DIMENSION" = "dims", "METRIC" = "mets"))
  kGaVars_df <- df
  metafile <- file.path(system.file(package = "ganalytics"), "R", "sysdata.rda")
  print(paste("Updating metadata file:", metafile))
  save(kGaVars, kGaVars_df, file = metafile)
  return(kGaVars)
}

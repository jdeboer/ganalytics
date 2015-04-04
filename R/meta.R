#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
#' @include GaApiRequest.R
#' @importFrom plyr mutate alply
#' @importFrom stringr str_replace
NULL

#' GaMetaUpdate
#' Update the metadata file
#' Update the package system file containing metadata about valid dimensions and metrics, etc.
#' This function should be used prior to package build.
#' @return a data.frame
GaMetaUpdate <- function(creds = GoogleApiCreds()) {
  scope <- ga_scopes['read_only']
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
  
  kGaVars_df <- mutate(
    kGaVars_df,
    lower_bounds = do.call(pmin, c(kGaVars_df[12:15], na.rm = TRUE)),
    upper_bounds = do.call(pmax, c(kGaVars_df[12:15], na.rm = TRUE))
  )
  kGaVars$allVars <- unlist(alply(kGaVars_df, 1, function(var_def){
    ret <- if(!is.na(var_def$upper_bounds)) {
      str_replace(var_def$id, "XX", seq(var_def$lower_bounds, var_def$upper_bounds))
    } else {
      var_def$id
    }
    ret
  }, .expand = FALSE), use.names = FALSE)
  
  metafile <- file.path("R", "sysdata.rda")
  message(paste("Updating metadata file:", metafile))
  save(kGaVars, kGaVars_df, file = metafile)
  return(kGaVars)
}

#' @include GaApiRequest.R
#' @importFrom plyr mutate alply dlply rbind.fill rename
#' @importFrom stringr str_replace str_trim
#' @importFrom devtools use_data
#' @importFrom rvest html html_nodes html_text
NULL

#' GaMetaUpdate
#' Update the metadata file
#' Update the package system file containing metadata about valid dimensions and metrics, etc.
#' This function should be used prior to package build.
#'
#' @return a data.frame
#' @param creds Google Analytics OAuth 2.0 credentials object.
GaMetaUpdate <- function(creds = .creds) {
  scope <- ga_scopes['read_only']
  request <- c("metadata", "ga", "columns")
  meta_data <- ga_api_request(creds = creds, request = request, scope = scope)
  vars <- meta_data$items[names(meta_data$items) != "attributes"]
  attributes <- meta_data$items$attributes
  attributes <- mutate(
    attributes,
    allowedInSegments = as.logical(match(attributes$allowedInSegments, table = c("false", "true")) - 1),
    minTemplateIndex = as.numeric(attributes$minTemplateIndex),
    maxTemplateIndex = as.numeric(attributes$maxTemplateIndex),
    premiumMinTemplateIndex = as.numeric(attributes$premiumMinTemplateIndex),
    premiumMaxTemplateIndex = as.numeric(attributes$premiumMaxTemplateIndex)
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
    ret <- if (!is.na(var_def$upper_bounds)) {
      str_replace(var_def$id, "XX", seq(var_def$lower_bounds, var_def$upper_bounds))
    } else {
      var_def$id
    }
    ret
  }, .expand = FALSE), use.names = FALSE)

  kGaVars$dims <- c(kGaVars$dims, "dateOfSession")
  kGaVars$allVars <- c(kGaVars$allVars, "dateOfSession")

  kGaVars_df$allowedInFilters <- TRUE

  kGaVars_df <- rbind.fill(kGaVars_df, data.frame(
    id = "dateOfSession",
    type = "DIMENSION",
    dataType = "STRING",
    group = "Session",
    status = "PUBLIC",
    uiName = "Date of Session",
    description = "Only for use in segments.",
    allowedInSegments = TRUE,
    allowedInFilters = FALSE
  ))

  mcf_var_ref <- "http://developers.google.com/analytics/devguides/reporting/mcf/dimsmets/"
  mcf_ref_html <- html(mcf_var_ref)
  kMcfVars <- list(
    dims = str_trim(html_text(html_nodes(mcf_ref_html, css = "div.entity.table > div.dim > div.line > a"))),
    mets = str_trim(html_text(html_nodes(mcf_ref_html, css = "div.entity.table > div.met > div.line > a")))
  )

  rt_var_ref <- "http://developers.google.com/analytics/devguides/reporting/realtime/dimsmets/"
  rt_ref_html <- html(rt_var_ref)
  kRtVars <- list(
    dims = str_trim(html_text(html_nodes(rt_ref_html, css = "div.entity.table > div.dim > div.line > a"))),
    mets = str_trim(html_text(html_nodes(rt_ref_html, css = "div.entity.table > div.met > div.line > a")))
  )

  metadata_path <- system.file("extdata", "metadata.RDA", package = "ganalytics")

  if (nchar(metadata_path) == 0) {
    package_path <- system.file(package = "ganalytics")
    extdata_path <- paste0(package_path, "/extdata")
    assert_that(dir.exists(extdata_path))
#     if (!dir.exists(extdata_path)) {
#       dir.create(extdata_path)
#     }
    metadata_path <- paste(extdata_path, "metadata.RDA", sep = "/")
  }

  prompt <- paste0(
    "Ready to update ganalytics metadata files, located in: ",
    metadata_path,
    "\nPress enter to continue..."
  )
  keypress <- readline(prompt)
  assert_that(nchar(keypress) == 0)

  save(kGaVars, kGaVars_df, kMcfVars, kRtVars, metadata_path)

}

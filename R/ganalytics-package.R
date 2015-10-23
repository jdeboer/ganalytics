#' ganalytics
#'
#' A Google Analytics and Google Tag Manager client for R
#'
#' Classes and methods for interactive use of the Google Analytics core
#' reporting, real-time, multi-channel funnel reporting, metadata, configuration
#' management, and Google Tag Manager APIs using R.
#'
#' To use this package with the Google Analytics Reporting and Google Tag
#' Manager APIs, it is neccessary to provide details for a Google APIs project
#' which has the neccessary APIs enabled.
#'
#' Features:
#'
#' * Auto-pagination to return up to 1,000,000 rows per query.
#'
#' * Ability to query more than 10 metrics at once. (TBA)
#'
#' * Minimise the effect of sampling by splitting the date range of queries
#' using the SplitDateRange function.
#'
#' * When sampling has occured, the output data.frame includes the sample and
#' total sizes as attributes.
#'
#' To report bugs enter: \code{utils::bug.report(package = "ganalytics")}
#'
#' When posting a bug, please try rerun your code wrapped within a call to
#' \code{httr::with_verbose} and include the output in the bug report (taking
#' care not to include any private data). For example:
#' httr::with_verbose(GetGaData(my_query))
#'
#' For a step-by-step guide with examples: \code{browseVignettes(package =
#' "ganalytics")}
#'
#' @references Google Analytics dimensions and metrics reference:
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}
#'
#' @references Google Analytics Table Filter expressions:
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/v3/reference#filters}
#'
#' @references Google Analytics Custom Segmentation expressions
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/v3/segments-feature-reference}
#'
#' @references Google Analytics Real-time Reporting API dimensions and metrics
#' @references Google Analytics Multi-Channel Funnel conversions Reporting API
#' @references Google Management API collections and resources
#' @references Google Tag Manager API collection and resources
#'
#' @keywords package
#' @import httpuv
#' @importFrom R6 R6Class
#' @importFrom lubridate now ymd_hms
#' @importFrom selectr querySelector querySelectorAll
#' @importFrom httr oauth_endpoints oauth1.0_token oauth2.0_token config
#'   stop_for_status content oauth_app modify_url add_headers GET POST PUT
#'   DELETE
#' @importFrom stringr str_c str_detect str_replace_all str_replace
#' @importFrom plyr aaply mutate alply l_ply ldply laply llply
#' @importFrom XML xmlParse xmlToList xmlApply
#' @importFrom jsonlite toJSON fromJSON validate
#' @importFrom methods setClass setClassUnion setValidity prototype
#' @importFrom stringr str_replace str_detect
#' @importFrom assertthat validate_that
#' @docType package
#' @name ganalytics
#' @aliases ganalytics ganalytics-package
#' @include globaldata.R
NULL



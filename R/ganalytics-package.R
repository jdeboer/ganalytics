#' ganalytics
#'
#' A Google Analytics and Google Tag Manager client for R
#'
#' Classes and methods for interacting with Google Analytics and Google Tag Manager in R.
#'
#' @references Google Analytics dimensions and metrics reference:
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}
#' @references Google Analytics Table Filter expressions
#' @references Google Analytics Custom Segmentation expressions
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
#' @docType package
#' @name ganalytics
#' @aliases ganalytics ganalytics-package
NULL

#' @include globaldata.R

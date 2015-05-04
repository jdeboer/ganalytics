#' ganalytics
#' 
#' A Google Analytics client for R
#' 
#' S4 classes and methods for interacting with Google Analytics data in R.
#' 
#' @references Google Analytics dimensions and metrics reference:
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}
#'   
#' @keywords package
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

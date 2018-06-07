#' ganalytics
#'
#' A Google Analytics and Google Tag Manager API client for R
#'
#' Classes and methods for interactive use of the Google Analytics core
#' reporting, real-time reporting, multi-channel funnel reporting, metadata,
#' configuration management and Google Tag Manager APIs using R.
#'
#' To use this package with these APIs, it is neccessary to provide OAuth
#' credentials for a Google APIs project with the neccessary APIs enabled.
#'
#' Features:
#'
#' * Auto-pagination to return up to 1,000,000 rows per query.
#'
#' * Ability to query more than 10 metrics at once. (TBC)
#'
#' * Minimise the effect of sampling by splitting the date range of queries
#' using the SplitDateRange function.
#'
#' * When sampling has occured, the output data.frame includes the sample and
#' total sizes as attributes.
#'
#' To report bugs, please run the following command:
#' \code{utils::bug.report(package = "ganalytics")}
#'
#' When posting a bug, please try rerun any API queries wrapped within a call to
#' \code{httr::with_verbose} and include the output in the bug report (taking
#' care not to include any private data). For example:
#' \code{httr::with_verbose(GetGaData(my_query))}
#'
#' For a step-by-step guide with examples, run this command:
#' \code{browseVignettes(package = "ganalytics")}
#'
#' @references Google Analytics core reporting API dimensions and metrics
#'   reference:
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}
#'
#' @references Google Analytics Table Filter expressions:
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/v3/reference#filters}
#'
#' @references Google Analytics Custom Segmentation expressions:
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/v3/segments-feature-reference}
#'
#' @references Google Analytics Real-time Reporting API dimensions and metrics:
#'   \url{https://developers.google.com/analytics/devguides/reporting/realtime/dimsmets/}
#'
#' @references Google Analytics Multi-Channel Funnel conversions Reporting API
#'   dimensions and metrics:
#'   \url{https://developers.google.com/analytics/devguides/reporting/mcf/dimsmets/}
#'
#' @references Google Analytics Management API collections and resources
#'   \url{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/}
#'
#' @references Google Tag Manager API collection and resources
#'   \url{https://developers.google.com/tag-manager/api/v1/reference/}
#'
#' @keywords package
#' @docType package
#' @name ganalytics
#' @aliases ganalytics ganalytics-package
#' @include globaldata.R
NULL



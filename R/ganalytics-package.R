#' @importFrom tibble tibble
NULL

#' ganalytics.
#'
#' A Google Analytics and Google Tag Manager API client for R
#'
#' Classes and methods for interactive use of the Google Analytics core
#' reporting, real-time reporting, multi-channel funnel reporting, metadata,
#' configuration management and Google Tag Manager APIs.
#'
#' To use this package with these APIs, it is necessary to provide OAuth
#' credentials for a Google APIs project with the necessary APIs enabled.
#'
#' Features:
#' \itemize{
#'   \item Auto-pagination to return up to 1 million rows per query.
#'   \item Ability to query more than 10 metrics at once.
#'   \item Minimise the effect of sampling by splitting the date range of
#'   queries using the \code{SplitDateRange} function.
#'   \item When sampling has occurred, the output \code{data.frame} includes
#'   the sample and total sizes as attributes.
#' }
#'
#' To report bugs, please run the following command:
#' \code{utils::bug.report(package = "ganalytics")}
#'
#' When posting a bug, try executing `traceback()` immediately after any error
#' message you receive and provide a copy of the results of the traceback in
#' your report. Also, please try rerun any API queries wrapped within a call to
#' \code{httr::with_verbose} and include the output in the bug report (taking
#' care not to include any private data). For example:
#' \code{httr::with_verbose(GetGaData(my_query))}
#'
#' For a step-by-step guide with examples, run this command:
#' \code{browseVignettes(package = "ganalytics")}
#'
#' Important functions to be familiar with in this package are:
#' \describe{
#'   \item{\code{\link{GaQuery}}}{This function is used to define a Google Analytics query
#' object that can be interactively manipulated and executed to aid exploratory
#' querying and analysis of Google Analytics data.}
#'   \item{\code{\link{GetGaData}}}{This function will execute the supplied query object.}
#'   \item{\code{\link{Expr}}}{This is used to define expressions involving Google Analytics
#' dimensions and/or metrics used for segments and filters.}
#'   \item{\code{\link{Sequence}}}{This function takes one or more expressions to construct
#' a sequence-based segment.}
#' }
#'
#' @section Package options: GOOGLE_APIS_USER GOOGLE_APIS_CONSUMER_ID
#'   GOOGLE_APIS_CONSUMER_SECRET
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
#' @keywords package internal
#' @docType package
#' @aliases ganalytics ganalytics-package
#' @include globaldata.R
"_PACKAGE"



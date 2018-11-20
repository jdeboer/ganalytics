## ----install, eval=FALSE-------------------------------------------------
#  devtools::install_github("MarkEdmondson1234/googleAnalyticsR")
#  devtools::install_github("jdeboer/ganalytics")

## ----setup---------------------------------------------------------------
library(googleAnalyticsR)
library(ganalytics)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(knitr)

ga_auth(file.path("~", "ga.oauth"))

view_id <- "117987738"
start_date <- "2018-05-01"
end_date <- "2018-06-30"

## ----define-filters------------------------------------------------------
# Device category is desktop or tablet - a dimension filter using an OR condition.
desktop_or_mobile <- Expr(~deviceCategory == "desktop") | Expr(~deviceCategory == "tablet")

# New visitors using either a desktop or tablet device - a dimension filter involving both an AND and an OR condition.
new_desktop_and_mobile_visitors <- Expr(~userType == "new") & desktop_or_mobile

# At least one goal completion or transaction - a metric filter using an OR condition.
at_least_one_conversion <- Expr(~goalCompletionsAll > 0) | Expr(~transactions > 0)

## ----pull-data, message=FALSE--------------------------------------------
results <- google_analytics(
  viewId = view_id,
  date_range = c(start_date, end_date),
  metrics = c("users", "sessions", "goalCompletionsAll", "transactions"),
  dimensions = c("deviceCategory", "userType"),
  dim_filters = new_desktop_and_mobile_visitors,
  met_filters = at_least_one_conversion
)

## ----display-pulled-data-------------------------------------------------
kable(results)


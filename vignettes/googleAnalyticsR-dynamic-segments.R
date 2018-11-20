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

## ----define-segments-----------------------------------------------------
bounces <- Expr(~bounces != 0)
conversions <- Expr(~goalCompletionsAll > 0) | Expr(~transactions > 0)
mobile_or_tablet <- Expr(~deviceCategory %in% c("mobile", "tablet"))
multi_session_users <- Include(PerUser(Expr(~sessions > 1)), scope = "users")
new_desktop_users <- Expr(~deviceCategory == "desktop") & Expr(~userType == "new")

my_segment_list <- list(
  bounced_sessions = PerSession(bounces),
  mobile_or_tablet = mobile_or_tablet,
  converters = PerUser(conversions),
  multi_session_users = multi_session_users,
  new_desktop_users = new_desktop_users,
  bounced_before_converting = Sequence(bounces, conversions, scope = "users")
)

## ----split-segment-list--------------------------------------------------
segment_chunks <- split(my_segment_list, (seq_along(my_segment_list) - 1L) %/% 4L)

## ----pull-data, message=FALSE--------------------------------------------
results <- map_df(segment_chunks, function(chunk) {
  google_analytics(
    viewId = view_id,
    date_range = c(start_date, end_date),
    metrics = c("users", "sessions"),
    dimensions = c("segment"),
    segments = Segments(chunk)
  )
})

## ----display-pulled-data-------------------------------------------------
kable(results)

## ----long-results--------------------------------------------------------
results_long <- results %>%
  gather(metric, count, users, sessions)

## ----show-long-results---------------------------------------------------
kable(results_long)

## ----visualize-results, fig.height=5, fig.width=10-----------------------
ggplot(results_long) +
  aes(segment, count, fill = metric) +
  geom_col(position = "dodge") +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  theme(axis.text.y.left = element_text(hjust = 0))


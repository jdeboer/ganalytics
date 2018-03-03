library(ganalytics)
library(googleAnalyticsR)

my_segment_list <- list(
  bounced_sessions = PerSession(Expr(~bounces != 0)),
  from_google = Expr(~source == "google"),
  repeat_user_sessions = PerUser(Expr(~sessions > 1))
)

my_segment_list <- lapply(names(my_segment_list), function(segment_name) {
  segment_def <- my_segment_list[[segment_name]]
  segment_def <- as(
    SegmentConditionFilter(segment_def),
    "segmentFilter_ga4"
  )
  segment_ga4(
    name = segment_name,
    session_segment = segment_def
  )
})

google_analytics(
  viewId = 157157785,
  date_range = c("7daysAgo", "yesterday"),
  metrics = c("users", "sessions"),
  dimensions = c("segment", "date"),
  segments = my_segment_list
)

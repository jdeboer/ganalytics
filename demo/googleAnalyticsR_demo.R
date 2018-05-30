library(ganalytics)
library(googleAnalyticsR)

my_segment_list <- list(
  bounced_sessions = PerSession(Expr(~bounces != 0)),
  from_google = Expr(~source == "google"),
  repeat_user_sessions = Include(PerUser(Expr(~sessions > 1)), scope = "users")#,
#  simple_segment = Segment(
#    Include(PerUser(Expr(~sessions > 1)), scope = "users"),
#    Include(Expr(~medium == "organic"), scope = "users")
#  )
)
my_segment_list <- lapply(names(my_segment_list), function(segment_name) {
  my_segment <- as(my_segment_list[[segment_name]], ".gaSegmentFilter")
  segment_def <- as(
    my_segment,
    "segmentDef_ga4"
  )
  switch(
    ScopeLevel(my_segment),
    sessions = segment_ga4(
      name = segment_name,
      session_segment = segment_def
    ),
    users = segment_ga4(
      name = segment_name,
      user_segment = segment_def
    )
  )
})
my_segment_list <- do.call(c, my_segment_list)

google_analytics(
  viewId = 157157785,
  date_range = c("7daysAgo", "yesterday"),
  metrics = c("users", "sessions"),
  dimensions = c("segment", "date"),
  segments = my_segment_list
)

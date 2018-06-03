library(ganalytics)
library(googleAnalyticsR)

my_segment_list <- list(
    bounced_sessions = PerSession(Expr(~bounces != 0)),
    from_google = Expr(~source == "google"),
    repeat_user_sessions = Include(PerUser(Expr(~sessions > 1)), scope = "users")
  )

my_segment_list <- as(Segments(my_segment_list), "segment_ga4")

my_dim_table_filter <- Expr(~medium == "(none)")
my_dim_table_filter <- as(TableFilter(my_dim_table_filter), ".filter_clauses_ga4")

google_analytics(
  viewId = 157157785,
  date_range = c("7daysAgo", "yesterday"),
  metrics = c("users", "sessions"),
  dimensions = c("segment", "medium"),
  segments = my_segment_list,
  dim_filters = my_dim_table_filter
)


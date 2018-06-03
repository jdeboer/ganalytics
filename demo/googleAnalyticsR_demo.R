library(ganalytics)
library(googleAnalyticsR)

my_dim_table_filter <- Expr(~medium == "organic") & Expr(~source == "google")

my_segment_list <- list(
  bounced_sessions = PerSession(Expr(~bounces != 0)),
  mobile_or_table = Expr(~deviceCategory %in% c("mobile", "tablet")),
  converters = PerUser(Expr(~goalCompletionsAll > 0) | Expr(~transactions > 0)),
  repeat_user_sessions = Include(PerUser(Expr(~sessions > 1)), scope = "users"),
  new_desktop_users = Expr(~deviceCategory == "desktop") & Expr(~userType == "new")
)

segment_chunks <- split(my_segment_list, (seq_along(my_segment_list) - 1) %/% 4)

results <- lapply(segment_chunks, function(chunk) {
  google_analytics(
    viewId = 157157785,
    date_range = c("7daysAgo", "yesterday"),
    metrics = c("users", "sessions"),
    dimensions = c("segment", "medium"),
    segments = Segments(chunk),
    dim_filters = TableFilter(my_dim_table_filter)
  )
})

results <- do.call(rbind, results)
rownames(results) <- NULL
results

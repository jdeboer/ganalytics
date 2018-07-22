library(ganalytics)
library(googleAnalyticsR)

# UI to select the appropraite Google Analytics view to get data from.
view_id <- ga_view_selector()$id

# Define a dimension table filter for use in the query later.
my_dim_table_filter <- TableFilter(
  Expr(~medium == "organic") & Expr(~source == "google")
)

# Define a list of segments to use in the query.
my_segment_list <- list(
  bounced_sessions = PerSession(Expr(~bounces != 0)),
  mobile_or_table = Expr(~deviceCategory %in% c("mobile", "tablet")),
  converters = PerUser(Expr(~goalCompletionsAll > 0) | Expr(~transactions > 0)),
  repeat_user_sessions = Include(PerUser(Expr(~sessions > 1)), scope = "users"),
  new_desktop_users = Expr(~deviceCategory == "desktop") & Expr(~userType == "new")
)

# The API can only query 4 segments at a time, so we need to break our list
# of segments into chunks. Because the segments are indexed from 1, instead of
# 0, we need to subtract 1 from the numerator in the integer division.
segment_chunks <- split(my_segment_list, (seq_along(my_segment_list) - 1L) %/% 4L)

# Query each chunk and bind the results into a single data.frame
#
# We will fetch count of users and sessions, by segment and channel group for
# the past 7 days, applying the dimension table filter defined earlier.
results <- lapply(segment_chunks, function(chunk) {
  google_analytics(
    viewId = view_id,
    date_range = c("7daysAgo", "yesterday"),
    metrics = c("users", "sessions"),
    dimensions = c("segment", "channelGrouping"),
    segments = Segments(chunk),
    dim_filters = my_dim_table_filter
  )
})

results <- do.call(rbind, results)
rownames(results) <- NULL

print(results)

library(ganalytics)

readline("press any key to continue")

# Assumes app creds can be found in environment variables (default prefix) or in a JSON file (default filename)
# Selects default view of first property in first account returned by the Management API.
myQuery <- GaQuery()
GetGaData(myQuery)

# Example 1 - Setting the date range

# Set the date range from 1 January 2013 to 31 May 2013: (Dates are specified in the format "YYYY-MM-DD".)
DateRange(myQuery) <- c("2013-01-01", "2013-05-31")

myData <- GetGaData(myQuery)
summary(myData)

# Adjust the start date to 1 March 2013:
StartDate(myQuery) <- "2013-03-01"
# Adjust the end date to 31 March 2013:
EndDate(myQuery) <- "2013-03-31"

myData <- GetGaData(myQuery)
summary(myData)

# Example 2 - Choosing what metrics to report

# Report number of page views instead
Metrics(myQuery) <- "pageviews"

myData <- GetGaData(myQuery)
summary(myData)

# Report both pageviews and sessions
Metrics(myQuery) <- c("pageviews", "sessions")
# These variations are also acceptable
Metrics(myQuery) <- c("ga:pageviews", "ga.sessions")

myData <- GetGaData(myQuery)
summary(myData)

# Example 3 - Selecting what dimensions to split your metrics by

# Similar to metrics, but for dimensions
Dimensions(myQuery) <- c("year", "week", "dayOfWeek", "hour")

# Lets set a wider date range
DateRange(myQuery) <- c("2012-10-01", "2013-03-31")

myData <- GetGaData(myQuery)
head(myData)
tail(myData)

# Example 4 - Sort by

# Sort by descending number of pageviews
SortBy(myQuery) <- "-pageviews"

myData <- GetGaData(myQuery)
head(myData)
tail(myData)

# Example 5 - Row filters

# Filter for Sunday sessions only
sundayExpr <- Expr("dayofweek", "=", "0")
TableFilter(myQuery) <- sundayExpr

myData <- GetGaData(myQuery)
head(myData)

# Remove the filter
TableFilter(myQuery) <- NULL

myData <- GetGaData(myQuery)
head(myData)

# Example 6 - Combining filters with AND

# Expression to define Sunday sessions
sundayExpr <- Expr("dayofweek", "=", "0")
# Expression to define organic search sessions
organicExpr <- Expr("medium", "=", "organic")
# Expression to define organic search sessions made on a Sunday
sundayOrganic <- sundayExpr & organicExpr
TableFilter(myQuery) <- sundayOrganic

myData <- GetGaData(myQuery)
head(myData)

# Let's concatenate medium to the dimensions for our query
Dimensions(myQuery) <- c(Dimensions(myQuery), "medium")

myData <- GetGaData(myQuery)
head(myData)

# Example 7 - Combining filters with OR

# In a similar way to AND
loyalExpr <- Expr("sessionCount", "!~", "^[0-3]$") # Made more than 3 sessions
recentExpr <- Expr("daysSinceLastSession", "~", "^[0-6]$") # Visited sometime within the past 7 days.
loyalOrRecent <- loyalExpr | recentExpr
TableFilter(myQuery) <- loyalOrRecent

myData <- GetGaData(myQuery)
summary(myData)

# Example 8 - Filters that combine ORs with ANDs

loyalExpr <- Expr("sessionCount", "!~", "^[0-3]$") # Made more than 3 sessions
recentExpr <- Expr("daysSinceLastSession", "~", "^[0-6]$") # Visited sometime within the past 7 days.
loyalOrRecent <- loyalExpr | recentExpr
sundayExpr <- Expr("dayOfWeek", "=", "0")
loyalOrRecent_Sunday <- loyalOrRecent & sundayExpr
TableFilter(myQuery) <- loyalOrRecent_Sunday

myData <- GetGaData(myQuery)
summary(myData)

# Perform the same query but change which dimensions to view
Dimensions(myQuery) <- c("sessionCount", "daysSinceLastSession", "dayOfWeek")

myData <- GetGaData(myQuery)
summary(myData)

# Example 9 - Sorting 'numeric' dimensions (continuing from example 8)

# Continuing from example 8...

# Change filter to loyal session AND recent sessions AND visited on Sunday
loyalAndRecent_Sunday <- loyalExpr & recentExpr & sundayExpr
TableFilter(myQuery) <- loyalAndRecent_Sunday

# Sort by decending visit count and ascending days since last visit.
SortBy(myQuery) <- c("-sessionCount", "+daysSinceLastSession")
myData <- GetGaData(myQuery)
head(myData)

# Notice that Google Analytics' Core Reporting API doesn't recognise 'numerical' dimensions as
# ordered factors when sorting. We can use R to sort instead, using a plyr::arrange function.
library(plyr)
myData <- arrange(myData, desc(sessionCount), daysSinceLastSession)
head(myData)
tail(myData)

# Example 10 - Session segmentation

# Visit segmentation is expressed similarly to row filters and supports AND and OR combinations.
# Define a segment for sessions where a "thank-you", "thankyou" or "success" page was viewed.
thankyouExpr <- Expr("pagePath", "~", "thank\\-?you|success")
Segment(myQuery) <- thankyouExpr

# Reset the filter
TableFilter(myQuery) <- NULL

# Split by traffic source and medium
Dimensions(myQuery) <- c("source", "medium")

# Sort by decending number of sessions
SortBy(myQuery) <- "-sessions"

myData <- GetGaData(myQuery)
head(myData)

# Example 11 - Using automatic pagination to get more than 10,000 rows of data per query

# Sessions by date and hour for the years 2011 (leap year) and 2012: 2 * 365.5 * 24 = 17544 rows
# First let's clear any filters or segments defined previously
TableFilter(myQuery) <- NULL
Segment(myQuery) <- NULL
# Define our date range
DateRange(myQuery) <- c("2011-01-01", "2012-12-31")
# Define our metrics and dimensions
Metrics(myQuery) <- "sessions"
Dimensions(myQuery) <- c("date", "dayOfWeek", "hour")
# Let's allow a maximum of 17544 rows (default is 10000)
MaxResults(myQuery) <- 17544

myData <- GetGaData(myQuery)
nrow(myData)

# Let's use plyr::ddply to analyse the data
library(plyr)

# Sessions by day of week
sessions_by_dayOfWeek <- ddply(myData, ~dayOfWeek, summarise, sessions = sum(sessions))
with(sessions_by_dayOfWeek, barplot(sessions, names.arg = dayOfWeek))

# Sessions by hour of day
sessions_by_hour <- ddply(myData, ~hour, summarise, sessions = sum(sessions))
with(sessions_by_hour, barplot(sessions, names.arg = hour))

# Example 12 - Using ggplot2

library(ggplot2)
library(plyr)

# Sessions by date and hour for the years 2011 (leap year) and 2012: 2 * 365.5 * 24 = 17544 rows
# First let's clear any filters or segments defined previously
TableFilter(myQuery) <- NULL
Segment(myQuery) <- NULL
# Define our date range
DateRange(myQuery) <- c("2011-01-01", "2012-12-31")
# Define our metrics and dimensions
Metrics(myQuery) <- "sessions"
Dimensions(myQuery) <- c("date", "dayOfWeek", "hour", "isMobile")
# Let's allow a maximum of 40000 rows (default is 10000)
MaxResults(myQuery) <- 40000

myData <- GetGaData(myQuery)

# Sessions by hour of day and day of week
avg_sessions_by_hour_wday_mobile <- ddply(
  myData,
  ~hour + dayOfWeek + isMobile,
  summarise,
  sessions = mean(sessions)
)

# Relabel the days of week
levels(avg_sessions_by_hour_wday_mobile$dayOfWeek) <- c(
  "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
)

# Plot the summary data
qplot(
  x = hour,
  y = sessions,
  data = avg_sessions_by_hour_wday_mobile,
  facets = ~dayOfWeek,
  fill = isMobile,
  geom = "bar",
  position = "stack",
  stat = "identity"
)

# Real-time reporting API

library(ganalytics)
my_creds = GoogleApiCreds(userName = "my_username@gmail.com", appCreds = "~/oauth_app_creds.json")
rt_query <- RtQuery(view = "ga:987654321", creds = my_creds)
Dimensions(rt_query) <- "rt:minutesAgo"
Metrics(rt_query) <- "rt:pageviews"
GetGaData(rt_query)

# In the above example, set userName to yours that you use to access Google
# Analytics (this is optional, but ensures you are authenticating under the
# correct Google account), and set the appCreds to the path of where you have
# saved your Google APIs Project OAuth application credentials JSON file (which
# you can download from the Google APIs Console). Also set view to the ID of the
# Google Analytics view of which you wish to get real-time reporting data for.

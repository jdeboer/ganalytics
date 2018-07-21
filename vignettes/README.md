---
title: "Interactively querying Google Analytics reports"
author: "Johann de Boer"
date: "2018-06-07"
output:
  html_vignette:
    keep_md: yes
  md_document:
    variant:
      markdown_github
vignette: >
  %\VignetteIndexEntry{README}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

<!-- README.md is generated from README.rmd. Please edit that file -->



<!--html_preserve--><img alt="logo" style="position:absolute; top:0; right:0; padding:10px;">data:image/png;base64,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</img><!--/html_preserve-->

ganalytics
==========
[![Travis-CI Build Status](https://travis-ci.org/jdeboer/ganalytics.png?branch=master)](https://travis-ci.org/jdeboer/ganalytics)

Classes and methods for interactive use of the Google Analytics core reporting, real-time reporting, multi-channel funnel reporting, metadata, configuration management and Google Tag Manager APIs.

The aim of this package is to support R users in defining reporting queries using natural R expressions instead of being concerned about API technical intricacies like query syntax, character code escaping and API limitations.

This package provides functions for querying the Google Analytics core reporting, real-time reporting, multi-channel funnel reporting and management APIs, as well as the Google Tag Manager API. Write methods are also provided for the Google Analytics Management and Google Tag Manager APIs so that you can, for example, change tag, property or view settings.

Updates
-------
Support for GoogleAnalyticsR integration is now available for segments and table filter objects. You can supply these objects to the `google_analytics` function in GoogleAnalyticsR by using `as()`, supplying the appropriate GoogleAnalyticsR class names, which are `"segment_ga4"` for segments and `".filter_clauses_ga4"` for table filters. Soon GoogleanalyticsR will implictly coerce ganalytics segments and table filters so that you do not need to explictly coerce using `as()`.

Many new functions have been provided for writing segmentation expressions:

* `Segments(...)` - define a list of segments dynamically based on one or more expressions and/or a selection of built-in and/or custom segments by their IDs.
* `Include(...)` - expressions (conditions or sequences) defining users or sessions to include in the segment
* `Exclude(...)` - expressions (conditions or sequences) defining users or sessions to exclude from the segment
* `PerUser(...)` - set the scope of one or more segment conditions or sequences to user-level, or set the scope of a metric condition to user-level.
* `PerSession(...)` - set the scope of one or more segment conditions or sequences to user-level, or set the scope of a metric condition to session-level.
* `PerHit(...)` - specify that a set of logically combined conditions must all be met for a single hit, or set the scope of a metric condition to hit-level.
* `Sequence(...)` - define a sequence of one or more conditions to use in a dynamic segment definition.
* `Then(condition)` - used within a `Sequence()` to specify that this condition must immediately follow the preceding condition, as opposed to the default of loosely following at some point later.
* `Later(condition)` - similar to `Then()` but means that a condition can happen any point after the preceding condition - this is how conditions are treated by default in a sequence if not explicitly set.
* `First(condition)` - similar to `Then()` but means that a condition must be the first interaction (hit) by the user within the specified date-range. Using `First()` is optional. Without using `First()` at the start of a sequence, then the first condition does not need to match the first interaction by the user. It does not make sense to use `First()` anywhere else in the sequence other than at the start, if used at all.

Multi-channel funnel (MCF) and real-time (RT) queries can now be constructed, but work is still needed to process the response from these queries - stay tuned for updates on this.

Instead of using `Or`, `And`, and `Not`, it is now possible to use familiar R language Boolean operators, `|` (`Or`), `&` (`And`), and `!` (`Not`) instead (thanks to @hadley for suggestion [#2](https://github.com/jdeboer/ganalytics/issues/2)). It is important to keep in mind however that Google Analytics requires `Or` to have precedence over `And`, which is the opposite to the natural precedence given by R when using the `|` and `&` operators. Therefore, remember to use parentheses `(` `)` to enforce the correct order of operation to your Boolean expressions. For example `my_filter <- !bounced & (completed_goal | transacted)` is a valid structure for a Google Analytics reporting API filter expression.

You can use query the Google Analytics Management API to obtain details in R about the configuration of your accounts, properties and views, such as goals you have defined. There are *write* methods available too, but these have not been fully tested so use with extreme care. If you wish to use these functions, it is recommended that you test these using test login, otherwise avoid using the "INSERT", "UPDATE" and "DELETE" methods.

There is also some basic support for the Google Tag Manager API, but again, this is a work in progress so take care with the write methods above.

Installation
------------
### 1. Install the necessary packages into R via the GitHub repository

#### Prerequisites
* Ensure you have installed the latest version of [R](https://cran.r-project.org/)

#### Execute the following statements in R to install ganalytics:

```r
# Install the latest version of remotes via CRAN
install.packages("remotes")
# Install ganalytics via the GitHub repository.
remotes::install_github("jdeboer/ganalytics")
# End
```

### 2. Prepare your Google API application _(you only need to do this once)_
* Browse to [**Google API Console**] (https://code.google.com/apis/console/)
* Check you are **signed into Google** with the account you wish to use.
* Choose **Create Project** from the Google API Console and give your project a name (or choose an existing project if you have one already).
* From the **APIs** page, enable the **Analytics API**. You may also want to enable the **Tag Manager API** if you wish to try that.
* You will need to **agree** and **accept** the Google APIs and Analytics API Terms of Service to proceed.
* Go to the **Credentials** page, click **Add credentials**, choose **OAuth 2.0 client ID**, then select "Other".
* Note your **Client ID** and **Client Secret** and download the JSON file to your R working directory.

_Note: For further information about Google APIs, please refer to the [References section](https://github.com/jdeboer/ganalytics/blob/master/README.md#useful-references) at the end of this document._

### 3. Set your system environment variables _(this is optional but recommended)_
* Add the following two user variables:

  |     | Variable name                 | Variable value         |
  | --- | ----------------------------- | ---------------------- |
  |   1 | `GOOGLE_APIS_CONSUMER_ID`     | `<Your client ID>`     |
  |   2 | `GOOGLE_APIS_CONSUMER_SECRET` | `<Your client secret>` |

  * To do this in Windows:
    * Search for and select **"Edit Environment Variables For Your Account"** from the Start menu.
    * Within the **Environment Variables** window, add the above **User Variables** by selecting **New** and entering the **Variable Name** and **Variable Value**, then click **OK**. Do this for both variables listed in the table above.
    * Click **OK**.
    * **Restart** your computer for the new environment variables to take effect.
  * There is also a free open source utility to set environment variables on Mac OS called [EnvPane](https://github.com/hschmidt/EnvPane)
  * Another method that works across platforms is to create an `.Renviron` file within your active R working directory that is structured like this:
  
```
GOOGLE_APIS_CONSUMER_ID = <Your client ID>
GOOGLE_APIS_CONSUMER_SECRET = <Your client secret>
```

**Alternatively** you can temporarily set your environment variables straight from R using this command:

```r
Sys.setenv(
  GOOGLE_APIS_CONSUMER_ID = "<Your client ID>",
  GOOGLE_APIS_CONSUMER_SECRET = "<Your client secret>"
)
```

  _Note: For other operating systems please refer to the Reference section at the end of this document._

### 4. Authenticate and attempt your first query with ganalytics
* ganalytics needs to know the ID of the Google Analytics **view** that you wish to query. You can obtain this in a number of ways:
  * Using the [Google Analytics Query Explorer tool](https://ga-dev-tools.appspot.com/explorer/)
  * From the **Admin page** in Google Analytics under **View Settings**, or
  * The browser's address bar while viewing a report in Google Analytics - look for the digits between the letter **'p'** and trailing **'/'**, e.g. `.../a11111111w22222222p33333333/` shows a view ID of `33333333`.

* **Alternatively, ganalytics can look up the view ID for you:**
  * If you have access to only one Google Analytics account, with one property, then ganalytics will automatically select the default view for you from that property.
  * Otherwise it will select the default view of the first property from the first account that it finds in the list of accounts that you have access to.

* Return to R and execute the following to load the ganalytics package:

  ```r
  library(ganalytics)
  ```

* If you have successfully set your system environment variables in step 3 above, then you can execute the following, optionally providing the email address you use to sign-in to Google Analytics:

  ```r
  my_creds <- GoogleApiCreds("you@domain.com")
  ```

* Otherwise do one of the following:
  * If you downloaded the JSON file containing your Google API app credentials, then provide the file path:

    ```r
    my_creds <- GoogleApiCreds("you@domain.com", "client_secret.json")
    ```
  * Or, instead of a file you can supply the `client_id` and `client_secret` directly:

    ```r
    my_creds <- GoogleApiCreds(
      "you@domain.com",
      list(client_id = "<client id>", client_secret = "<client secret>")
    )
    ```

* Now formulate and run your Google Analytics query, remembering to substitute `view_id` with the view ID you wish to use:

  ```r
  myQuery <- GaQuery( view_id, creds = my_creds ) # view_id is optional
  GetGaData(myQuery)
  ```

* You should then be directed to *accounts.google.com* within your default web browser asking you to sign-in to your Google account if you are not already. Once signed-in you will be asked to grant read-only access to your Google Analytics account for the Google API project you created in step 1.
* Make sure you are signed into the Google account you wish to use, then grant access by selecting **"Allow access"**. You can then close the page and return back to R.

If you have successfully executed all of the above R commands you should see the output of the default ganalytics query; sessions by day for the past 7 days. For example:

```
        date sessions
1 2015-03-27     2988
2 2015-03-28     1594
3 2015-03-29     1912
4 2015-03-30     3061
5 2015-03-31     2609
6 2015-04-01     2762
7 2015-04-02     2179
8 2015-04-03     1552
```

_Note: A small file will be saved to your home directory ('My Documents' in Windows) to cache your new reusable authentication token._

Examples
--------

As demonstrated in the installation steps above, before executing any of the following examples:

1. Load the ganalytics package
2. Generate a `gaQuery` object using the `GaQuery()` function and assigning the object to a variable name such as `myQuery`.

### Asumptions

**The following examples assume you have successfully completed the above steps and have named your Google Analytics query object: `myQuery`.**

### Example 1 - Setting the date range

```r
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
# End
```

### Example 2 - Choosing what metrics to report

```r
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
# End
```

### Example 3 - Selecting what dimensions to split your metrics by

```r
# Similar to metrics, but for dimensions
Dimensions(myQuery) <- c("year", "week", "dayOfWeekName", "hour")

# Lets set a wider date range
DateRange(myQuery) <- c("2012-10-01", "2013-03-31")

myData <- GetGaData(myQuery)
head(myData)
tail(myData)
# End
```

### Example 4 - Sort by

```r
# Sort by descending number of pageviews
SortBy(myQuery) <- "-pageviews"

myData <- GetGaData(myQuery)
head(myData)
tail(myData)
# End
```

### Example 5 - Row filters

```r
# Filter for Sunday sessions only
sundayExpr <- Expr(~dayOfWeekName == "Sunday")
TableFilter(myQuery) <- sundayExpr

myData <- GetGaData(myQuery)
head(myData)

# Remove the filter
TableFilter(myQuery) <- NULL

myData <- GetGaData(myQuery)
head(myData)
# End
```

### Example 6 - Combining filters with AND

```r
# Expression to define Sunday sessions
sundayExpr <- Expr(~dayOfWeekName == "Sunday")
# Expression to define organic search sessions
organicExpr <- Expr(~medium == "organic")
# Expression to define organic search sessions made on a Sunday
sundayOrganic <- sundayExpr & organicExpr
TableFilter(myQuery) <- sundayOrganic

myData <- GetGaData(myQuery)
head(myData)

# Let's concatenate medium to the dimensions for our query
Dimensions(myQuery) <- c(Dimensions(myQuery), "medium")

myData <- GetGaData(myQuery)
head(myData)
# End
```

### Example 7 - Combining filters with OR

```r
# In a similar way to AND
loyalExpr <- !Expr(~sessionCount %matches% "^[0-3]$") # Made more than 3 sessions
recentExpr <- Expr(~daysSinceLastSession %matches% "^[0-6]$") # Visited sometime within the past 7 days.
loyalOrRecent <- loyalExpr | recentExpr
TableFilter(myQuery) <- loyalOrRecent

myData <- GetGaData(myQuery)
summary(myData)
# End
```

### Example 8 - Filters that combine ORs with ANDs

```r
loyalExpr <- !Expr(~sessionCount %matches% "^[0-3]$") # Made more than 3 sessions
recentExpr <- Expr(~daysSinceLastSession %matches% "^[0-6]$") # Visited sometime within the past 7 days.
loyalOrRecent <- loyalExpr | recentExpr
sundayExpr <- Expr(~dayOfWeekName == "Sunday")
loyalOrRecent_Sunday <- loyalOrRecent & sundayExpr
TableFilter(myQuery) <- loyalOrRecent_Sunday

myData <- GetGaData(myQuery)
summary(myData)

# Perform the same query but change which dimensions to view
Dimensions(myQuery) <- c("sessionCount", "daysSinceLastSession", "dayOfWeek")

myData <- GetGaData(myQuery)
summary(myData)
# End
```

### Example 9 - Sorting 'numeric' dimensions (continuing from example 8)

```r
# Continuing from example 8...

# Change filter to loyal session AND recent sessions AND visited on Sunday
loyalAndRecent_Sunday <- loyalExpr & recentExpr & sundayExpr
TableFilter(myQuery) <- loyalAndRecent_Sunday

# Sort by decending visit count and ascending days since last visit.
SortBy(myQuery) <- c("-sessionCount", "+daysSinceLastSession")
myData <- GetGaData(myQuery)
head(myData)

# Notice that the Google Analytics Core Reporting API doesn't recognise 'numerical' dimensions as
# ordered factors when sorting. We can use R to sort instead, such as using dplyr.
library(dplyr)
myData <- myData %>% arrange(desc(sessionCount), daysSinceLastSession)
head(myData)
tail(myData)
# End
```

### Example 10 - Session segmentation

```r
# Visit segmentation is expressed similarly to row filters and supports AND and OR combinations.
# Define a segment for sessions where a "thank-you", "thankyou" or "success" page was viewed.
thankyouExpr <- Expr(~pagePath %matches% "thank\\-?you|success")
Segments(myQuery) <- thankyouExpr

# Reset the filter
TableFilter(myQuery) <- NULL

# Split by traffic source and medium
Dimensions(myQuery) <- c("source", "medium")

# Sort by decending number of sessions
SortBy(myQuery) <- "-sessions"

myData <- GetGaData(myQuery)
head(myData)
# End
```

### Example 11 - Using automatic pagination to get more than 10,000 rows of data per query

```r
# Sessions by date and hour for the years 2016 and 2017:
# First let's clear any filters or segments defined previously
TableFilter(myQuery) <- NULL
Segments(myQuery) <- NULL
# Define our date range
DateRange(myQuery) <- c("2016-01-01", "2017-12-31")
# Define our metrics and dimensions
Metrics(myQuery) <- "sessions"
Dimensions(myQuery) <- c("date", "dayOfWeekName", "hour")
# Let's allow a maximum of 20000 rows (default is 10000)
MaxResults(myQuery) <- 20000

myData <- GetGaData(myQuery)
nrow(myData)

## Let's use dplyr to analyse the data
library(dplyr)

# Sessions by day of week
sessions_by_dayOfWeek <- myData %>%
  count(dayOfWeekName, wt = sessions) %>% 
  mutate(dayOfWeekName = factor(dayOfWeekName, levels = c(
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
  ), labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ordered = TRUE)) %>% 
  arrange(dayOfWeekName)
with(
  sessions_by_dayOfWeek,
  barplot(n, names.arg = dayOfWeekName, xlab = "day of week", ylab = "sessions")
)

# Sessions by hour of day
sessions_by_hour <- myData %>%
  count(hour, wt = sessions)
with(
  sessions_by_hour,
  barplot(n, names.arg = hour, xlab = "hour", ylab = "sessions")
)
# End
```

### Example 12 - Using ggplot2
To run this example first install ggplot2 if you haven't already.

```r
install.packages("ggplot2")
```

Once installed, then run the following example.

```r
library(ggplot2)
library(dplyr)

# Sessions by date and hour for the years 2016 and 2017:
# First let's clear any filters or segments defined previously
TableFilter(myQuery) <- NULL
Segments(myQuery) <- NULL
# Define our date range
DateRange(myQuery) <- c("2016-01-01", "2017-12-31")
# Define our metrics and dimensions
Metrics(myQuery) <- "sessions"
Dimensions(myQuery) <- c("date", "dayOfWeek", "hour", "deviceCategory")
# Let's allow a maximum of 40000 rows (default is 10000)
MaxResults(myQuery) <- 40000

myData <- GetGaData(myQuery)

# Sessions by hour of day and day of week
avg_sessions_by_hour_wday_device <- myData %>% 
  group_by(hour, dayOfWeek, deviceCategory) %>% 
  summarise(sessions = mean(sessions)) %>% 
  ungroup()

# Relabel the days of week
levels(avg_sessions_by_hour_wday_device$dayOfWeek) <- c(
  "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
)

# Plot the summary data
qplot(
  x = hour,
  y = sessions,
  data = avg_sessions_by_hour_wday_device,
  facets = ~dayOfWeek,
  fill = deviceCategory,
  geom = "col"
)

# End
```

Thanks to:
----------
* Hadley Wickham @hadley
* Mark Edmondson @MarkEdmondson1234
* RStudio team
* R Core team

Useful references
-----------------

1. [Google Analytics Core Reporting API reference guide](https://developers.google.com/analytics/devguides/reporting/core/v3/reference)
2. [Google Analytics Dimensions and Metrics reference](https://developers.google.com/analytics/devguides/reporting/core/dimsmets)
3. [Creating a Google API project](https://developers.google.com/console/help/#creatingdeletingprojects)
4. [Generating an OAuth 2.0 client ID for Google APIs](https://developers.google.com/console/help/#generatingoauth2)
5. [Using OAuth 2.0 for Installed Applications](https://developers.google.com/accounts/docs/OAuth2InstalledApp)
7. [EnvPane utility for setting environment variables in OSX](https://github.com/hschmidt/EnvPane)
8. [Setting environment variables in Ubuntu Linux](https://help.ubuntu.com/community/EnvironmentVariables)

Notes
-----
Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Google Analytics and Google Tag Manager are trademarks of Google.

<!-- README.md is generated from README.rmd. Please edit that file -->
![logo](https://raw.githubusercontent.com/jdeboer/ganalytics/dev/inst/figures/hexicon-small.png)

ganalytics
==========

[![Travis-CI Build
Status](https://travis-ci.org/jdeboer/ganalytics.png?branch=master)](https://travis-ci.org/jdeboer/ganalytics)

[![Rdoc](http://www.rdocumentation.org/badges/version/ganalytics)](http://www.rdocumentation.org/packages/ganalytics)

Classes and methods for interactive use of the Google Analytics core
reporting, real-time reporting, multi-channel funnel reporting,
metadata, configuration management and Google Tag Manager APIs.

The aim of this package is to support R users in defining reporting
queries using natural R expressions instead of being concerned about API
technical intricacies like query syntax, character code escaping and API
limitations.

This package provides functions for querying the Google Analytics core
reporting, real-time reporting, multi-channel funnel reporting and
management APIs, as well as the Google Tag Manager API. Write methods
are also provided for the Google Analytics Management and Google Tag
Manager APIs so that you can, for example, change tag, property or view
settings.

Updates
-------

Support for GoogleAnalyticsR integration is now available for segments
and table filter objects. You can supply these objects to the
`google_analytics` function in GoogleAnalyticsR by using `as()`,
supplying the appropriate GoogleAnalyticsR class names, which are
`"segment_ga4"` for segments and `".filter_clauses_ga4"` for table
filters. Soon GoogleanalyticsR will implicitly coerce ganalytics
segments and table filters so that you do not need to explicitly coerce
using `as()`.

Many new functions have been provided for writing segmentation
expressions:

-   `Segments(...)` - define a list of segments dynamically based on one
    or more expressions and/or a selection of built-in and/or custom
    segments by their IDs.
-   `Include(...)` - expressions (conditions or sequences) defining
    users or sessions to include in the segment
-   `Exclude(...)` - expressions (conditions or sequences) defining
    users or sessions to exclude from the segment
-   `PerUser(...)` - set the scope of one or more segment conditions or
    sequences to user-level, or set the scope of a metric condition to
    user-level.
-   `PerSession(...)` - set the scope of one or more segment conditions
    or sequences to user-level, or set the scope of a metric condition
    to session-level.
-   `PerHit(...)` - specify that a set of logically combined conditions
    must all be met for a single hit, or set the scope of a metric
    condition to hit-level.
-   `Sequence(...)` - define a sequence of one or more conditions to use
    in a dynamic segment definition.
-   `Then(condition)` - used within a `Sequence()` to specify that this
    condition must immediately follow the preceding condition, as
    opposed to the default of loosely following at some point later.
-   `Later(condition)` - similar to `Then()` but means that a condition
    can happen any point after the preceding condition - this is how
    conditions are treated by default in a sequence if not explicitly
    set.
-   `First(condition)` - similar to `Then()` but means that a condition
    must be the first interaction (hit) by the user within the specified
    date-range. Using `First()` is optional. Without using `First()` at
    the start of a sequence, then the first condition does not need to
    match the first interaction by the user. It does not make sense to
    use `First()` anywhere else in the sequence other than at the start,
    if used at all.

Multi-channel funnel (MCF) and real-time (RT) queries can now be
constructed, but work is still needed to process the response from these
queries - stay tuned for updates on this.

Instead of using `Or`, `And`, and `Not`, it is now possible to use
familiar R language Boolean operators, `|` (`Or`), `&` (`And`), and `!`
(`Not`) instead (thanks to @hadley for suggestion
[\#2](https://github.com/jdeboer/ganalytics/issues/2)). It is important
to keep in mind however that Google Analytics requires `Or` to have
precedence over `And`, which is the opposite to the natural precedence
given by R when using the `|` and `&` operators. Therefore, remember to
use parentheses `(` `)` to enforce the correct order of operation to
your Boolean expressions. For example
`my_filter <- !bounced & (completed_goal | transacted)` is a valid
structure for a Google Analytics reporting API filter expression.

You can use query the Google Analytics Management API to obtain details
in R about the configuration of your accounts, properties and views,
such as goals you have defined. There are *write* methods available too,
but these have not been fully tested so use with extreme care. If you
wish to use these functions, it is recommended that you test these using
test login, otherwise avoid using the “INSERT”, “UPDATE” and “DELETE”
methods.

There is also some basic support for the Google Tag Manager API, but
again, this is a work in progress so take care with the write methods
above.

Installation
------------

### 1. Install the necessary packages into R via the GitHub repository

#### Prerequisites

-   Ensure you have installed the latest version of
    [R](https://cran.r-project.org/)

#### Current stable release from CRAN

You can install the released version of ganalytics from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ganalytics")
```

#### Current development release from GitHub

Alternatively, you can execute the following statements in R to install
the current stable development version of ganalytics from GitHub:

``` r
# Install the latest version of remotes via CRAN
install.packages("remotes")
# Install ganalytics via the GitHub repository.
remotes::install_github("jdeboer/ganalytics")
# End
```

### 2. Prepare your Google API application *(you only need to do this once)*

-   Browse to \[**Google API Console**\]
    (<https://code.google.com/apis/console/>)
-   Check you are **signed into Google** with the account you wish to
    use.
-   Choose **Create Project** from the Google API Console and give your
    project a name (or choose an existing project if you have one
    already).
-   From the **APIs** page, enable the **Analytics API**. You may also
    want to enable the **Tag Manager API** if you wish to try that.
-   You will need to **agree** and **accept** the Google APIs and
    Analytics API Terms of Service to proceed.
-   Go to the **Credentials** page, click **Add credentials**, choose
    **OAuth 2.0 client ID**, then select “Other”.
-   Note your **Client ID** and **Client Secret** and download the JSON
    file to your R working directory.

*Note: For further information about Google APIs, please refer to the
[References
section](https://github.com/jdeboer/ganalytics/blob/master/README.md#useful-references)
at the end of this document.*

### 3. Set your system environment variables *(this is optional but recommended)*

-   Add the following two user variables:

    |     | Variable name                 | Variable value         |
    |-----|-------------------------------|------------------------|
    | 1   | `GOOGLE_APIS_CONSUMER_ID`     | `<Your client ID>`     |
    | 2   | `GOOGLE_APIS_CONSUMER_SECRET` | `<Your client secret>` |

    -   To do this in Windows:
        -   Search for and select **“Edit Environment Variables For Your
            Account”** from the Start menu.
        -   Within the **Environment Variables** window, add the above
            **User Variables** by selecting **New** and entering the
            **Variable Name** and **Variable Value**, then click **OK**.
            Do this for both variables listed in the table above.
        -   Click **OK**.
        -   **Restart** your computer for the new environment variables
            to take effect.
    -   There is also a free open source utility to set environment
        variables on Mac OS called
        [EnvPane](https://github.com/hschmidt/EnvPane)
    -   Another method that works across platforms is to create an
        `.Renviron` file within your active R working directory that is
        structured like this:

<!-- -->

    GOOGLE_APIS_CONSUMER_ID = <Your client ID>
    GOOGLE_APIS_CONSUMER_SECRET = <Your client secret>

**Alternatively** you can temporarily set your environment variables
straight from R using this command:

``` r
Sys.setenv(
  GOOGLE_APIS_CONSUMER_ID = "<Your client ID>",
  GOOGLE_APIS_CONSUMER_SECRET = "<Your client secret>"
)
```

*Note: For other operating systems please refer to the Reference section
at the end of this document.*

### 4. Authenticate and attempt your first query with ganalytics

-   ganalytics needs to know the ID of the Google Analytics **view**
    that you wish to query. You can obtain this in a number of ways:
    -   Using the [Google Analytics Query Explorer
        tool](https://ga-dev-tools.appspot.com/explorer/)
    -   From the **Admin page** in Google Analytics under **View
        Settings**, or
    -   The browser’s address bar while viewing a report in Google
        Analytics - look for the digits between the letter **‘p’** and
        trailing **‘/’**, e.g. `.../a11111111w22222222p33333333/` shows
        a view ID of `33333333`.
-   **Alternatively, ganalytics can look up the view ID for you:**
    -   If you have access to only one Google Analytics account, with
        one property, then ganalytics will automatically select the
        default view for you from that property.
    -   Otherwise it will select the default view of the first property
        from the first account that it finds in the list of accounts
        that you have access to.
-   Return to R and execute the following to load the ganalytics
    package:

    ``` r
    library(ganalytics)
    ```

-   If you have successfully set your system environment variables in
    step 3 above, then you can execute the following, optionally
    providing the email address you use to sign-in to Google Analytics:

    ``` r
    my_creds <- GoogleApiCreds("you@domain.com")
    ```

-   Otherwise do one of the following:
    -   If you downloaded the JSON file containing your Google API app
        credentials, then provide the file path:

        ``` r
        my_creds <- GoogleApiCreds("you@domain.com", "client_secret.json")
        ```

    -   Or, instead of a file you can supply the `client_id` and
        `client_secret` directly:

        ``` r
        my_creds <- GoogleApiCreds(
          "you@domain.com",
          list(client_id = "<client id>", client_secret = "<client secret>")
        )
        ```

-   Now formulate and run your Google Analytics query, remembering to
    substitute `view_id` with the view ID you wish to use:

    ``` r
    myQuery <- GaQuery( view_id, creds = my_creds ) # view_id is optional
    GetGaData(myQuery)
    ```

-   You should then be directed to *accounts.google.com* within your
    default web browser asking you to sign-in to your Google account if
    you are not already. Once signed-in you will be asked to grant
    read-only access to your Google Analytics account for the Google API
    project you created in step 1.
-   Make sure you are signed into the Google account you wish to use,
    then grant access by selecting **“Allow access”**. You can then
    close the page and return back to R.

If you have successfully executed all of the above R commands you should
see the output of the default ganalytics query; sessions by day for the
past 7 days. For example:

            date sessions
    1 2015-03-27     2988
    2 2015-03-28     1594
    3 2015-03-29     1912
    4 2015-03-30     3061
    5 2015-03-31     2609
    6 2015-04-01     2762
    7 2015-04-02     2179
    8 2015-04-03     1552

*Note: A small file will be saved to your home directory (‘My Documents’
in Windows) to cache your new reusable authentication token.*

Examples
--------

As demonstrated in the installation steps above, before executing any of
the following examples:

1.  Load the ganalytics package
2.  Generate a `gaQuery` object using the `GaQuery()` function and
    assigning the object to a variable name such as `myQuery`.

### Assumptions

**The following examples assume you have successfully completed the
above steps and have named your Google Analytics query object:
`myQuery`.**

### Example 1 - Setting the date range

``` r
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

``` r
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

``` r
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

``` r
# Sort by descending number of pageviews
SortBy(myQuery) <- "-pageviews"

myData <- GetGaData(myQuery)
head(myData)
tail(myData)
# End
```

### Example 5 - Row filters

``` r
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

``` r
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

``` r
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

``` r
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

### Example 9 - Sorting ‘numeric’ dimensions (continuing from example 8)

``` r
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

``` r
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

``` r
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

To run this example first install ggplot2 if you haven’t already.

``` r
install.packages("ggplot2")
```

Once installed, then run the following example.

``` r
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

-   Hadley Wickham @hadley
-   Mark Edmondson @MarkEdmondson1234
-   RStudio team
-   R Core team

Useful references
-----------------

1.  [Google Analytics Core Reporting API reference
    guide](https://developers.google.com/analytics/devguides/reporting/core/v3/reference)
2.  [Google Analytics Dimensions and Metrics
    reference](https://developers.google.com/analytics/devguides/reporting/core/dimsmets)
3.  [Creating a Google API
    project](https://developers.google.com/console/help/#creatingdeletingprojects)
4.  [Generating an OAuth 2.0 client ID for Google
    APIs](https://developers.google.com/console/help/#generatingoauth2)
5.  [Using OAuth 2.0 for Installed
    Applications](https://developers.google.com/accounts/docs/OAuth2InstalledApp)
6.  [EnvPane utility for setting environment variables in
    OSX](https://github.com/hschmidt/EnvPane)
7.  [Setting environment variables in Ubuntu
    Linux](https://help.ubuntu.com/community/EnvironmentVariables)

Notes
-----

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.

Google Analytics and Google Tag Manager are trademarks of Google.

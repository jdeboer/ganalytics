ganalytics
==========

Interact with **Google Analytics** using **R**

S4 classes and methods for interactive use of the Google Analytics core reporting API using R.


Installation
------------
### Prerequisites
1. Ensure you have installed the latest version of [R](http://cran.r-project.org/)
2. If using Windows, also install the latest version of [RTools](http://cran.r-project.org/bin/windows/Rtools/)

### 1. Install the necessary packages into R
  * Execute the following statements in R:

```r
install.packages("devtools") # Installs the latest version of devtools available from CRAN
library(devtools)
install_github(repo = "httr", username = "jdeboer") # a fork of httr.
install_github(repo = "ganalytics", username = "jdeboer") # installs ganalytics from the GitHub repository.
```
### 2. Prepare your Google API application
  * Browse to [**Google API Console**] (https://code.google.com/apis/console/)
  * Check you are **signed in to Google** with the account you wish to use.
  * Choose **"Create project..."** from the Google API Console (or choose an existing project if you have one already.)
  * From the Services page, set the status of **Analytics API** to **ON**.
  * From the API Access page, click the blue button to **"Create an OAuth 2.0 client ID"**.
  * Complete the form; **Product Name** is required - give it a unique name, e.g. "ganalytics for R", keep in mind you may want add more Google services in future, e.g. YouTube Analytics, so it's suggested to keep the name generic but unique to you.
  * *Client ID Settings* > *Application type*: choose **"Installed application"** with **"Other"** as the sub-type.
  * Note down your **Client ID** and **Client secret**.

### 3. Set your system environment variables (this is optional but recommended)
  * In Windows 7: Search for *"Edit environment variables for your account"* from the start menu.
  * Within *Environment variables* > *User variables*: Add the following variables separately by selecting *New* and entering the name and values for each with the details you noted down from step 2:
  a. name = `GANALYTICS_CONSUMER_ID`, value = `<Your client ID>`
  b. name = `GANALYTICS_CONSUMER_SECRET`, value = `<Your client secret>`
  * Click *OK*, then *OK* again. Log-off windows then log-on again, or simply restart.

### 4. Authenticate while attempting your first query with ganalytics
  * To perform a basic query, you will need to obtain your Google Analytics **profile ID**. This can be accessed via the Admin page in Google Analytics under *"Profile Settings"*, or by simply copying from the browser's address bar when viewing a report in Google Analytics; look for the digits between the letter **"p"** and trailing **"/"**, e.g. `.../a11111111w22222222p33333333/` has a profile ID of `33333333`. Note: a function for accessing this from within R will be available in the near future.
  * Return back to R and execute the following, remembering to substitute `profile_id` with the profile ID you noted down. (Please be aware that a small file will be saved to your home directory ("My Documents" in Windows) containing your new reusable authentication token):
  
a. **If you completed step 3**:

```r
library(ganalytics)
myQuery <- GaQuery( profile_id )
GetGaData(myQuery)
```
  b. **If you did NOT complete step 3**; also substitute `client_id` and `client_secret` below with your details from step 2:

```r
library(ganalytics)
myQuery <- GaQuery( profile_id )
GetGaData(myQuery, key = client_id , secret = client_secret)  
```
  * You should be directed to *http://accounts.google.com* within your default web browser asking you to sign-in to your Google account if you are not already. Once signed-in you will be asked to grant read-only access to your Google Analytics account for the Google API product you created in step 1.
  * Make sure you are signed in to the Google account you wish to use, then grant access by selecting **"Allow access"**. You can then close the page and return back to R.
  * After you have successfully executed all of the above R commands you should see the output of the default `ganalytics` report: *visits by day for the past 7 days*.


Examples
--------

As demonstrated in the installation steps above, before executing any of the following examples, the `ganalytics` package must be loaded and a new gaQuery object be generated with a Google Analytics profile ID assigned to it. The following code performs these steps: (Remember to replace `123456789` with the profile ID you wish to use.) 

```r
library(ganalytics)
myQuery <- GaQuery( 123456789 )  # Replace this with your Google Analytics profile ID
```
If you have just completed the installation steps, then you would have already done this.

**The following examples assume you have successfully complete the above steps.**

### Example 1 - Setting the date range

```r
# Set the date range from 1 January 2013 to 31 May 2013: (Dates are specified in the format "YYYY-MM-DD".)
GaDateRange(myQuery) <- c("2013-01-01", "2013-05-31")

myData <- GetGaData(myQuery)
summary(myData)

# Adjust the start date to 1 March 2013:
GaStartDate(myQuery) <- "2013-03-01"
# Adjust the end date to 31 March 2013:
GaEndDate(myQuery) <- "2013-03-31"

myData <- GetGaData(myQuery)
summary(myData)
```

### Example 2 - Choosing what metrics to report

```r
# Report number of page views instead
GaMetrics(myQuery) <- "pageivews"

myData <- GetGaData(myQuery)
summary(myData)

# Report both pageviews and visits
GaMetrics(myQuery) <- c("pageviews", "visits")
# These variations are also acceptable
GaMetrics(myQuery) <- c("ga:pageviews", "ga.Visits")

myData <- GetGaData(myQuery)
summary(myData)
```

### Example 3 - Selecting what dimensions to split your metrics by

```r
# Similar to metrics, but for dimensions
GaDimensions(myQuery) <- c("year", "week", "dayofweek", "hour")

# Lets set a wider date range
GaDateRange(myQuery) <- c("2012-10-01", "2013-03-31")

myData <- GetGaData(myQuery)
head(myData)
tail(myData)
```

### Example 4 - Sort by

```r
# Sort by descending number of pageviews
GaSortBy(myQuery) <- "-pageviews"

myData <- GetGaData(myQuery)
head(myData)
tail(myData)
```

### Example 5 - Row filters

```r
# Filter for Sunday visits only
sundayExpr <- GaExpr("dayofweek", "=", "0")
GaFilter(myQuery) <- sundayExpr

myData <- GetGaData(myQuery)
summary(myData)

# Remove the filter
GaFilter(myQuery) <- NULL

myData <- GetGaData(myQuery)
summary(myData)
```

### Example 6 - Combining filters with AND

```r
# Expression to define Sunday visits
sundayExpr <- GaExpr("dayofweek", "=", "0")
# Expression to define organic search visits
organicExpr <- GaExpr("medium", "=", "organic")
# Expression to define organic search visits made on a Sunday
sundayOrganic <- GaAnd(sundayExpr, organicExpr)
GaFilter(myQuery) <- sundayOrganic

myData <- GetGaData(myQuery)
summary(myData)

# Let's concatenate medium to the dimensions for our query
GaDimensions(myData) <- c(GaDimensions(myData), "medium")

myData <- GetGaData(myQuery)
summary(myData)
```

### Example 7 - Combining filters with OR

```r
# In a similar way to AND
loyalExpr <- GaExpr("visitCount", "!~", "^[0-3]$") # Made more than 3 visits
recentExpr <- GaExpr("daysSinceLastVisit"), "~", "^[0-6]$") # Visited sometime within the past 7 days.
loyalOrRecent <- GaOr(loyalExpr, recentExpr)
GaFilter(myQuery) <- loyalOrRecent

myData <- GetGaData(myQuery)
summary(myData)
```

### Example 8 - Filters that combine ORs with ANDs

```r
loyalExpr <- GaExpr("visitCount", "!~", "^[0-3]$") # Made more than 3 visits
recentExpr <- GaExpr("daysSinceLastVisit"), "~", "^[0-6]$") # Visited sometime within the past 7 days.
loyalOrRecent <- GaOr(loyalExpr, recentExpr)
sundayExpr <- GaExpr("dayofweek", "=", "0")
loyalOrRecent_Sunday <- GaAnd(loyalOrRecent, sundayExpr)
GaFilter(myQuery) <- loyalOrrecent_Sunday

myData <- GetGaData(myQuery)
summary(myData)
```
### Example 9 - Visit segmentation

```r
# Visit segmentation is expressed similarly to row filters and supports AND and OR combinations.
# Define a segment for visits where a "thank-you" or "thankyou" page was viewed.
thankyouExpr <- GaExpr("pagePath", "~", "thank\\-?you")
GaSegment(myQuery) <- thankyouExpr

myData <- GetGaData(myQuery)
summary(myData)
```

### Example 10 -  Using automatic pagination to get more than 10,000 rows of data per query

```r
# Visits by date and hour for the years 2011 (leap year) and 2012: 2 * 365.5 * 24 = 17544 rows
# First let's clear any filters or segments defined previously
GaFilter(myQuery) <- NULL
GaSegment(myQuery) <- NULL
# Define our date range
GaDateRange(myQuery) <- c("2011-01-01", "2012-12-31")
# Define our metrics and dimensions
GaMetrics(myQuery) <- "visits"
GaDimensions(myQuery) <- c("date", "hour")
# Let's allow a maximum of 17544 rows (default is 10000)
GaMaxResults(myQuery) <- 17544

myData <- GetGaData(myQuery)
summary(myData)
nrow(myData)
```

Useful references
-----------------

1. [Google Analytics Core Reporting API reference guide](https://developers.google.com/analytics/devguides/reporting/core/v3/reference)
2. [Google Analytics Dimensions and Metrics reference](https://developers.google.com/analytics/devguides/reporting/core/dimsmets)
3. [Creating a Google API project](https://developers.google.com/console/help/#creatingdeletingprojects)
4. [Generating an OAuth 2.0 client ID for Google APIs](https://developers.google.com/console/help/#generatingoauth2)
5. [Using OAuth 2.0 for Installed Applications](https://developers.google.com/accounts/docs/OAuth2InstalledApp)

ganalytics
==========

Interact with **Google Analytics** using **R**

S4 classes and methods for interactive use of the Google Analytics core reporting API using R.


Installation
------------

### 1. Install the necessary packages into R
  * execute the following statements in R:

```r
install.packages("devtools") # Installs the latest version of devtools available from CRAN
library(devtools)
install_github(repo = "httr", username = "jdeboer", ref = "OAuth2.0-reference-class") # a dev branch of httr.
install_github(repo = "ganalytics", username = "jdeboer") # installs ganalytics from the GitHub repository.
```
### 2. Prepare your Google APIs application
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
  * To perform a basic query, you will need to obtain your Google Analytics **profile ID**. This can be accessed via the Admin page in Google Analytics under *"Profile Settings"*, or by simply copying from the browser's address bar when viewing a report in Google Analytics; look for the digits between the letter **"p"** and trailing **"/"**, e.g. ".../a11111111w22222222p33333333/". Note: a function for accessing this from within R will be available in the near future.
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
  * You should be directed to *http://accounts.google.com* within your default web browser asking you to sign-in to your Google account, or if you are already signed-in, you will be asked to grant read-only access for your Google Analytics account to the Google API product you created in step 1.
  * Make sure you are signed in to the Google account you wish to use, then grant access by selecting **"Allow access"**. You can then close the page and return back to R.
  * After you have successfully executed all of the above R commands you should see the output of the default `ganalytics` report: *visits by day for the past 7 days*.


Useful reference:
-----------------

1. [Google Analytics Core Reporting API reference guide](https://developers.google.com/analytics/devguides/reporting/core/v3/reference)
2. [Google Analytics Dimensions and Metrics reference](https://developers.google.com/analytics/devguides/reporting/core/dimsmets)

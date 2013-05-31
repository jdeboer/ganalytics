ganalytics
==========

Interact with Google Analytics using R

S4 classes and methods for interactive use of the Google Analytics core reporting API using R.

Installation
------------
library(devtools)
# install_github(repo = "httr", username = "jdeboer")
install_github(repo = "httr", username = "jdeboer", branch = "OAuth2.0-reference-class")

Create a query object using GaQuery(profileID), providing the applicable profileId, and assigning the resulting object to a symbol.
Manipulate the query object.
Execute the query object and get the resulting data from Google Analytics using GetGaData(query), passing the query object as the argument.

Google Analytics Core Reporting API reference: 

Example
-------
```r
profileId <- 123456789
myQuery <- GetGaData(profileId)
myData <- GetGaData(myQuery)
print(myData)
```
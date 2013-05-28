

# Google Analytics dimension types
kGaDimTypes <- list(
  dates = "ga:date",
  ints = c(
    "ga:dayOfWeek",
    "ga:hour",
    "ga:year",
    "ga:month",
    "ga:week",
    "ga:day",
    "ga:visitLength",
    "ga:daysSinceLastVisit",
    "ga:visitCount",
    "ga:nthDay",
    "ga:nthWeek",
    "ga:nthMonth"
  ),
  nums = c(
    "ga:latitude",
    "ga:longitude"
  ),
  bools = c(
    "ga:isMobile",
    "ga:isTablet",
    "ga:javaEnabled",
    "ga:searchUsed"
  )
)

# Constants
# ---------

# Google APIs OAuth2.0 URLs
auth_url <- "https://accounts.google.com/o/oauth2/auth"
token_url <- "https://accounts.google.com/o/oauth2/token"
scope_url <- "https://www.googleapis.com/auth/analytics.readonly"

# Google APIs requests base URLs
management.api <- "https://www.googleapis.com/analytics/v3/management"
reporting.api <- "https://www.googleapis.com/analytics/v3/data/ga"

# Google Analytics date formats
kGaDateInFormat <- "%Y-%m-%d"
kGaDateOutFormat <- "%Y%m%d"

# The earliest valid date is 20050101. There is no upper limit restriction for a start-date.
kGaDateOrigin <- as.Date("2005-01-01")

# Google Analytics expression operators
kGaOps <- list(
  met = c("==","!=","<",">","<=",">="),
  dim = c("==","!=","=~","!~","=@","!@")
)

# ganalytics tolerated dimension and metric prefixes
kGaPrefix <- "^ga[;\\-\\._]"

# Maximum dimensions and metrics allowed by Google Analytics Core Reporting API
kGaMax <- list(
  dimensions = 7,
  metrics = 10
)

# Maximum results per page and maximum rows accessible in a query.
kGaMaxResults <- 10000L
kGaMaxRows <- 1000000L

install.packages("httr")
install.packages("httpuv")

# Let's try a manual authentication prior to installation.

# Please run this block of code, substituting for your Google Developers API
# project client ID and client secret. (Remember to keep those hidden):

client_id <- "91043842526.apps.googleusercontent.com"
client_secret <- "5QtY70UvZHN2I76eAwTFkvBp"

client_id <- "insert your client ID here"
client_secret <- "insert your client secret here"

# Then run the following commands and let me know how it goes. If successful,
# then retry the installation of `ganalytics`. The idea is that authentication
# will have already been done, so it won't attempt to do that during
# installation:

library(httr)

scope <- "https://www.googleapis.com/auth/analytics.readonly"
url <- "https://www.googleapis.com/analytics/v3/metadata/ga/columns"
endpoint <- oauth_endpoints(name = "google")
cache <- file.path("~", ".google_apis_auth.RDS")
app <- oauth_app(
  appname = "GOOGLE_APIS",
  key = client_id,
  secret = client_secret
)
token <- oauth2.0_token(
  endpoint = endpoint,
  app = app,
  scope = scope,
  use_oob = FALSE,
  as_header = TRUE,
  cache = cache
)
config = config(token = token)
GET(url = url, config = config)

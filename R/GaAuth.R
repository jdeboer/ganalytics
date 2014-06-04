GaAuth <- function(
  appname = "GANALYTICS",
  scope = "https://www.googleapis.com/auth/analytics.readonly",
  key = NULL, secret = NULL,
  use_oob = FALSE,
  cache = getOption("httr_oauth_cache")
) {
  if (is.null(key)) {
    key <- Sys.getenv(str_c(toupper(appname), "_CONSUMER_ID"))
  }
  if (is.null(secret)) {
    secret <- Sys.getenv(str_c(toupper(appname), "_CONSUMER_SECRET"))
  }
  oauth <- oauth2.0_token(
    endpoint = oauth_endpoints("google"),
    app = oauth_app(
      appname = appname, key = key, secret = secret
    ),
    scope = scope,
    use_oob = use_oob,
    cache = cache
  )
  return(oauth)
}

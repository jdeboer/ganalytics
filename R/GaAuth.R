GaAuth <- function(authFile = "~/ganalytics_token.RDS", key = NULL, secret = NULL) {
  new_oauth(
    key = key,
    secret = secret,
    file = authFile,
    scope = "https://www.googleapis.com/auth/analytics.readonly",
    base_url = "https://accounts.google.com/o/oauth2",
    authorize = "auth",
    access = "token",
    appname = "GANALYTICS"
  )
}
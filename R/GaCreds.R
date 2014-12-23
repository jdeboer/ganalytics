#'@export
GaCreds <- function(
  userName = character(0),
  appCreds = NULL,
  cache = character(0),
  use_oob = FALSE,
  appname = "GANALYTICS"
){
  if (length(cache) == 0) {
    if (length(userName) == 0) {
      cache <- "~/ganalytics_token.RDS"
    } else {
      cache <- paste0("~/.", userName, "_ga_auth.RDS")
    }
  }
  list(
    app = app_oauth_creds(
      appname = appname,
      creds = appCreds
    ),
    user = list(
      login = userName,
      cache = cache
    ),
    use_oob = use_oob
  )
}


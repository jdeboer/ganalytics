#'@export
GaCreds <- function(
  userName = character(0),
  appCreds = NULL,
  cache = character(0),
  use_oob = FALSE,
  appname = "GANALYTICS"
){
  cache_generic_file_name <- paste0(tolower(appname), "_auth.RDS")
  cache_file_prefix <- "."
  cache_default_dir <- "~"
  if (length(cache) == 0) {
    if (length(userName) == 0) {
      cache <- cache_generic_file_name
    } else {
      cache <- paste0(userName, "_", cache_generic_file_name)
    }
    cache <- paste0(cache_default_dir, "/", cache_file_prefix, cache)
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

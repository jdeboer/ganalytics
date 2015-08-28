#' @include backwards-compatibility.R
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Welcome to ganalytics.\n\nTo get started, use the GoogleApiCreds() function to set credentials for authenticating with your Google Analytics API project. E.g:\n creds <- GoogleApiCreds("<my_user_name>@gmail.com", ".my_app_oauth_creds.json") \n\nThen, check out the list of available demos and package documentation with:\n vignette(package = "ganalytics")\n package?ganalytics\n demo(package = "ganalytics")\n\nFor help with authentication, enter:\n ?ganalytics::GoogleApiCreds')
}

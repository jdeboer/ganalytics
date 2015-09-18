#' @include deprecated.R
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    'Welcome to ganalytics.\n\nTo get started, use the GoogleApiCreds() function to set credentials for authenticating with your Google Analytics API project. E.g:\n creds <- GoogleApiCreds("<my_user_name>@gmail.com", ".my_app_oauth_creds.json") \n\nThen, check out the list of available demos and package documentation with:\n vignette(package = "ganalytics")\n package?ganalytics\n demo(package = "ganalytics")\n\nFor help with authentication, enter:\n ?ganalytics::GoogleApiCreds\n\nPlease note: Authentication will automatically direct you to Google\'s website where you will be asked to grant permission for your project to access Google Analytics - this will open within your default web browser. Unless a file path is provided at the time of authentication, tokens will be cached by httr in your home directory.')
}

.onLoad <- function(libname, pkgname) {
  invisible(GoogleApiCreds())
}


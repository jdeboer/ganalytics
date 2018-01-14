#' @include deprecated.R
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    'Welcome to ganalytics.',
    '',
    'To get started, use the GoogleApiCreds() function to set credentials for',
    'authenticating with your Google Analytics API project. E.g:',
    ' creds <- GoogleApiCreds("<my_user_name>@gmail.com", ".my_app_oauth_creds.json")',
    '',
    'Then, check out the list of available demos and package documentation with:',
    ' vignette(package = "ganalytics")',
    ' vignette("README",  "ganalytics")',
    ' package?ganalytics\n demo(package = "ganalytics")',
    ' demo("gademo", "ganalytics")',
    '',
    'For help with authentication, enter:',
    ' ?ganalytics::GoogleApiCreds',
    '',
    'Please note: Authentication will automatically direct you to Google\'s website',
    'where you will be asked to grant permission for your project to access Google',
    'Analytics - this will open within your default web browser. Unless a file path',
    'is provided at the time of authentication, tokens will be cached by httr in your',
    'home directory.',
    sep = "\n"
  ))
}

.onLoad <- function(libname, pkgname) {
  NULL
}

.onUnload <- function(libpath) {
  NULL
}

#' ArgList
#' 
#' If the only argument passed was already a list, then extract that list.
#' 
ArgList <- function(...) {
  exprList <- as.list(
    unlist(x = list(...), recursive = FALSE)
  )
}
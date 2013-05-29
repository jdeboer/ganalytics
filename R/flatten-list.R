#' flatten
#' 
#' Flatten a nested list while preserving the class of each element
#' 
#' Convert a list type object into a non-nested list, preserving
#' the original object classes.
#' 
#' @param x a list type object to flatten.
#' 
#' @return a list
#' 
#' @references \url{http://stackoverflow.com/a/8139959/1007029}
#' 
flatten <- function(x) {
  len <- sum(rapply(x, function(x) 1L))
  y <- vector('list', len)
  i <- 0L
  rapply(x, function(x) { i <<- i+1L; y[[i]] <<- x })
  y
}
#' flatten
#' Flatten a nested list while preserving the class of each element
#' Convert a list type object into a non-nested list, preserving
#' the original object classes.
#' @param x a list type object to flatten.
#' @return a list
#' @references \url{http://stackoverflow.com/a/8139959/1007029}
flatten <- function(x) {
  len <- sum(rapply(x, function(x) 1L))
  y <- vector('list', len)
  i <- 0L
  rapply(x, function(x) { i <<- i+1L; y[[i]] <<- x })
  y
}

#'CheckVectorBounds
#'Check the length of each named slot within object is within the lower and
#'upper bounds specified.
#'@param object an object with slots that match the names of slot_vector_bound_list
#'@param slot_vector_bound_list a named list of vectors specifying the upper
#'and lower bounds for the length of each slot of object.
CheckVectorBounds <- function(object, slot_vector_bound_list) {
  slot_vector_bounds <- data.frame(
    slot_vector_bound_list,
    row.names = c('lower', 'upper')
  )
  ret <- lapply(names(slot_vector_bounds), function(slot_name) {
    slot_length <- length(slot(object, slot_name))
    slot_bounds <- slot_vector_bounds[[slot_name]]
    names(slot_bounds) <- row.names(slot_vector_bounds)
    if(slot_length < slot_bounds['lower'] | slot_length > slot_bounds['upper']) {
      if (as.numeric(slot_bounds['lower'][1]) == as.numeric(slot_bounds['upper'][1])) {
        slot_bounds <- slot_bounds['lower']
        paste0("Slot '", slot_name, "' must be of length ", slot_bounds)
      } else {
        paste0("Slot '", slot_name, "' length must be from ",
               slot_bounds['lower'], " to ",
               slot_bounds['uppper'], "."
        )
      }
    } else {
      TRUE
    }
  })
  ret <- unlist(ret[sapply(ret, is.character)])
  if (length(ret) == 0) {
    ret <- TRUE
  }
  return(ret)
}

#' ArgList
#' If the only argument passed was already a list, then extract that list.
#' @param ... arguments or list of arguments
ArgList <- function(...) {
  exprList <- as.list(
    unlist(x = list(...), recursive = FALSE)
  )
}

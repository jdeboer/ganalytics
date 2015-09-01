#' @importFrom lazyeval lazy
#' @importFrom assertthat assert_that
NULL

# TO DO: Rename this function as it is the same name as a base function.
# Alternatively, make the base function generic and this a specific method of
# that function. Another alternative would be to make this a method of the
# GaSequence generic function (soon to be renamed to Sequence).

#' sequence.
#'
#' Create a sequence using non-standard evaluation.
#'
#' Steps must be separated by commas (\code{,}). ... denotes zero or more of any
#' interaction preceeding the step that follows.
#'
#' @param steps a list of expressions (of one or more conditions), each
#'   repreating a step in the sequence.
#'
#' @examples
#' a <- condition(pagePath == "/home")
#' b <- condition(eventCategory == "Video") &
#'   condition(eventAction == "Play")
#' c <- condition(medium == "email")
#' s <- sequence(steps = list( ..., a, ..., b, c ))
#'
#' @export
sequence <- function(steps){
  lazy_expr <- lazy(steps)
  assert_that(lazy_expr$expr[[1]] == "list")
  step_exprs <- lazy_expr$expr[-1]
  lapply(step_exprs, as.character)
}


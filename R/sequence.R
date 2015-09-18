#' @importFrom lazyeval lazy
#' @importFrom assertthat assert_that
NULL

#' sequential_segment.
#'
#' Create a sequence using R non-standard evaluation syntax.
#'
#' Steps must be separated by commas (\code{,}). ... denotes zero or more of
#' interactions may preceed the step that follows, otherwise without ... then
#' the there must not be any interactions between the adjacent steps.
#'
#' @param steps a list of expressions (of one or more conditions), each
#'   repreating a step in the sequence.
#'
#' @examples
#' a <- condition(pagePath == "/home")
#' b <- condition(eventCategory == "Video") &
#'   condition(eventAction == "Play")
#' c <- condition(medium == "email")
#' s <- sequential_segment(list( ..., a, ..., b, c ))
#'
#' @export
sequential_segment <- function(steps){
  lazy_expr <- lazy(steps)
  assert_that(lazy_expr$expr[[1]] == "list")
  step_exprs <- as.list(lazy_expr$expr[-1])
  dots <- sapply(step_exprs, function(expr) {any(as.character(expr) == "...")})
  laters <- c(TRUE, dots[-length(dots)])
  thens <- !(dots | laters)
  step_exprs[laters] <- lapply(step_exprs[laters], function(expr) {Later(eval(expr, envir = lazy_expr$env))})
  step_exprs[thens] <- lapply(step_exprs[thens], function(expr) {Then(eval(expr, envir = lazy_expr$env))})
  do.call(Sequence, step_exprs[thens | laters])
}


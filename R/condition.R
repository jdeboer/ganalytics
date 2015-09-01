#' @importFrom lazyeval lazy
#' @importFrom assertthat assert_that
NULL

#' condition
#'
#' Define a Google Analytics condition using a domain-specific language familiar
#' to R users and using non-standard evaluation.
#'
#' @param cond An expression in the form of: \code{<variable> <comparator>
#'   <operand>} where only \code{<operand>} is evaluated.
#'
#' @export
condition <- function(cond) {
  lazy_expr <- lazy(cond)
  assert_that(is.call(lazy_expr$expr))
  assert_that(length(lazy_expr$expr) == 3)
  comparator <- as.character(lazy_expr$expr[[1]])
  comparator <- switch(
    comparator,
    `%starts_with%` = "=@",
    `=@` = "=@",
    `%matches%` = "=~",
    `%=~%` = "=~",
    `%in%` = "[]",
    `%[]%` = "[]",
    `%between%` = "<>",
    `%<>%` = "<>",
    comparator
  )
  var <- as.character(lazy_expr$expr[[2]])
  operand <- lazy_expr$expr[[3]]
  Expr(var, comparator, eval(operand, envir = lazy_expr$env))
}

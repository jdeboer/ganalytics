#' @include utils.R
#' @importFrom stringr str_replace_all str_match str_split_fixed
#' @importFrom assertthat assert_that
NULL

# Coercing to .expr

# Need to consider escaping of the following characters in the operand:\|,;_
parseOperand <- function(operand, comparator) {
  if (comparator == "[]") {
    operand <- str_split(operand, "\\|")[[1]]
  } else if (comparator == "<>") {
    operand <- str_split_fixed(operand, "_", 2)[1,]
  }
  operand <- gsub("\\\\", "\\", operand)
}

# Need to redo this to properly handle escaping with the \ used with GA.
setAs(from = "character", to = ".expr", def = function(from) {
  ops <- union(kGaOps$met, kGaOps$dim)
  ops <- str_replace_all(ops, "(\\[|\\])", "\\\\\\1")
  ops <- paste(ops, collapse = "|")
  comparator <- str_match(from, ops)[1,1]
  x <- str_split_fixed(from, ops, 2)
  var <- Var(x[1,1])
  operand <- x[1,2]
  Expr(var, comparator, parseOperand(operand, comparator))
})

# Coercing to orExpr
setAs(from = ".expr", to = "orExpr", def = simpleCoerceToList)

setAs(from = "andExpr", to = "orExpr", def = function(from, to) {
  # This is currently only legal if the gaAnd object does not contain any gaOr
  # object of length greater than 1 OR if there is only one gaOr. Otherwise,
  # in a future implementation if any gaOr objects have a length greater than
  # 1, then they will need to be shortened to length 1 which is only possible
  # if each expression within that gaOr shares the same dimension and the
  # expression comparators and operands can be combined either as a match regex
  # or a match list.

  # Check that all contained gaOr objects in the list have a length of 1
  assert_that(all(sapply(from, length) == 1) | length(from) == 1)

  # Break apart the AND expression into OR expressions
  # then break apart each OR expression into single
  # expressions. Concatenate the single expressions
  # back up the chain. Then convert array into a list of
  # expressions to use for a new OR expression.

  orExpr <- as.list(do.call(c, do.call(c, from@.Data)))
  as(orExpr, to)
}
)

# Coercing to andExpr
setAs(from = "orExpr", to = "andExpr", def = simpleCoerceToList)

setAs(from = ".expr", to = "andExpr", def = function(from, to) {
  as(as(from, "orExpr"), "andExpr")
})

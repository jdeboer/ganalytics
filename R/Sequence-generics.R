#' Later.
#'
#' Treat a step within a sequence as happening at any point after any preceding
#' steps in the sequence, i.e. 'later'. 'Later' means 'followed by', but not
#' necessarily immediately.
#'
#' @param object The expression that should preceed others in the sequence.
#' @param ... Any other expressions that should follow the first one but before
#'   any others in the sequence.
#'
#' @return a gaSegmentSequenceStep object, with the immediate flag not set.
#'
#' @seealso Sequence
#'
#' @export
setGeneric(
  "Later",
  function(object, ...) {},
  valueClass = "gaSegmentSequenceStep",
  useAsDefault = FALSE
)

#' Then.
#'
#' Treat a step within a sequence as happening immediately after any preceding
#' steps in the sequence, i.e. 'immediately following'.
#'
#' @param object The expression that should \bold{immediately} preceed others in
#'   the sequence.
#' @param ... Any other expressions that should \bold{immediately} follow the
#'   first one but before any others in the sequence.
#'
#' @return a gaSegmentSequenceStep object, with the immediate flag set.
#'
#' @export
#' @seealso Sequence
setGeneric(
  "Then",
  function(object, ...) {},
  valueClass = "gaSegmentSequenceStep",
  useAsDefault = FALSE
)

#' First.
#'
#' If used at the beginning of a sequence, indicates that this step must match
#' the first interaction of included sessions and users within the select date
#' range. First expressly means 'first interaction' within the date range.
#'
#' @param object An expression that should be at the start of a sequence
#'   expression.
#' @param ... Any other expressions that should immediately follow the first
#'   expression.
#'
#' @return a gaSegmentSequenceStep object, with the immediate flag set.
#'
#'
#' @export
#' @seealso Sequence
setGeneric(
  "First",
  function(object, ...) {},
  valueClass = "gaSegmentSequenceStep",
  useAsDefault = FALSE
)

#' Sequence.
#'
#' Create a new gaSequence object
#'
#' @param object A sequence step or another expression that should be coerced to
#'   a sequence condition.
#' @param ... Other steps within the sequence condition, in the order in which
#'   they should be applied.
#' @param negation Logical TRUE or FALSE to match segments where this sequence
#'   has not occured.
#'
#' @export
setGeneric(
  "Sequence",
  function(object, ..., negation = FALSE) {
    if (!missing(negation)) {
      warning("Argument 'negation' is deprecated. Instead, please wrap the sequence or condtion within an Include or Exclude call.")
    }
    standardGeneric("Sequence")
  },
  valueClass = "gaSegmentSequenceFilter",
  useAsDefault = FALSE
)

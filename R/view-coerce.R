#' @include utils.R
#' @importFrom methods as<-
NULL

# Coercion to viewId
setAs(from = "numeric", to = "viewId", def = simpleCoerceData)
setAs(from = "character", to = "viewId", def = simpleCoerceData)

setAs(from = "gaView", to = "viewId",
      def = function(from, to) {
        as(from$id, "viewId")
      },
      replace = function(from, value) {
        from$id <- as(value, "viewId")
      }
)

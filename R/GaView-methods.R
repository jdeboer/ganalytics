#' @include query-classes.R
#' @include management-api-classes.R
#' @include view-coerce.R
#' @include Query-generics.R
#' @importFrom methods setMethod as as<-
NULL

#######\/ Consider changing to generic methods View and View<- rather than coercion
setAs(from = ".query", to = "viewId",
      def = function(from, to) {
        from@viewId
      },
      replace = function(from, value) {
        from@viewId <- as(value, "viewId")
        validObject(from)
        from
      }
)

#######\/ Consider changing to generic methods rather than coercion
# Select the default view of the property
setAs(from = "gaProperty", to = "viewId",
      def = function(from, to) {
        defaultView <- from$defaultView
        if (length(defaultView) == 0) {
          defaultView <- from$views$entities[[1]]
        }
        as(defaultView, "viewId")
      },
      replace = function(from, value) {
        as(from$defaultView, "viewId") <- as(value, "viewId")
      }
)

#######\/ Consider changing to generic methods rather than coercion
# Sselect the first property of the account, which is then
# used to select a view (as above).
setAs(from = "gaAccount", to = "viewId",
      def = function(from, to) {
        as(from$properties$entities[[1]], "viewId")
      },
      replace = function(from, value) {
        as(from$properties$entities[[1]], "viewId") <- as(value, "viewId")
      }
)

# -- GaView ----
#' @describeIn GaView Returns the ID of the supplied view, or the first view
#'   within the supplied property or the first view within the first property of
#'   the supplied account, or gets the view ID of the supplied query, or coerce a
#'   numeric or character into a viewId.
setMethod("GaView", c("ANY", "missing"),
          function(object) {
            as(object, "viewId")
          }
        )

#' @describeIn GaView Set the view of a query, returning the query with the updated view applied.
setMethod("GaView", c(".query", "ANY"),
          function(object, value) {
            as(object, "viewId") <- value
          }
        )

#' @describeIn GaView Replace the view being used by a query.
setMethod(f = "GaView<-", signature = c(".query", "ANY"),
          definition = function(object, value) {
            as(object, "viewId") <- value
            object
          }
        )

#' @describeIn GaView Replaces the view being used by a query.
setMethod(
  f = "GaView<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, "viewId") <- value
    object
  }
)

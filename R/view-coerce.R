#' @include utils.R
NULL

# Coercion to viewId
setAs(from = "numeric", to = "viewId", def = simpleCoerceData)
setAs(from = "character", to = "viewId", def = simpleCoerceData)

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

setAs(from = "gaView", to = "viewId",
      def = function(from, to) {
        as(from$id, "viewId")
      },
      replace = function(from, value) {
        from$id <- as(value, "viewId")
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

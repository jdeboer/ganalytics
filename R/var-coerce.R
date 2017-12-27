#' @include utils.R
#' @include management-api-classes.R
#' @importFrom methods extends
NULL

# Coercing to .var classes

setAs(from = "character", to = ".gaVar", def = function(from) {
  tryCatch(
    as(from, "gaMetVar"),
    error = function(e1) {
      tryCatch(
        as(from, "gaDimVar"),
        error = function(e2) {
          stop(e1, e2)
        }
      )
    }
  )
})

setAs(from = "character", to = ".mcfVar", def = function(from) {
  tryCatch(
    as(from, "mcfMetVar"),
    error = function(e1) {
      tryCatch(
        as(from, "mcfDimVar"),
        error = function(e2) {
          stop(e1, e2)
        }
      )
    }
  )
})

setAs(from = "character", to = ".rtVar", def = function(from) {
  tryCatch(
    as(from, "rtMetVar"),
    error = function(e1) {
      tryCatch(
        as(from, "rtDimVar"),
        error = function(e2) {
          stop(e1, e2)
        }
      )
    }
  )
})

setAs(from = "character", to = ".metVar", def = function(from) {
  tryCatch(
    as(from, "gaMetVar"),
    error = function(e1) {
      tryCatch(
        as(from, "mcfMetVar"),
        error = function(e2) {
          tryCatch(
            as(from, "rtMetVar"),
            error = function(e3) {
              stop(e1, e2, e3)
            }
          )
        }
      )
    }
  )
})

setAs(from = "character", to = ".dimVar", def = function(from) {
  tryCatch(
    as(from, "gaDimVar"),
    error = function(e1) {
      tryCatch(
        as(from, "mcfDimVar"),
        error = function(e2) {
          tryCatch(
            as(from, "rtDimVar"),
            error = function(e3) {
              stop(e1, e2, e3)
            }
          )
        }
      )
    }
  )
})

setAs(from = "character", to = ".var", def = function(from) {
  tryCatch(
    as(from, ".gaVar"),
    error = function(e1) {
      tryCatch(
        as(from, ".mcfVar"),
        error = function(e2) {
          tryCatch(
            as(from, ".rtVar"),
            error = function(e3) {
              stop(e1, e2, e3)
            }
          )
        }
      )
    }
  )
})

setAs(from = "character", to = "gaDimVar", def = simpleCoerce)
setAs(from = "character", to = "gaMetVar", def = simpleCoerce)
setAs(from = "character", to = "mcfDimVar", def = simpleCoerce)
setAs(from = "character", to = "mcfMetVar", def = simpleCoerce)
setAs(from = "character", to = "rtDimVar", def = simpleCoerce)
setAs(from = "character", to = "rtMetVar", def = simpleCoerce)

coerceVarNS <- function(from, to) {
  fromClass <- class(from)
  toClass <- to
  if (extends(fromClass, ".gaVar")) fromNS <- "ga"
  if (extends(toClass, ".gaVar")) toNS <- "ga"
  if (extends(fromClass, ".mcfVar")) fromNS <- "mcf"
  if (extends(toClass, ".mcfVar")) toNS <- "mcf"
  if (extends(fromClass, ".rtVar")) fromNS <- "rt"
  if (extends(toClass, ".rtVar")) toNS <- "rt"
  fromNS <- paste0(fromNS, ":")
  toNS <- paste0(toNS, ":")
  as(as(sub(fromNS, toNS, from), "character"), to)
}

setAs(from = ".mcfVar", to = ".gaVar", def = coerceVarNS)
setAs(from = ".rtVar", to = ".gaVar", def = coerceVarNS)
setAs(from = ".gaVar", to = ".mcfVar", def = coerceVarNS)
setAs(from = ".rtVar", to = ".mcfVar", def = coerceVarNS)
setAs(from = ".gaVar", to = ".rtVar", def = coerceVarNS)
setAs(from = ".mcfVar", to = ".rtVar", def = coerceVarNS)

setAs(from = "gaGoal", to = "gaMetVar",
      def = function(from, to) {
        Var(paste0("goal", from$id, "completions"))
      }
)


## access and replace methods
#if(!isGeneric("param<-")){ 
#    setGeneric("param<-", function(object, value) standardGeneric("param<-"))
#}

# ---- 'GaIsRegEx' ----
setGeneric(
  name = "GaIsRegEx",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "logical"
)

# ---- 'GaVar': Gets or Creates an object from the superclass .gaVar ----
setGeneric(
  name = "GaVar",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "character"
)

# ---- 'GaVar<-': Sets the value of an object or sets its slot belonging to the superclass .gaVar ----
setGeneric(
  name = "GaVar<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaVar<-")
    validObject(.Object)
    return(.Object)
  }
)

# ---- 'GaOperator' and 'GaOperator<-' ----

setGeneric(
  name = "GaOperator",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = ".gaOperator"
)

setGeneric(
  name = "GaOperator<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaOperator<-")
    validObject(.Object)
    return(.Object)
  }
)

# ------ 'GaDimOperator' and 'GaMetOperator' constructor methods ----

setGeneric(
  name = "GaDimOperator",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "gaDimOperator"
)

setGeneric(
  name = "GaMetOperator",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "gaMetOperator"
)

# ---- 'GaOperand' and 'GaOperand<-' ----

setGeneric(
  name = "GaOperand",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = ".gaOperand"
)

setGeneric(
  name = "GaOperand<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaOperand<-")
    validObject(.Object)
    return(.Object)
  }
)


# ---- GaNot ----

setGeneric(
  name = "GaNot",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = ".gaLogical"
)

# ---- GaExpr, GaOr, GaAnd ----

setGeneric(
  name = "GaExpr",
  def = function(.Object, gaOperator, gaOperand) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = ".gaExpr"
)

setGeneric(
  name = "GaOr",
  def = function(.Object, ...) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "gaOr"
)

setGeneric(
  name = "GaAnd",
  def = function(.Object, ...) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "gaAnd"
)

# ---- GaSegment ----

setGeneric(
  name = "GaSegment",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = ".gaSegmentOrNULL"
)

setGeneric(
  name = "GaSegment<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaSegment<-")
    validObject(.Object)
    return(.Object)
  }
)

# ---- GaFilter ----

setGeneric(
  name = "GaFilter",
  def = function(.Object, ...) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = ".gaFilterOrNULL"
)

setGeneric(
  name = "GaFilter<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaFilter<-")
    validObject(.Object)
    return(.Object)
  }
)


# -- 'GaDateRange' and 'GaDateRange<-' ----
setGeneric(
  name = "GaDateRange",
  def = function(.Object, endDate) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "gaDateRange"
)

setGeneric(
  name = "GaDateRange<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaDateRange<-")
    validObject(.Object)
    return(.Object)
  }
)

# -- 'GaStartDate' and 'GaStartDate<-' ----

setGeneric(
  name = "GaStartDate",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "Date"
)

setGeneric(
  name = "GaStartDate<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaStartDate<-")
    validObject(.Object)
    return(.Object)
  }
)

# -- 'GaEndDate' and 'GaEndDate<-' ----

setGeneric(
  name = "GaEndDate",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "Date"
)

setGeneric(
  name = "GaEndDate<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaEndDate<-")
    validObject(.Object)
    return(.Object)
  }
)

# -- 'GaMetrics' and 'GaMetrics<-' ----

setGeneric(
  name = "GaMetrics",
  def = function(.Object, ...) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "gaMetrics"
)

setGeneric(
  name = "GaMetrics<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaMetrics<-")
    validObject(.Object)
    return(.Object)
  }
)

# -- 'GaDimensions' and 'GaDimensions<-' ----

setGeneric(
  name = "GaDimensions",
  def = function(.Object, ...) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = ".gaDimensionsOrNULL"
)

setGeneric(
  name = "GaDimensions<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaDimensions<-")
    validObject(.Object)
    return(.Object)
  }
)

# -- 'GaSortBy' and 'GaSortBy<-' ----

setGeneric(
  name = "GaSortBy",
  def = function(.Object, ..., desc = logical(0)) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = ".gaSortByOrNULL"
)

setGeneric(
  name = "GaSortBy<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaSortBy<-")
    validObject(.Object)
    return(.Object)
  }
)

# -- 'GaProfileId' and 'GaProfileId<-' ----
setGeneric(
  name = "GaProfileId",
  def = function(.Object, ..., desc) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "gaProfileId"
)

setGeneric(
  name = "GaProfileId<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaProfileId<-")
    validObject(.Object)
    return(.Object)
  }
)


# -- 'GaMaxResults' and 'GaMaxResults<-' ----

setGeneric(
  name = "GaMaxResults",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "numeric"
)

setGeneric(
  name = "GaMaxResults<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaMaxResults<-")
    validObject(.Object)
    return(.Object)
  }
)

# -- GetGaUrl ----

setGeneric(
  name = "GetGaUrl",
  def = function(.Object) {
    stop(
      paste("No method defined for object of class", class(.Object), sep = ": ")
    )
  },
  valueClass = "utf8"
)


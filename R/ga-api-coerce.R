#' @include utils.R
#' @include init-methods.R
#' @importFrom methods setAs validObject
#' @importFrom plyr ldply alply
NULL

## As setAs looks within the global namespace for any internally called functions within
# the definition, then the following function is likely to throw an error if used with setAs.
compileOperand <- function(from) {
  unEscapedOperand <- as(Operand(from), "character")
  comparator <- as(Comparator(from), "character")
  compiledOperand <- gsub(
    pattern = "([,;])", # What about _ and | used within an operand when using <> or [] comparators
    replacement = "\\\\\\1",
    x = unEscapedOperand
  )
  if (isTRUE(comparator == "[]")) {
    compiledOperand <- paste0(compiledOperand, collapse = "|")
  } else if (isTRUE(from@comparator == "<>")) {
    compiledOperand <- paste0(compiledOperand, collapse = "_")
  }
  compiledOperand
}

# Coercing to character
setAs(from = ".metOperand", to = "character",
      def = function(from) {
        format(from@.Data, scientific = FALSE)
      },
      replace = function(from, value) {
        from@.Data <- as.numeric(value)
        validObject(from)
        from
      }
)

## the folowing function calls another function which is not in the global namesapce
# Coercing GA expressions to GA API compatible character strings
setAs(from = ".expr", to = "character", def = function(from, to) {
  operand_class <- class(from@operand)
  if(inherits(from, "gaExpr")) {
    if(from@comparator@operator == "BEGINS_WITH") {
      from@operand <- as(paste("^", quotemeta(from@operand), sep = ""), operand_class)
      Comparator(from) <- "=~"
    } else if(from@comparator@operator == "ENDS_WITH") {
      from@operand <- as(paste(quotemeta(from@operand), "$", sep = ""), operand_class)
      Comparator(from) <- "=~"
    }
  }
  paste0(
    if (class(from) == "gaSegMetExpr") {
      if (from@metricScope != "") paste0(from@metricScope, "::")
    },
    as(from@var, to),
    as(from@comparator, to),
    as(compileOperand(from), to)
  )
})

setAs(from = "orExpr", to = "character", def = function(from, to) {
  do.call(paste, c(lapply(from, as, to), sep = ","))
})

setAs(from = "andExpr", to = "character", def = function(from, to) {
  do.call(paste, c(lapply(from, as, to), sep = ";"))
})

setAs(from = "gaSegmentConditionFilter", to = "character",
      def = function(from) {
        paste0(
          "condition::",
          if (from@negation) {"!"} else {""},
          as(as(from, "andExpr"), "character")
        )
      }
)

setAs(from = "gaSegmentConditionFilter", to = "json",
      def = function(from) {
      }
)

setAs(from = "gaSegmentSequenceFilter", to = "character",
      def = function(from, to) {
        if (length(from) >= 1L) {
          paste0(
            "sequence::",
            if (from@negation) {"!"} else {""},
            do.call(
              paste0,
              lapply(seq_along(from),
                     FUN = function(sequenceStep) {
                       paste0(
                         if (sequenceStep > 1L) {
                           if (from[[sequenceStep]]@immediatelyPrecedes) {";->"} else {";->>"}
                         } else {
                           if (from[[sequenceStep]]@immediatelyPrecedes) {"^"} else {""}
                         },
                         as(from[[sequenceStep]], to)
                       )
                     }
              )
            )
          )
        } else {
          character(0)
        }
      }
)

setAs(from = "gaSegmentSequenceFilter", to = "json",
      def = function(from, to) {

      }
)

setAs(from = "gaDynSegment", to = "json",
      def = function(from, to) {
        lapply(from, FUN = function(segmentFilter) {
          as(segmentFilter, to)
        })
      }
)

scoped_segment_filter_list_to_char <- function(from, scope) {
  if (length(from) >= 1L) {
    paste(
      scope,
      do.call(
        paste,
        c(
          lapply(from, FUN = function(segmentFilter) {
            as(segmentFilter, "character")
          }),
          sep = ";"
        )
      ),
      sep = "::"
    )
  } else {
    character(0)
  }
}

setAs(from = "gaDynSegment", to = "character",
  def = function(from, to) {
    segment_filter_lists <- list(
      users = select_segment_filters_with_scope(from, "users"),
      sessions = select_segment_filters_with_scope(from, "sessions")
    )
    contains_empty_list <- unlist(lapply(segment_filter_lists, length)) == 0L
    segment_filter_lists <- segment_filter_lists[!contains_empty_list]
    segment_filter_lists <- lapply(names(segment_filter_lists), function(scope) {
      scoped_segment_filter_list_to_char(
        segment_filter_lists[[scope]],
        scope = scope
      )
    })
    paste(unlist(segment_filter_lists), collapse = ";")
  }
)

setAs(from = "gaDynSegment", to = "json",
      def = function(from, to) {
        # User-scoped segment filter list
        # Session-scoped segment filter list
      }
)

setAs(from = "gaSegmentId", to = "character",
      def = function(from) {
        as.character(from@.Data)
      },
      replace = function(from, value) {
        initialize(from, value)
      }
)

# Coercing from .metrics, .dimensions, and .sortBy to character
setAs(from = ".varList", to = "character",
      def = function(from) {
        paste(from@.Data, collapse = ",")
      },
      replace = function(from, value) {
        initialize(from, unlist(strsplit(value, split = ",")))
      }
)

setAs(from = ".sortBy", to = "character",
      def = function(from) {
        varNames <- sapply(from@.Data, FUN = function(varName) {
          as(varName, "character")
        })
        descChar <- ifelse(from@desc, "-", "")
        paste0(descChar, varNames, collapse = ",")
      },
      replace = function(from, value) {
        as(value, class(from))
      }
)

# Coercion to matrix
setAs(
  from = "gaQuery",
  to = "matrix",
  def = function(from) {
    segments <- Segments(from)
    if(length(segments) == 0L) {
      segments <- Segments(list(Segment(NULL)))
    }
    viewsDatesSegments <- expand.grid(
      list(
        viewId = as(GaView(from), "viewId"),
        dateRange = alply(data.frame(
          startDate = StartDate(from),
          endDate = EndDate(from)
        ), 1),
        segment = segments
      ),
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    )
    viewsDatesSegments <- c(
      list(
        viewId = viewsDatesSegments$viewId,
        segment = viewsDatesSegments$segment
      ),
      ldply(viewsDatesSegments$dateRange, .id = NULL)
    )
    params <- mapply(
      FUN = function(startDate, endDate, viewId, segment) {
        metrics <- from@metrics
        dimensions <- from@dimensions
        sortBy <- from@sortBy
        tableFilter <- from@filters
        c(
          "ids" = as(viewId, "character"),
          "start-date" = as.character(startDate),
          "end-date" = as.character(endDate),
          "metrics" = as(metrics, "character"),
          "dimensions" = if (length(dimensions) >= 1L) {
            as(dimensions, "character")
          },
          "sort" = if (length(sortBy) >= 1L) {
            as(sortBy, "character")
          },
          "filters" = if (length(tableFilter) >= 1L) {
            as(tableFilter, "character")
          },
          "segment" = if (length(segment) >= 1L) {
            as(segment, "character")
          },
          "samplingLevel" = as(from@samplingLevel, "character"),
          "include-empty-rows" = "false"
        )
      },
      viewsDatesSegments$startDate,
      viewsDatesSegments$endDate,
      viewsDatesSegments$viewId,
      viewsDatesSegments$segment
    )
  }
)

setAs(
  from = "mcfQuery",
  to = "matrix",
  def = function(from) {
    views <- as(GaView(from), "viewId")
    dateRange <- from@dateRange
    startDates <- dateRange@startDate
    endDates <- dateRange@endDate
    viewsDates <- do.call(
      what = rbind,
      args = lapply(
        X = views,
        FUN = function(viewId) {
          data.frame(
            startDate = startDates,
            endDate = endDates,
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(startDate, endDate, viewId) {
        metrics <- from@metrics
        dimensions <- from@dimensions
        sortBy <- from@sortBy
        tableFilter <- from@filters
        c(
          "ids" = as(viewId, "character"),
          "start-date" = as.character(startDate),
          "end-date" = as.character(endDate),
          "metrics" = as(metrics, "character"),
          "dimensions" = if (length(dimensions) >= 1L) {
            as(dimensions, "character")
          },
          "sort" = if (length(sortBy) >= 1L) {
            as(sortBy, "character")
          },
          "filters" = if (length(tableFilter) >= 1L) {
            as(tableFilter, "character")
          },
          "samplingLevel" = as(from@samplingLevel, "character")
        )
      },
      viewsDates$startDate,
      viewsDates$endDate,
      viewsDates$viewId
    )
  }
)

setAs(
  from = "rtQuery",
  to = "matrix",
  def = function(from) {
    views <- do.call(
      what = rbind,
      args = lapply(
        X = as(GaView(from), "viewId"),
        FUN = function(viewId) {
          data.frame(
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(viewId) {
        metrics <- from@metrics
        dimensions <- from@dimensions
        sortBy <- from@sortBy
        tableFilter <- from@filters
        c(
          "ids" = as(viewId, "character"),
          "metrics" = as(metrics, "character"),
          "dimensions" = if (length(dimensions) >= 1L) {
            as(dimensions, "character")
          },
          "sort" = if (length(sortBy) >= 1L) {
            as(sortBy, "character")
          },
          "filters" = if (length(tableFilter) >= 1L) {
            as(tableFilter, "character")
          }
        )
      },
      views$viewId
    )
  }
)

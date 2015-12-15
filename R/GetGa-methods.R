#' @include query-classes.R
#' @include Query-generics.R
#' @include ga-api-coerce.R
#' @include GaGetCoreReport.R
#' @importFrom methods setMethod as
#' @importFrom plyr alply ldply mutate llply laply join
#' @importFrom scales percent
NULL

#' GetGaData Execute a ganalytics query.
#' @param query the query to execute.
#' @param creds the Google APIs Project OAuth 2.0 credentials to use.
#' @param .progress progress bar to display. use .progress = "none" to turn off.
#' @param use_oob httr argument
#' @param addViewId logical value indicating whether to add a viewID column for
#'   when more than one view has been provided.
#' @param addSegmentId logical value indicating whether to add the name of the
#'   segment for when more than one segment has been queried.
#' @return a dataframe
setMethod("GetGaData", ".query", function(
  query,
  creds = NULL,
  .progress = "time",
  use_oob = FALSE,
  addViewId = FALSE,
  addSegmentId = FALSE
) {
  if (is.null(creds)) {
    creds <- query@creds
  }
  if (!missing(use_oob)) {
    warning("Argument 'use_oob' is defunct, please use the GaCreds or GoogleApiCreds functions instead to either supply a creds argument or to set the creds of the supplied query object.", call. = FALSE)
  }

  metrics <- as.character(Metrics(query))

  group_metrics <- function(metrics) {
    split(metrics, ceiling(seq_along(metrics) / kGaMax$metrics))
  }

  if (class(query) == "gaQuery") {
    segmentNames <- names(query@segments)
  } else {
    segmentNames <- NULL
  }

  if (length(segmentNames) == 0) {
    addSegmentId <- FALSE
  }

  data_by_metric_group <- lapply(group_metrics(metrics), function(metrics_group) {
    Metrics(query) <- metrics_group
    queryParams <- as(query, "matrix")

    # Need to determine if the query object is a MCF or GA query and tell GaPaginate
    responses <- llply(
      .data = seq_len(ncol(queryParams)),
      .fun = function(i) {
        GaPaginate(
          queryParams[, i],
          maxRequestedRows = MaxResults(query),
          creds = creds,
          queryClass = class(query),
          segmentName = segmentNames[i]
        )
      },
      .progress = .progress
    )

    data <- ldply(
      .data = responses,
      .fun = function(response) {
        df <- response$data
        if (addViewId & nrow(df) >= 1) {
          df <- mutate(df, viewId = response$viewId)
        }
        if (addSegmentId & nrow(df) >= 1) {
          df <- mutate(df, segment = response$segmentName)
        }
        return(df)
      }
    )
    attr(data, "sampleSize") <- sum(laply(responses, function(response){as.numeric(response$sampleSize)}))
    attr(data, "sampleSpace") <- sum(laply(responses, function(response){as.numeric(response$sampleSpace)}))
    sampleSize <- attr(data, "sampleSize")
    sampleSpace <- attr(data, "sampleSpace")
    sampled <- any(laply(responses, function(response) {isTRUE(response$sampled)}))
    if (sampled) {
      warning(paste("Contains sampled data: ", sampleSize, "/", sampleSpace, "(", percent(sampleSize/sampleSpace), ")."))
    }
    data
  })

  join_by_vars <- c("viewId"[addViewId], "segment"[addSegmentId], sub(kAnyPrefix, "", as.character(Dimensions(query))))

  if (length(join_by_vars) == 0) {
    if (length(data_by_metric_group) > 1) {
      required_rows <- 1:max(lapply(data_by_metric_group, nrow))
      data <- Reduce(function(x, y) {cbind(x[required_rows, ], y[required_rows, ])}, data_by_metric_group)
    } else {
      data <- data_by_metric_group[[1]]
    }
  } else {
    data <- Reduce(function(x, y) {join(x, y, by = join_by_vars, type = "full")}, data_by_metric_group)
  }

  metric_columns <- sub(kAnyPrefix, "", metrics)

  data[metric_columns] <- lapply(data[metric_columns], function(col) {
    col[is.na(col)] <- 0
    col
  })

  dim_cols <- colnames(data) %in% join_by_vars

  data[c(which(dim_cols), which(!dim_cols))]

})

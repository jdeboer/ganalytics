ColTypes <- function(df, colNames, asFun, ...) {
  #cols <- aaply(tolower(names(df)), 1, function(df_col) {any(str_detect(df_col, colNames))})
  cols <- tolower(names(df)) %in% tolower(colNames)
  if(TRUE %in% cols) df[cols] <- lapply(X = df[cols], FUN = asFun, ...)
  return(df)
}

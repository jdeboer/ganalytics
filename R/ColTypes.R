ColTypes <- function(df, colNames, asFun, ...) {
  cols <- tolower(names(df)) %in% tolower(colNames)
  if(TRUE %in% cols) df[cols] <- lapply(X = df[cols], FUN = asFun, ...)
  return(df)
}

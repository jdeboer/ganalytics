YesNo2Logical <- function(char) {
  if (length(char) > 0) {
    char[char=="Yes"] <- "TRUE"
    char[char=="No"] <- "FALSE"
  }
  char <- as.logical(char)
  return(char)
}

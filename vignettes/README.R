## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)

## ----echo = FALSE--------------------------------------------------------
knitr::include_graphics(
  devtools::package_file("inst", "figures", "hexicon.png")
)

## ------------------------------------------------------------------------
Sys.setenv(
  GOOGLE_APIS_CONSUMER_ID = "<Your client ID>",
  GOOGLE_APIS_CONSUMER_SECRET = "<Your client secret>"
)


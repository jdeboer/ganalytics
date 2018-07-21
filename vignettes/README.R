## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)

## ---- echo = FALSE-------------------------------------------------------
htmltools::img(knitr::image_uri(devtools::package_file("inst", "figures", "hexicon.png")), 
               alt = 'logo', 
               style = 'position:absolute; top:0; right:0; padding:10px;')

## ------------------------------------------------------------------------
Sys.setenv(
  GOOGLE_APIS_CONSUMER_ID = "<Your client ID>",
  GOOGLE_APIS_CONSUMER_SECRET = "<Your client secret>"
)


# This code finds and install the libraries used by the project (CRAN version)
renv::hydrate()

# Specific other packages
library("rmarkdown")
library("knitr")
library("pkgload")
library("remotes")
library("Rglpk")
library("sessioninfo")
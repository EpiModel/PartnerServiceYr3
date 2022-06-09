# CRAN packages
install.packages(c("remotes","tergmLite","EpiModel"))

# Github packages
remotes::install_github(c("EpiModel/EpiModelHIV-p@PartnerServicesYr3",
                          "EpiModel/ARTnetData",
                          "EpiModel/ARTnet"))
library("EpiModelHIV")
pkgload::load_all("../EpiModelHIV-p")

#renv
renv::snapshot()

# This code installs the packages only available on GitHub (not on CRAN)
renv::install(c(
  "EpiModel/EpiModel@main",
  "EpiModel/ARTnet@main",
  "EpiModel/EpiModelHIV-p@PartnerServicesYr3",
  "EpiModel/EpiModelHPC@main",
  "EpiModel/slurmworkflow@main"
))

# This code finds and install the libraries used by the project (CRAN version)
renv::hydrate()

# Force `renv` to discover the following packages
if (FALSE) {
  library("rmarkdown")
  library("pkgload")
  library("sessioninfo")
}


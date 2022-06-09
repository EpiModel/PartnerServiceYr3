## Updating packages
renv::status()
install.packages(c("remotes","tergmLite","EpiModel"))
remotes::install_github(c("EpiModel/EpiModelHIV-p@PartnerServicesYr3",
                          "EpiModel/ARTnetData","EpiModel/EpiModel",
                          "EpiModel/ARTnet","EpiModel/slurmworkflow",
                          "EpiModel/EpiModelHPC"))
remotes::install_github("EpiModel/EpiModelHIV-p@PartnerServicesYr3")
renv::snapshot()

#Seeing changes in EMHIV
# One approach is to:
# 1. Update EpiModelHIV (e.g., add the new trackers)
# 2. Commit those changes to Github
# 3. Switch over to your project repo
# 4. Restart Rstudio in the project repo
# 5. Use renv::update , which will pull the new version of EpiModelHIV
# 6. Restart Rstudio in the project repo
# 7. Run your code

# Another approach is to:
# 1. Update EpiModelHIV (e.g., add the new trackers)
# 2. Switch over to your project repo
# 3. “Soft reload” the updated version of EpiModelHIV with pkgload::load_all(path/to/EpiModelHIV)
# 4. Run the control settings function again (this ensures your updated module gets pulled in to netsim)
# 5. Run netsim
pkgload::load_all(path/to/EpiModelHIV)

write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
update.packages(c("MASS","Matrix","nlme"))

##
## 42. Remove sim files after processing for contour plot data
##


#get sims directory
sims_dir <- paste0("data/intermediate/hpc/figdata")

files_to_delete <- dir(path = sims_dir, pattern = "^sim__.*rds$")

file.remove(file.path(sims.dir, files_to_delete))

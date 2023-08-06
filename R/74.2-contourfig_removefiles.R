##
## 42. Remove sim files after processing for contour plot data
##


#remove sim files
sims_dir <- paste0("data/intermediate/hpc/figdata")

files_to_delete_1 <- dir(path = sims_dir, pattern = "^sim__.*rds$")
file.remove(file.path(sims_dir, files_to_delete_1))




#remove log files
log_dir <- paste0("workflows/psy3fitdat/log")

files_to_delete <- dir(path = log_dir, pattern = "^psy3figdat_step2.*out$")
file.remove(file.path(log_dir, files_to_delete))

files_to_delete <- dir(path = log_dir, pattern = "^psy3figdat_step5.*out$")
file.remove(file.path(log_dir, files_to_delete))

files_to_delete <- dir(path = log_dir, pattern = "^psy3figdat_step8.*out$")
file.remove(file.path(log_dir, files_to_delete))

files_to_delete <- dir(path = log_dir, pattern = "^psy3figdat_step11.*out$")
file.remove(file.path(log_dir, files_to_delete))

files_to_delete <- dir(path = log_dir, pattern = "^psy3figdat_step14.*out$")
file.remove(file.path(log_dir, files_to_delete))

files_to_delete <- dir(path = log_dir, pattern = "^psy3figdat_step17.*out$")
file.remove(file.path(log_dir, files_to_delete))

files_to_delete <- dir(path = log_dir, pattern = "^psy3figdat_step20.*out$")
file.remove(file.path(log_dir, files_to_delete))

files_to_delete <- dir(path = log_dir, pattern = "^psy3figdat_step23.*out$")
file.remove(file.path(log_dir, files_to_delete))
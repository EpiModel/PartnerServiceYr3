##
## 42. Remove sim and log files after processing for table 2 simulations
##

context <- "hpc"


context <- "local"
#remove sim files
sims_dir <- paste0("data/intermediate/",context,"/scenarios_tbl2")

simfiles_to_delete <- dir(path = sims_dir, pattern = "^sim__.*rds$")
file.remove(file.path(sims_dir, simfiles_to_delete))



#remove log files
log_dir <- paste0("workflows/psy3tbl2all/log")

logfiles_to_delete <- dir(path = log_dir, pattern = "^psy3tbl2all_step*out$")
file.remove(file.path(log_dir, logfiles_to_delete))
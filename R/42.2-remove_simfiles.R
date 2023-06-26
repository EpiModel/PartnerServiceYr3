##
## 42. Remove sim and log files after processing for table 2 simulations
##

context <- "hpc"


#remove sim files
sims_dir <- paste0("data/intermediate/",context,"/scenarios_tbl2")

simfiles <- list.files(sims_dir, full.names = T)
simfiles_to_delete <- simfiles[grep("^sim__.*rds$", simfiles)]
file.remove(simfiles_to_delete)



#remove log files
log_dir <- paste0("workflows/psy3tbl2all/log")

logfiles <- list.files(log_dir, full.names = T)
logfiles_to_delete <- logfiles[grep("^psy3tbl2all_step.*out$", logfiles)]
file.remove(logfiles_to_delete)


##
## 10. Epidemic Model Parameter Calibration, HPC setup
##



# Setup 
#-----------------------------------------------------------------------------------------
library("slurmworkflow")
library("EpiModelHPC")
library("EpiModelHIV")
library("tidyr")
library("dplyr")
library("ggplot2")

hpc_configs <- swf_configs_rsph(
  partition = "preemptable",
  r_version = "4.2.1",
  git_version = "2.35.1",
  mail_user = "uonwubi@emory.edu"
)

max_cores <- 32
cp_dir <- "data/cp_recal"




#Create workflow
#-----------------------------------------------------------------------------------------
wf <- create_workflow(
  wf_name = "psy3tbl2all",
  default_sbatch_opts = hpc_configs$default_sbatch_opts
)




#Step 1: Update renv (from GitHub project repo)
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_renv_restore(
    git_branch = "main",
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = hpc_configs$renv_sbatch_opts
)



#Set up simulation inputs
#-----------------------------------------------------------------------------------------
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R") 
source("R/utils-targets.R")

control <- control_msm(
  start = restart_time,
  nsteps = nsteps,
  nsims = 1, 
  ncores = 1,
  initialize.FUN = reinit_msm,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  
  .traceback.on.error = FALSE,
  .dump.frame.on.error = FALSE
  
  #raw.output = FALSE
)





# # Simulate HIV epidemic scenarios over estimated networks
# #-----------------------------------------------------------------------------------------
# 
# 
# # Table 2A ---------------------------------------------
# #set up scenarios
# scenarios.df.A <- readr::read_csv("./data/input/scenarios_tbl2A.csv")
# scenarios.list.A <- EpiModel::create_scenario_list(scenarios.df.A)
# 
# #step 2: run the simulations
# wf <- add_workflow_step(
#   wf_summary = wf,
#   step_tmpl = step_tmpl_netsim_scenarios(
#     path_to_restart, param, init, control,
#     scenarios_list = scenarios.list.A,
#     output_dir = "data/intermediate/hpc/scenarios_tbl2",
#     libraries = "EpiModelHIV",
#     save_pattern = "simple",
#     n_rep = 10 * max_cores,                                                                            
#     n_cores = max_cores,
#     max_array_size = 999,
#     setup_lines = hpc_configs$r_loader
#   ),
#   sbatch_opts = list(
#     "mail-type" = "FAIL,TIME_LIMIT,END",
#     "cpus-per-task" = max_cores,
#     "time" = "04:00:00",
#     "mem" = "0" # special: all mem on node
#   )
# )
# 
# #step 3: run the processing file
# wf <- add_workflow_step(
#   wf_summary = wf,
#   step_tmpl = step_tmpl_do_call_script(
#     r_script = "R/42-tbl2_dataprocessing.R",
#     args = list(
#       ncores = 15,
#       nsteps = 52
#     ),
#     setup_lines = hpc_configs$r_loader
#   ),
#   sbatch_opts = list(
#     "cpus-per-task" = max_cores,
#     "time" = "04:00:00",
#     "mem-per-cpu" = "4G",
#     "mail-type" = "END"
#   )
# )
# 
# 
# #step 4: remove sim files
# wf <- add_workflow_step(
#   wf_summary = wf,
#   step_tmpl = step_tmpl_do_call_script(
#     r_script = "R/42.2-remove_simfiles_tbl2.R",
#     args = list(
#       ncores = 15),
#     setup_lines = hpc_configs$r_loader
#   ),
#   sbatch_opts = list(
#     "cpus-per-task" = max_cores,
#     "time" = "04:00:00",
#     "mem-per-cpu" = "4G",
#     "mail-type" = "END"
#   )
# )



# Table 2B ----------------------------------------------
#set up scenarios
scenarios.df.B <- readr::read_csv("./data/input/scenarios_tbl2B.csv")
scenarios.list.B <- EpiModel::create_scenario_list(scenarios.df.B)

#step 5: run the simulations
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    path_to_restart, param, init, control,
    scenarios_list = scenarios.list.B,
    output_dir = "data/intermediate/hpc/scenarios_tbl2",
    libraries = "EpiModelHIV",
    save_pattern = "simple",
    n_rep = 10 * max_cores,                                                                            
    n_cores = max_cores,
    max_array_size = 999,
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "mail-type" = "FAIL,TIME_LIMIT,END",
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem" = "0" # special: all mem on node
  )
)

#step 6: run the processing file
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/42-tbl2_dataprocessing.R",
    args = list(
      ncores = 15,
      nsteps = 52
    ),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END"
  )
)


#step 7: remove sim files
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/42.2-remove_simfiles_tbl2.R",
    args = list(
      ncores = 15),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END"
  )
)



# Table 2C ----------------------------------------
#set up scenarios
scenarios.df.C <- readr::read_csv("./data/input/scenarios_tbl2C.csv")
scenarios.list.C <- EpiModel::create_scenario_list(scenarios.df.C)

#step 8: run the simulations
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    path_to_restart, param, init, control,
    scenarios_list = scenarios.list.C,
    output_dir = "data/intermediate/hpc/scenarios_tbl2",
    libraries = "EpiModelHIV",
    save_pattern = "simple",
    n_rep = 10 * max_cores,                                                                            
    n_cores = max_cores,
    max_array_size = 999,
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "mail-type" = "FAIL,TIME_LIMIT,END",
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem" = "0" # special: all mem on node
  )
)

#step 9: run the processing file
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/42-tbl2_dataprocessing.R",
    args = list(
      ncores = 15,
      nsteps = 52
    ),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END"
  )
)


#step 10: remove sim files
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/42.2-remove_simfiles_tbl2.R",
    args = list(
      ncores = 15),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END"
  )
)



# Table 2D -------------------------------------------
#set up scenarios
scenarios.df.D <- readr::read_csv("./data/input/scenarios_tbl2D.csv")
scenarios.list.D <- EpiModel::create_scenario_list(scenarios.df.D)

#step 11: run the simulations
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    path_to_restart, param, init, control,
    scenarios_list = scenarios.list.D,
    output_dir = "data/intermediate/hpc/scenarios_tbl2",
    libraries = "EpiModelHIV",
    save_pattern = "simple",
    n_rep = 10 * max_cores,                                                                            
    n_cores = max_cores,
    max_array_size = 999,
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "mail-type" = "FAIL,TIME_LIMIT,END",
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem" = "0" # special: all mem on node
  )
)

#step 12: run the processing file
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/42-tbl2_dataprocessing.R",
    args = list(
      ncores = 15,
      nsteps = 52
    ),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END"
  )
)


#step 13: remove sim files
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/42.2-remove_simfiles_tbl2.R",
    args = list(
      ncores = 15),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END"
  )
)





# to send restart file to the HPC (Run in R terminal)
# scp -r data/intermediate/hpc/estimates sph: /projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/
# scp -r data/intermediate/hpc/estimates sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/
  
# to send workflows to the HPC (Run in R terminal)
# scp -r workflows/psy3tbl2all sph:projects/epimodel/uonwubi/PartnerServiceYr3/workflows

# # to execute jobs
# chmod +x workflows/put_wf_name_here/start_workflow.sh    
# ./workflows/put_wf_name_here/start_workflow.sh 

# to get the data back after simulations (Run in R terminal)
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc data/intermediate

# to get only the processed files back
# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/processed data/intermediate/hpc/

# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/scenarios_tbl2 data/intermediate/hpc/
  
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/workflows/modeltest/log data/intermediate/


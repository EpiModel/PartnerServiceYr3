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
  wf_name = "psy3figdat",
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
  
  .traceback.on.error = TRUE,
  .dump.frame.on.error = FALSE
  
  #raw.output = FALSE
)



#Set up scenarios
#-----------------------------------------------------------------------------------------
# if using scenarios described in a csv doc
scenarios.df <- readr::read_csv("./data/input/contourparms_base.csv")

scenarios.list <- EpiModel::create_scenario_list(scenarios.df)



#Step 2: Simulate HIV epidemic scenarios over estimated networks
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    path_to_restart, param, init, control,
    scenarios_list = scenarios.list,
    output_dir = "data/intermediate/hpc/figdata",
    libraries = "EpiModelHIV",
    save_pattern = "simple",
    n_rep = 25,                                                                            
    n_cores = max_cores,
    max_array_size = 999,
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "mail-type" = "FAIL,TIME_LIMIT",
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem" = "0" # special: all mem on node
  )
)




#Step 3: Process output data for base table
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/44-fig_dataprocessing.R",
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



#Step 4: remove sims for base table (after processing)
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/44.2-fig_simsremove.R",
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




#Set up scenarios for max scenarios
#-----------------------------------------------------------------------------------------
# if using scenarios described in a csv doc
scenarios.df2 <- readr::read_csv("./data/input/contourparms_max.csv")
scenarios.list2 <- EpiModel::create_scenario_list(scenarios.df2)



#Step 5: Simulate HIV epidemic scenarios over estimated networks
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    path_to_restart, param, init, control,
    scenarios_list = scenarios.list2,
    output_dir = "data/intermediate/hpc/figdata",
    libraries = "EpiModelHIV",
    save_pattern = "simple",
    n_rep = 25,                                                                            
    n_cores = max_cores,
    max_array_size = 999,
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "mail-type" = "FAIL,TIME_LIMIT",
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem" = "0" # special: all mem on node
  )
)




#Step 6: Process output data for max scenarios
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/44-fig_dataprocessing.R",
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



#Step 7: remove sims for max table (after processing)
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/44.2-fig_simsremove.R",
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
# scp -r workflows/psy3figdat sph:projects/epimodel/uonwubi/PartnerServiceYr3/workflows

# to get the data back after simulations (Run in R terminal)
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc data/intermediate

# to get only the processed files back
# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/processed data/intermediate/hpc/

# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/figdata data/intermediate/hpc/
  
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/workflows/modeltest/log data/intermediate/

# to execute jobs
  # chmod +x workflows/put_wf_name_here/start_workflow.sh    
  # ./workflows/put_wf_name_here/start_workflow.sh 
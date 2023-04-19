##
## 10. Epidemic Model Parameter Calibration, HPC setup
##



# Setup 
#-----------------------------------------------------------------------------------------
#renv::install("EpiModel", rebuild = TRUE)
#renv::snapshot()

library("slurmworkflow")
library("EpiModelHPC")
library("EpiModelHIV")
library("tidyr")
library("dplyr")
library("ggplot2")

hpc_configs <- swf_configs_rsph(
  partition = "epimodel",
  r_version = "4.2.1",
  git_version = "2.35.1",
  mail_user = "uonwubi@emory.edu"
)

max_cores <- 32
cp_dir <- "data/cp_recal"



#Create workflow
#-----------------------------------------------------------------------------------------
wf <- create_workflow(
  wf_name = "psy3sim",
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
  .dump.frame.on.error = TRUE
  
  #raw.output = FALSE
)



#Set up scenarios
#-----------------------------------------------------------------------------------------
# if using scenarios described in a csv doc
  scenarios_df <- readr::read_csv("./data/input/scenarios.csv")

# #if using scenarios described using tibble::tibble
# scenarios.df <- tibble::tibble(
#   .scenario.id = c("base", "interv1", "interv2", "both"),
#   .at = 1,
#   prevpos.retest.start	= c(Inf, interv_start, Inf, interv_start),
#   second.genps.start	= c(Inf, Inf, interv_start, interv_start)
# )
scenarios.list <- EpiModel::create_scenario_list(scenarios.df)



#Step 2: Simulate HIV epidemic over estimated networks
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    path_to_restart, param, init, control,
    scenarios_list = scenarios.list,
    output_dir = "data/intermediate/hpc/scenarios",
    libraries = "EpiModelHIV",
    save_pattern = "simple",
    n_rep = 2,                                                                            
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



# #Step 3: Process data from model scenarios output
# #-----------------------------------------------------------------------------------------
# wf <- add_workflow_step(
#   wf_summary = wf,
#   step_tmpl = step_tmpl_do_call_script(
#     r_script = "R/41-intervention_scenarios_process.R",
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



#Step 3: Process output data for paper
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/42-paper_counterfactuals_process.R",
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




# to send restart file to the HPC (Run in R terminal)
# scp -r data/intermediate/hpc/estimates sph: /projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/
# scp -r data/intermediate/hpc/estimates sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/
  
# to send workflows to the HPC (Run in R terminal)
# scp -r workflows/modeltest sph:/projects/epimodel/uonwubi/PartnerServiceYr3/workflows
# scp -r workflows/psy3sim sph:projects/epimodel/uonwubi/PartnerServiceYr3/workflows

# to get the data back after simulations (Run in R terminal)
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc data/intermediate

# to get only the processed files back
# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/processed data/intermediate/hpc/

# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/scenarios data/intermediate/hpc/
  
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/workflows/modeltest/log data/intermediate/


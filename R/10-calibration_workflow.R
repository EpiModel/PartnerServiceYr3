##
## 10. Epidemic Model Parameter Calibration, HPC setup
##

# Setup 
#-----------------------------------------------------------------------------------------
library("slurmworkflow")
library("EpiModelHPC")
library("EpiModelHIV")

hpc_configs <- swf_configs_rsph(
  partition = "epimodel",
  mail_user = "uonwubi@emory.edu"
)

max_cores <- 10
cp_dir <- "data/cp_recal"



#Create workflow
#-----------------------------------------------------------------------------------------
wf <- create_workflow(
  wf_name = "modeltest",
  default_sbatch_opts = hpc_configs$default_sbatch_opts
)




#Update renv (from GitHub project)
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_renv_restore(
    git_branch = "main",
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = hpc_configs$renv_sbatch_opts
)



#Set up network simulation inputs
#-----------------------------------------------------------------------------------------
#network size
source("R/utils-netsize.R") 

#netsim iputs (epistats, netstats, netest estimates, param, init and relevant times)
source("R/utils-netsim_inputs.R")

control <- control_msm(
  nsteps = nsteps,
  nsims = 1, 
  ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  #raw.output = FALSE
)



#Set up scenarios
#-----------------------------------------------------------------------------------------
scenarios.df <- tibble::tibble(
  .scenario.id = c("base", "interv1", "interv2", "both"),
  .at = 1,
  prevpos.retest.start	= c(Inf, interv_start, Inf, interv_start),
  second.genps.start	= c(Inf, Inf, interv_start, interv_start)
)
scenarios.list <- EpiModel::create_scenario_list(scenarios.df)



#Run netsim
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    est, param, init, control,
    scenarios_list = scenarios.list,
    output_dir = "data/output/modeltest",
    libraries = "EpiModelHIV",
    n_rep = 100,                                                                            
    n_cores = max_cores,
    max_array_size = 500,
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "mail-type" = "FAIL,TIME_LIMIT",
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem" = "0" # special: all mem on node
  )
)

# Get data frame with all scenarios (intervention data only)
#-----------------------------------------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/100-get_interv_data.R",
    args = list(
      ncores = 20,
      cp_dir = cp_dir
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

# to send the workflows on the HPC
  # Mine (Run in R terminal)
  # scp -r workflows/modeltest sph:/projects/epimodel/uonwubi/PartnerServiceYr3/workflows/modeltest


# to get the data back
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/data/output/modeltest data/output
##
## 10. Epidemic Model Parameter Calibration, HPC setup
##

# Setup ------------------------------------------------------------------------
library("slurmworkflow")
library("EpiModelHPC")

# hpc_configs <- swf_configs_hyak(hpc = "mox", partition = "csde")
hpc_configs <- swf_configs_rsph(
  partition = "epimodel",
  mail_user = "uonwubi@emory.edu"
)

max_cores <- 32

# Workflow creation ------------------------------------------------------------
wf <- create_workflow(
  wf_name = "calibration",
  default_sbatch_opts = hpc_configs$default_sbatch_opts
)

# Update RENV on the HPC -------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_renv_restore(
    git_branch = "main",
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = hpc_configs$renv_sbatch_opts
)

# # Run the simulations ----------------------------------------------------------
source("R/utils-netsize.R")
source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")

cp_dir <- "data/cp_recal"

control <- control_msm(
  nsteps = nsteps,
  nsims = 1, ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  #.tracker.list = calibration_trackers, # created in R/utils-targets.R,
  # .checkpoint.dir = cp_dir,
  # .checkpoint.clear = FALSE,
  # .checkpoint.steps = 30 * 52,
  verbose = FALSE,
  raw.output = FALSE
)

# insert test values here
# scenarios.df <- tibble(
#   .scenario.id = c("base", "interv1", "interv2", "both"),
#   .at = 1,
#   prevpos.retest.start	= c(Inf, interv_start, Inf, interv_start),
#   second.genps.start	= c(Inf, Inf, interv_start, interv_start)
# )
scenarios.df <- tibble(
  .scenario.id = c("base", "both"),
  .at = 1,
  prevpos.retest.start	= c(Inf, interv_start),
  second.genps.start	= c(Inf, interv_start)
)
scenarios.list <- EpiModel::create_scenario_list(scenarios.df)

wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    est, param, init, control,
    scenarios_list = scenarios.list,
    output_dir = "data/output/calib",
    libraries = "EpiModelHIV",
    n_rep = 1,                                                                            
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

# # Process calibrations ---------------------------------------------------------
# # produce a data frame with the calibration targets for each scenario
# wf <- add_workflow_step(
#   wf_summary = wf,
#   step_tmpl = step_tmpl_do_call_script(
#     r_script = "R/12-calibration_process.R",
#     args = list(
#       ncores = 15,
#       nsteps = 52,
#       cp_dir = cp_dir
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
# to send the workflows on the HPC
  # Mine (Run in R terminal)
  # scp -r workflows/calibration sph:/projects/epimodel/uonwubi/PartnerServiceYr3/workflows/calibration

# to execute the workflow (do on the HPC *****Windows users have to run both code lines****)
  # chmod +x workflows/calibration/start_workflow.sh    
  # ./workflows/calibration/start_workflow.sh 


# to get the data back
# 
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/data/output/calib data/output

# 

##
## 00. Network Model Estimation, HPC setup
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
  wf_name = "estimation",
  default_sbatch_opts = hpc_configs$default_sbatch_opts
)


# Update RENV on the HPC -------------------------------------------------------
# Will run: git pull and renv::restore()
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_renv_restore(
    git_branch = "main",
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = hpc_configs$renv_sbatch_opts
)


# Estimate the networks --------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/01-estimation.R",
    args = list(
      ncores = max_cores
   ),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "24:00:00",
    "mem" = "0" # special: all mem on node
  )
)

# Generate the diagnostics data ------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/02-diagnostics.R",
    args = list(
      ncores = max_cores,
      nsims = 100,
      nsteps = 500
    ),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END" # to get a mail upon completion
  )
)


# to send the workflows on the HPC (Run on R terminal, not console)
  # scp -r workflows/estimation sph:/projects/epimodel/uonwubi/PartnerServiceYr3/workflows/estimation

# to execute the workflow (do on the HPC *****Windows users have to run both code lines****)
  # chmod +x workflows/estimation/start_workflow.sh    
  # ./workflows/estimation/start_workflow.sh 



# to get the data back
  # scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/data/input data/input

##
## Probabilistic Bias Analysis of Stigma-MH association
##



# Setup 
#-----------------------------------------------------------------------------------------
library("slurmworkflow")
library("EpiModelHPC")
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
wf <- create_workflow(
  wf_name = "s2_pba",
  default_sbatch_opts = hpc_configs$default_sbatch_opts
)



#Update renv (from GitHub project repo)
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_renv_restore(
    git_branch = "main",
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = hpc_configs$renv_sbatch_opts
)



# Misclassification bias adjustment
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/79.1-stigmaMH_pba_S2.R",
    args = list(ncores = 15),
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "400:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END"
  )
)







#rds files to be transferred to hpc - newdata, c_valdat and nc_valdat
#to send folder (mplusdat has all 3 rds files)
# scp -r data/aim1/mplusdat_mh sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/aim1

# to send workflows to the HPC (Run in R terminal)
# scp -r workflows/s2_pba sph:projects/epimodel/uonwubi/PartnerServiceYr3/workflows

# to get only the processed files back
# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/aim1/output data/aim1/


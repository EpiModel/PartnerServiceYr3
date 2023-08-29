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
  partition = "preemptable",
  r_version = "4.2.1",
  git_version = "2.35.1",
  mail_user = "uonwubi@emory.edu"
)

max_cores <- 32
cp_dir <- "data/cp_recal"
#numsims <- 10 * max_cores




#Create workflow
wf <- create_workflow(
  wf_name = "pba_stigMH",
  default_sbatch_opts = hpc_configs$default_sbatch_opts
)




# Misclassification bias adjustment
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/79.1-stigmaMH_pba.R",
    args = list(ncores = 15),
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
# scp -r data/aim1/mplusdat sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/aim1
  
# to send workflows to the HPC (Run in R terminal)
# scp -r workflows/pba_stigMH sph:projects/epimodel/uonwubi/PartnerServiceYr3/workflows

# to get the data back after simulations (Run in R terminal)
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc data/intermediate

# to get only the processed files back
# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/processed data/intermediate/hpc/

# scp -r sph:projects/epimodel/uonwubi/PartnerServiceYr3/data/intermediate/hpc/scenarios_tbl3 data/intermediate/hpc/
  
# scp -r sph:/projects/epimodel/uonwubi/PartnerServiceYr3/workflows/modeltest/log data/intermediate/


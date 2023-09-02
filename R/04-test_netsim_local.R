#  
# Base model testing (running netsim() on local PC)
# 


library("EpiModelHIV")


context <- "local"



#Test with uncalibrated model (no restart.rds file) 
#-----------------------------------------------------------------------------------------
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")


epistats <- readRDS("data/intermediate/local/estimates/epistats-local.rds")
netstats <- readRDS("data/intermediate/local/estimates/netstats-local.rds")
est      <- readRDS("data/intermediate/local/estimates/netest-local.rds")


control <- control_msm(
  nsteps = nsteps,
  nsims = 1,                          #change num of sims
  ncores = 1)


sim <- netsim(est, param, init, control)


#check output
print(sim)
par(mar = c(3, 3, 2, 2), mgp = c(2, 1, 0))
plot(sim, y = "i.num", main = "Prevalence")
plot(sim, y = "ir100", main = "Incidence")

df <- as.data.frame(sim)
head(df)
tail(df)


#De-bugging 
pkgload::load_all("C:/Users/Uonwubi/OneDrive - Emory University/Desktop/Personal/RSPH EPI Docs/RA2/GitRepos/EpiModelHIV-p")

control <- control_msm(
  nsteps = 120,
  nsims = 1,
  ncores = 1,
)

# debug(partident_msm) 
# debug(hivtest_msm)
# debug(prep_msm)
sim <- netsim(est, param, init, control)
# undebug(partident_msm)
# undebug(hivtest_msm)
# undebug(prep_msm)

  



  
#Test with calibrated model (has restart.rds file - local) 
#-----------------------------------------------------------------------------------------
rm(list = ls())


context <- "local"


library("EpiModelHIV")


epistats <- readRDS("data/intermediate/local/estimates/epistats-local.rds")
netstats <- readRDS("data/intermediate/local/estimates/netstats-local.rds")
est      <- readRDS("data/intermediate/local/estimates/netest-local.rds")


source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")
source("R/utils-netsize.R")


control <- control_msm(
  start               = restart_time,
  nsteps              = nsteps,
  nsims               = 1,
  ncores              = 1,
  initialize.FUN      = reinit_msm,
  cumulative.edgelist = TRUE,
  truncate.el.cuml    = 0,
  .tracker.list       = calibration_trackers,
  verbose             = TRUE)


#scenarios
# scenarios_df <- tibble::tibble(
#   .scenario.id = c("base","interv1","interv2", "both"),
#   .at = 1,
#   prevpos.retest.start	= c(Inf,interv_start, Inf, interv_start),
#   second.genps.start	= c(Inf,Inf, interv_start, interv_start)
# )
# scenarios_list <- EpiModel::create_scenario_list(scenarios_df)
  
scenarios.df <- readr::read_csv("./data/input/scenarios_tbl3A.csv")
scenarios.list <- EpiModel::create_scenario_list(scenarios.df)



#Method 1: run scenarios as on the hpc ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (will output sims of intervention scenarios in batches)
pkgload::load_all("C:/Users/Uonwubi/OneDrive - Emory University/Desktop/Personal/RSPH EPI Docs/RA2/GitRepos/EpiModelHIV-p")

EpiModelHPC::netsim_scenarios(
  path_to_restart, param, init, control, scenarios.list,
  n_rep = 1,
  n_cores = max_cores,
  output_dir = paste0("data/intermediate/",context,"/scenarios"),
  libraries = "EpiModelHIV",
  save_pattern = "simple"
)
list.files(paste0("data/intermediate/",context,"/scenarios"))
 
      
 
#Method 2: Use to debug in scenario testing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d_list <- vector(mode = "list", length = length(scenarios.list))
names(d_list) <- names(scenarios.list)

pkgload::load_all("C:/Users/Uonwubi/OneDrive - Emory University/Desktop/Personal/RSPH EPI Docs/RA2/GitRepos/EpiModelHIV-p")

control <- control_msm(
  simno = 1,
  nsteps = nsteps,
  nsims = 1,
  ncores = 1,
  verbose = TRUE)

# debug(partident_msm)
# debug(hivtest_msm)
# debug(prep_msm)
# debug(hivtx_msm)
# options(error = recover)
# if(at>=3121) browser()
for (scenario in scenarios.list){
  print(scenario$id)
  sc.param <- use_scenario(param, scenario)
  sim <- netsim(est, sc.param, init, control)
  
  saveRDS(sim, paste0("data/intermediate/",context,"/scenarios/sim__",scenario$id,"__1.rds"))

  #convert sim object to a df
  d_sim <- as.data.frame(sim)
  d_sim[["scenario"]] <- scenario$id
  d_list[[scenario$id]] <-d_sim
}
  # undebug(hivtest_msm)
  # undebug(partident_msm)
  # undebug(hivtx_msm)
  # undebug(prep_msm)
  # 

  
  
  
#Process output (scenario testing)
#-----------------------------------------------------------------------------------------
source("R/04.3-tbl3_outputprocess_local.R")
  

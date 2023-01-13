

# Netsim on local: Base model testing ----------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

## Example interactive epidemic simulation run script with basic parameterization
##    and all parameters defined in `param_msm`, with example of writing/debugging modules

#source("R/utils-0_project_settings.R")
context <- "local"

#Libraries
library("EpiModelHIV")


#Set up files 
#~~~~~~~~~~~~
epistats <- readRDS("data/intermediate/estimates/epistats-local.rds")
netstats <- readRDS("data/intermediate/estimates/netstats-local.rds")
est      <- readRDS("data/intermediate/estimates/netest-local.rds")

  #param
  prep_start <- 52 * 2
  param <- param.net(
    data.frame.params = readr::read_csv("data/input/params.csv"),
    netstats          = netstats,
    epistats          = epistats,
    prep.start        = prep_start,
    riskh.start       = prep_start - 53)
  #print(param)
  
  #init
  #Note: For models with bacterial STIs, these must be initialized here with non-zero values
  init <- init_msm(
    prev.ugc = 0.1,
    prev.rct = 0.1,
    prev.rgc = 0.1,
    prev.uct = 0.1)

  #control
  control <- control_msm(
    nsteps = 250,
    nsims = 1,
    ncores = 1)
  #print(control)

#Simulate epidemic and examine output 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #run 1 sim*********************
  sim <- netsim(est, param, init, control)

  print(sim)

  par(mar = c(3, 3, 2, 2), mgp = c(2, 1, 0))
  plot(sim, y = "i.num", main = "Prevalence")
  plot(sim, y = "ir100", main = "Incidence")

  # df <- as.data.frame(sim)
  # head(df)
  # tail(df)

  #run >1 sim *****************
  control <- control_msm(
    nsteps = 250,
    nsims = 2,
    ncores = 2)
  
  sim <- netsim(est, param, init, control)

  par(mfrow = c(2, 1))
  plot(sim, y = "i.num", main = "Prevalence")
  plot(sim, y = "ir100", main = "Incidence")

  
#De-bugging EMHIV-p in base model 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pkgload::load_all("C:/Users/Uonwubi/OneDrive - Emory University/Desktop/Personal/RSPH EPI Docs/RA2/GitRepos/EpiModelHIV-p")

  control <- control_msm(
    nsteps = 250,
    nsims = 1,
    ncores = 1,
  )

  debug(partident_msm) #replace module name
  sim <- netsim(est, param, init, control)
  undebug(partident_msm)



# NetSim on local: Testing Scenarios -----------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
#source("R/utils-0_project_settings.R")
context <- "local"

#Libraries
library("EpiModelHIV")

#Set up files 
#~~~~~~~~~~~~
source("R/utils-netsim_inputs.R") 
source("R/utils-targets.R")
source("R/utils-netsize.R")

  # epistats <- readRDS("data/input/epistats.rds")
  # netstats <- readRDS(paste0("data/input/netstats-", netsize_string, ".rds"))
  # est <- readRDS(paste0("data/input/netest-", netsize_string, ".rds"))
  
epistats <- readRDS("data/intermediate/estimates/epistats-local.rds")
netstats <- readRDS("data/intermediate/estimates/netstats-local.rds")
est      <- readRDS("data/intermediate/estimates/netest-local.rds")

  
  #edit times
  calibration_length <- 52 * 0
  restart_time       <- calibration_length + 1                       
  prep_start         <- restart_time + (52 * 0)
  interv_start       <- prep_start + (52 * 0.25)                       
  nsteps             <- interv_start + (52 * .25)
  
  #controls
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
  
  #set up scenarios
  scenarios_df <- tibble::tibble(
    .scenario.id = c("base","interv1"),#, "interv2", "both"),
    .at = 1,
    prevpos.retest.start	= c(interv_start,interv_start),#, Inf, interv_start),
    second.genps.start	= c(Inf,Inf)#, interv_start), interv_start)
  )
  scenarios_list <- EpiModel::create_scenario_list(scenarios_df)
  
#Simulate epidemic in scenarios and examine output 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    #Method 1
      #expect 4 files (2 per scenario)
      EpiModelHPC::netsim_scenarios(
        path_to_restart, param, init, control, scenarios_list,
        n_rep = 3,
        n_cores = 2,
        output_dir = "data/intermediate/scenarios",
        libraries = "EpiModelHIV",
        save_pattern = "simple"
      )
      list.files("data/intermediate/scenarios")
 
      
 
    #Method 2 (use to debug in scenario testing)
      options(error = recover)
      
      #set up a list to hold the sim results
        d_list <- vector(mode = "list",
                       length = length(scenarios_list))
        names(d_list) <- names(scenarios_list)
      
      #reload EMHIV-p package from local (if changes made locally)
        pkgload::load_all("C:/Users/Uonwubi/OneDrive - Emory University/Desktop/Personal/RSPH EPI Docs/RA2/GitRepos/EpiModelHIV-p")
      
        control <- control_msm(
          simno = 1,
          nsteps = nsteps,
          nsims = 1,
          ncores = 1,
          verbose = TRUE)
    
      #run netsim() looped over the scenarios
        #debug(hivtest_msm)
        #debug(partident_msm)
        #debug(hivtx_msm)
        options(error = recover)
        for (scenario in scenarios_list){
          print(scenario$id)
          sc.param <- use_scenario(param, scenario)
          sim <- netsim(est, sc.param, init, control)
      
          #convert sim object to a df
          d_sim <- as.data.frame(sim)
          d_sim[["scenario"]] <- scenario$id
          d_list[[scenario$id]] <-d_sim
        }
        # undebug(hivtest_msm)
        # undebug(partident_msm)
        # undebug(hivtx_msm)
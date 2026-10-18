# Setup ------------------------------------------------------------------------
context <- "local"
#source("R/utils-0_project_settings.R")

# Run the simulations ----------------------------------------------------------
library("EpiModelHIV")

# Necessary files
source("R/utils-netsim_inputs.R") # generate `path_to_restart`, `param`, `init`

# Controls
source("R/utils-targets.R")
control <- control_msm(
  start               = restart_time,
  nsteps              = nsteps,
  nsims               = 1,
  ncores              = 1,
  initialize.FUN      = reinit_msm,
  cumulative.edgelist = TRUE,
  truncate.el.cuml    = 0,
  .tracker.list       = calibration_trackers,
  verbose             = TRUE
)

# Using scenarios --------------------------------------------------------------
# scenarios_df <- readr::read_csv("./data/input/scenarios.csv")
# glimpse(scenarios_df)

scenarios_df <- tibble::tibble(
  .scenario.id = c("base", "interv1"),#, "interv2", "both"),
  .at = 1,
  prevpos.retest.start	= c(Inf, interv_start),#, Inf, interv_start),
  second.genps.start	= c(Inf, Inf)#, interv_start, interv_start)
)

scenarios_list <- EpiModel::create_scenario_list(scenarios_df)

# Here 2 scenarios will be used "baseline" and "scenario_1".
# This will generate 4 files (2 per scenarios)
EpiModelHPC::netsim_scenarios(
  path_to_restart, param, init, control, scenarios_list,
  n_rep = 3,
  n_cores = 2,
  output_dir = "data/intermediate/scenarios",
  libraries = "EpiModelHIV",
  save_pattern = "simple"
)
list.files("data/intermediate/scenarios")

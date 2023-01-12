##
## utils. Default Inputs for EpiModel::nestim
##

# Required variables: (provided by the calling script)
# - NETSIZE
# - netsize_string

# Setup ------------------------------------------------------------------------
suppressMessages({
  library("EpiModelHIV")
  library("EpiModelHPC")
})

epistats <- readRDS("data/input/epistats.rds")
netstats <- readRDS(paste0("data/input/netstats-", netsize_string, ".rds"))
path_to_est <- paste0("data/input/netest-", netsize_string, ".rds")
path_to_restart <- paste0("data/input/restart-hpc.rds")

# Relevant times
calibration_length <- 52 * 60
restart_time       <- calibration_length + 1                       #new
prep_start         <- restart_time + (52 * 5)
interv_start       <- prep_start + (52 * 10)                       #increased to 10. Impact?
nsteps             <- interv_start + (52 * 10)


# Parameters -------------------------------------------------------------------
csv_params <- paste0("data/input/params.csv")
df_params <- readr::read_csv(csv_params)

param <- param.net(
  data.frame.params = df_params,
  netstats = netstats,
  epistats = epistats,
  
  #other params
  part.ident.main.window = 52,                                                            
  part.ident.casl.window = 52,
  part.ident.ooff.window = 52,
  part.ident.main.prob   = 0.5,                                                           
  part.ident.casl.prob   = 0.5,
  part.ident.ooff.prob   = 0.5,
  part.index.prob        = 0.667,
  part.ppindex.prob      = 0.64,                                                         
  part.hiv.test.rate     = rep(0.394, 3),                                                  
  part.tx.init.rate      = rep(0.387, 3),
  part.index.window.int  = 0, 
  riskh.start = prep_start - 53,
  prep.start = prep_start,
  
  
  #interv start times
  part.ident.start    = prep_start,                                                        
  prevpos.retest.start = Inf,
  second.genps.start = Inf,
  
  #update param list
  .param.updater.list = list( # High PrEP intake for the first year only
    list(at = prep_start, param = list(
      prep.start.prob = function(x) plogis(qlogis(x) + log(2)))),
    list(at = prep_start + 52, param = list(
      prep.start.prob = function(x) plogis(qlogis(x) - log(2))))
  )
)

# Initial conditions -----------------------------------------------------------
# (default prevalence initialized in epistats)
# For models without bacterial STIs, these must be initialized here with non-zero values
init <- init_msm(
  prev.ugc = 0.1,
  prev.rct = 0.1,
  prev.rgc = 0.1,
  prev.uct = 0.1
)


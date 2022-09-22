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
est <- readRDS(paste0("data/input/netest-", netsize_string, ".rds"))

# Relevant times
calibration_length <- 52 * 10
prep_start         <- calibration_length + 52 * 5 + 1
interv_start       <- prep_start + 52 * 5
nsteps             <- interv_start + 52 * 10 - 1


# Parameters -------------------------------------------------------------------
csv_params <- paste0("data/input/params-", netsize_string, ".csv")
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
  riskh.start = prep_start - 53,
  prep.start = prep_start,
  part.index.window.int  = 0,                                                             
  
  #interv start times
  part.ident.start    = prep_start,                                                        
  prevpos.retest.start = Inf,
  second.genps.start = Inf,
)

# Initial conditions -----------------------------------------------------------
init <- init_msm()


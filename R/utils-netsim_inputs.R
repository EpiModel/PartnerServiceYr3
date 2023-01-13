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

est_dir <- "./data/intermediate/estimates/"
#diag_dir <- "./data/intermediate/diagnostics/"

#use post-calib networks estimation products
epistats <- readRDS(paste0(est_dir, "epistats-", context, ".rds"))
netstats <- readRDS(paste0(est_dir, "netstats-", context, ".rds"))
path_to_est <- paste0(est_dir, "netest-", context, ".rds")
path_to_restart <- paste0(est_dir, "restart-", context, ".rds")

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
  part.index.prob        = 0.666,
  part.ppindex.prob      = 0.64,                                                         
  part.hiv.test.rate     = rep(0.394, 3),                                                  
  part.tx.init.rate      = rep(0.387, 3),
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


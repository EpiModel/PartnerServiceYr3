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
calibration_length <- 52 * 60
prep_start         <- calibration_length + 52 * 5 + 1
interv_start       <- prep_start + 52 * 5
nsteps             <- interv_start + 52 * 10 - 1


# Parameters -------------------------------------------------------------------
csv_params <- paste0("data/input/params-", netsize_string, ".csv")
df_params <- readr::read_csv(csv_params)

param <- param.net(
  data.frame.params = df_params,
  part.ident.main.window = 24,                                                            #Num of ts that a main partner qualifies for partner identification (default=12wks)
  part.ident.casl.window = 24,
  part.ident.ooff.window = 24,
  part.ident.main.prob   = 0.5,                                                           #Probability that an elicited main partner is identified
  part.ident.casl.prob   = 0.5,
  part.ident.ooff.prob   = 0.5,
  part.hiv.test.rate     = rep(0.84, 3),                                                  #using param from complete case analysis in combprevnet (YR2 study)
  prevpos.retest.start = Inf,
  second.genps.start = Inf,
  part.index.prob        = 0.667,                                                         #Probability that an ND-index case would initiate PS
  part.ppindex.prob      = 0.667,                                                         #Probability that an PP-index case would initiate PS
  netstats = netstats,
  epistats = epistats,
  riskh.start = prep_start - 53,
  prep.start = prep_start
)

# Initial conditions -----------------------------------------------------------
init <- init_msm()


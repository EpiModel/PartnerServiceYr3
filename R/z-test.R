# Scratchpad for interactive testing before integration in a script

source("R/utils-0_project_settings.R")

# Libraries  -------------------------------------------------------------------
library("EpiModelHIV")
pkgload::load_all("../EpiModel.git/main")

# Necessary files
epistats <- readRDS("data/intermediate/estimates/epistats-local.rds")
netstats <- readRDS("data/intermediate/estimates/netstats-local.rds")
est      <- readRDS("data/intermediate/estimates/netest-local.rds")

prep_start <- 52 * 2
param <- param.net(
  data.frame.params = readr::read_csv("data/input/params.csv"),
  netstats          = netstats,
  epistats          = epistats,
  prep.start        = prep_start,
  riskh.start       = prep_start - 53
)

# Initial conditions (default prevalence initialized in epistats)
# For models with bacterial STIs, these must be initialized here with non-zero values
init <- init_msm(
  prev.ugc = 0.1,
  prev.rct = 0.1,
  prev.rgc = 0.1,
  prev.uct = 0.1
)

# Control settings
control <- control_msm(
  nsteps = 25,
  nsims = 2,
  ncores = 2,
  .traceback.on.error = TRUE,
  .dump.frame.on.error = TRUE,
  .control.updater.list = list(
    list(
      at = 10,
      control = list(
        save.nwstats = NA
      )
    )
  )
)

# Epidemic simulation
sim <- netsim(est, param, init, control)

# EpiModelHPC::netsim_scenarios(
#   path_to_est, param, init, control,
#   scenarios_list = NULL,
#   n_rep = 3,
#   n_cores = 2,
#   output_dir = "data/intermediate/no_scenario_test",
#   libraries = "EpiModelHIV",
#   save_pattern = "simple"
# )

load("./dump_20230203_102912_6.rda")
 debugger()

  found.uid.nd <- hivpos.uid[runif(length(hivpos.uid)) < part.index.prob]
  hivpos_pel <- hivpos_pel[hivpos_pel[["index"]] %in% found.uid.nd, ]

  found.uid.pp <- retests.uid[runif(length(retests.uid)) < 1]
  retests_pel <- retests_pel[retests_pel[["index"]] %in% found.uid.pp, ]

  dat <- set_epi(dat, "found.indexes.nd", at, length(found.uid.nd))
  dat <- set_epi(dat, "found.indexes.pp", at, length(found.uid.pp))

      #update found pp indexes (for tx check and restart)
      if (length(found.uid.pp)>0) {
        found.uid.pp <- EpiModel::get_posit_ids(dat, unique(found.uid.pp))
        dat <- set_attr(dat, "foundPrevPos.ident", at, posit_ids = found.uid.pp)
        dat <- set_epi(dat, "found.indexes.pp.un", at, length(found.uid.pp))
      }

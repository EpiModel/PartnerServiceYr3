##
## 14. Epidemic Model Parameter Calibration, Best sim
##
#

# Setup ------------------------------------------------------------------------
suppressMessages({
  library(EpiModel)
  library(dplyr)
  library(tidyr)
})

source("R/utils-netsize.R")
source("R/utils-targets.R")

d <- readRDS("data/output/calib/assessments_persim.rds")

for (nm in names(targets)) {
  d[[nm]] <- d[[nm]] - targets[[nm]]
}

d <- d %>%
  select(-c(ir100.gc, ir100.ct, prep_prop)) %>%
  select(batch, sim, scenario_name, everything())

d_mat <- as.matrix(d[, - c(1:3)])

d$score <- apply(d_mat, 1, function(x) sum(x^2))

d %>%
  arrange(score) %>%
  head(1) %>%
  select(batch, sim)

# get batch N from HPC
# rsync -v rsph:projects/BigNets/data/output/calib/sim__choose_restart__50.rds data/output/calib
sims <- readRDS("data/output/calib/sim__choose_restart__49.rds")
sim <- get_sims(sims, 30)

saveRDS(sim, paste0("data/input/restart-", netsize_string, ".rds"))


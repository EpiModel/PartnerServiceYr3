##
## 22. Epidemic Model Restart Point, Choice of the best restart point
##

# Setup ------------------------------------------------------------------------
# Choose the right context: "local" if you are working with smaller networks
#   estimated locally or "hpc" for the full size networks. For "hpc", this
#   assumes that you downloaded the "assessments_raw.rds" files from the HPC.
context <- c("local", "hpc")[2]
source("R/utils-0_project_settings.R")

# Libraries  -------------------------------------------------------------------
library("EpiModel")
library("dplyr")
library("tidyr")
source("R/utils-default_inputs.R")

d <- readRDS("data/intermediate/calibration/assessments_raw.rds")

source("R/utils-targets.R")
nm_targets <- intersect(names(targets), names(d))
for (nme in nm_targets) {
  d[[nme]] <- d[[nme]] - targets[[nme]]
}

# Calculate RMSE
mat_d <- as.matrix(select(d, - c(batch_number, sim, scenario_name)))

d$scores <- apply(mat_d, 1, function(x) {
  sum(x^2, na.rm = TRUE)
})

# pick best sim
best_sim <- d %>%
  arrange(scores) %>%
  select(batch_number, sim) %>%
  head(1)

# Check the values manually
d %>%
  arrange(scores) %>%
  head(1) %>%
  as.list()

# Get best sim
best <- readRDS(fs::path(
  "data/intermediate/calibration",
  paste0("sim__empty_scenario__", best_sim$batch_number, ".rds"))
)

best <- EpiModel::get_sims(best, best_sim$sim)
epi_num <- best$epi$num

# Remove all epi except `num` to lighten the file
best$epi <- list(num = epi_num)

# `path_to_restart` is computed in "./R/utils-default_inputs.R" and depend on
# `context` being setup properly
saveRDS(best, path_to_restart)

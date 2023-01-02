##
## 00. Shared variables setup
##

est_dir <- "./data/intermediate/estimates/"
diag_dir <- "./data/intermediate/diagnostics/"

# Information for the HPC workflows
current_git_branch <- "alg_calib"
mail_user <- "aleguil@emory.edu"

# Relevant time steps for the simulation
calibration_end    <- 52 * 60
restart_time       <- calibration_end + 1
prep_start         <- restart_time + 52 * 5
intervention_start <- prep_start + 52 * 10 # back to 5 ? check
intervention_end   <- intervention_start + 52 * 10

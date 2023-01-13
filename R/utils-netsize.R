##
##  Shared variables used all over the project
##
##  The variables should be changed here only to avoid discrepancies in the
##  different scripts
##

# # Size of network to use
# if (! exists("NETSIZE")) {
#   NETSIZE <- 100 * 1e3
# }

if (context == "hpc") {
  networks_size   <- 100 * 1e3
  NETSIZE <- 100 *1e3
  estimation_method <- "MCMLE"
  estimation_ncores <- 10
} else if (context == "local") {
  networks_size   <- 10 * 1e3
  NETSIZE <- 10 *1e3
  estimation_method <- "Stochastic-Approximation"
  estimation_ncores <- 1
} else {
  stop("The `context` variable must be set to either 'local' or 'hpc'")
}
netsize_string <- format(NETSIZE, scientific = FALSE)
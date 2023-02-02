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
  networks_size   <- 10 * 1e3
  estimation_ncores <- 10
  max_cores<-32
  } else if (context == "local") {
  networks_size   <- 10 * 1e3
  max_cores <- 2
  } else {
  stop("The `context` variable must be set to either 'local' or 'hpc'")
}
netsize_string <- format(networks_size, scientific = FALSE)
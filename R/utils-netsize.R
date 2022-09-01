##
##  Shared variables used all over the project
##
##  The variables should be changed here only to avoid discrepancies in the
##  different scripts
##

# Size of network to use
if (! exists("NETSIZE")) {
  NETSIZE <- 1 * 1e5
}

netsize_string <- format(NETSIZE, scientific = FALSE)

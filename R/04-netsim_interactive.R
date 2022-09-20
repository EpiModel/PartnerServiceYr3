
##
## 04. Small-scale epidemic simulation for testing/debugging
##

## Packages
suppressMessages(library("EpiModelHIV"))
suppressMessages(library("EpiModelHPC"))

# Load the `NETSIZE` value and the formatted `netsize_string`
# NETSIZE <- 1e4     # to override (before sourcing the file)
source("R/utils-netsize.R")

## Parameters
epistats <- readRDS("data/input/epistats.rds")
netstats <- readRDS(paste0("data/input/netstats-", netsize_string, ".rds"))
est <- readRDS(paste0("data/input/netest-", netsize_string, ".rds"))

param <- param_msm(
  netstats               = netstats,
  epistats               = epistats,
  a.rate                 = 0.00049,
  hiv.test.rate          = c(0.00385, 0.00380, 0.00690),
  tx.init.rate           = c(0.1775, 0.190, 0.2521),
  tx.halt.partial.rate   = c(0.0062, 0.0055, 0.0031),
  tx.reinit.partial.rate = c(0.00255, 0.00255, 0.00255),
  hiv.trans.scale        = c(2.44, 0.424, 0.270),
  riskh.start            = 52,
  prep.start             = 52,
  prep.start.prob        = rep(0.66, 3),

  #partner identification params at baseline
  part.ident.start       = 1*52+1,                                                        #turn on partner identification to start 1 year after network initialization (default=Inf i.e. no partner identification)
  part.index.window.int  = 0,                                                             #Num. of prior ts that a ND case is eligible for partner notification (set to 1 ?to be considered if ND in prior time step?)
  part.index.prob        = 0.667,                                                         #Probability that an index case would initiate PS
  part.ident.main.window = 24,                                                            #Num of ts that a main partner qualifies for partner identification (default=12wks)
  part.ident.casl.window = 24,
  part.ident.ooff.window = 24,
  part.ident.main.prob   = 0.5,                                                           #Probability that an elicited main partner is identified
  part.ident.casl.prob   = 0.5,
  part.ident.ooff.prob   = 0.5,
  part.hiv.test.rate     = rep(0.84, 3),                                                  #using param from complete case analysis in combprevnet (YR2 study)
  prevpos.retest.start   = 1*52+1,                                                        #New parameter to set start time to PP retesting
  second.genps.start     = 2*52+1,
  part.ppindex.prob      = 0.667                                                          #Probability that a PP index would initiate PS
)

init <- init_msm()

pkgload::load_all("C:/Users/Uonwubi/OneDrive - Emory University/Desktop/Personal/RSPH EPI Docs/RA2/GitRepos/EpiModelHIV-p")

#debug(control_msm)
control <- control_msm(
  simno = 1,
  nsteps = 3*52,                                                                          #i.e. 4 years (1 without PS and 3 with)
  nsims = 1,
  ncores = 5,
  verbose = TRUE
)

#debug(partident_msm)
#debug(hivtest_msm)

sim <- netsim(est, param, init, control)
#undebug(partident_msm)
#undebug(hivtest_msm)


# #param object to test use_scenario() function
# param <- param_msm(
#   netstats               = netstats,
#   epistats               = epistats,
#   a.rate                 = 0.00049,
#   hiv.test.rate          = c(0.00385, 0.00380, 0.00690),
#   tx.init.rate           = c(0.1775, 0.190, 0.2521),
#   tx.halt.partial.rate   = c(0.0062, 0.0055, 0.0031),
#   tx.reinit.partial.rate = c(0.00255, 0.00255, 0.00255),
#   hiv.trans.scale        = c(2.44, 0.424, 0.270),
#   riskh.start            = 52,                                                  
#   prep.start             = 52,
#   prep.start.prob        = rep(0.66, 3),
#   
#   #partner identification params at baseline
#   part.index.window.int  = 0,                                                             
#   part.index.prob        = 0.667,                                                         
#   part.ident.main.window = 24,                                                            
#   part.ident.casl.window = 24,
#   part.ident.ooff.window = 24,
#   part.ident.main.prob   = 0.5,                                                           
#   part.ident.casl.prob   = 0.5,
#   part.ident.ooff.prob   = 0.5,
#   part.hiv.test.rate     = rep(0.84, 3),
#   part.ppindex.prob      = 0.667,                                                         
#   
#   #start times
#   part.ident.start       = 1*52+1,                                                        
#   prevpos.retest.start   = Inf,                                                        
#   second.genps.start     = Inf
# )
# 
# #set up scenarios
# interv_start       <- 52 + 52 * 1
# scenarios.df <- tibble::tibble(
#   .scenario.id = c("base", "interv1", "interv2", "both"),
#   .at = 1,
#   prevpos.retest.start	= c(Inf, interv_start, Inf, interv_start),
#   second.genps.start	= c(Inf, Inf, interv_start, interv_start)
# )
# scenarios.list <- EpiModel::create_scenario_list(scenarios.df)
# 
# #set up a list to hold the sim results
# d_list <- vector(mode = "list",
#                length = length(scenarios.list))
# names(d_list) <- names(scenarios.list)
# 
# #set up init, control, update EMHIHp and run netsim()
# init <- init_msm()
# pkgload::load_all("C:/Users/Uonwubi/OneDrive - Emory University/Desktop/Personal/RSPH EPI Docs/RA2/GitRepos/EpiModelHIV-p")
# control <- control_msm(
#   simno = 1,
#   nsteps = 3*52,                                                                          
#   nsims = 1,
#   ncores = 5,
#   verbose = TRUE
# )
# 
# #run netsim() looped over the scenarios
# for (scenario in scenarios.list){
#   print(scenario$id)
#   sc.param <- use_scenario(param, scenario)
#   sim <- netsim(est, sc.param, init, control)
#   
#   #convert sim object to a df
#   d_sim <- as.data.frame(sim)
#   d_sim[["scenario"]] <- scenario$id
#   d_list[[scenario$id]] <-d_sim
# }
#  



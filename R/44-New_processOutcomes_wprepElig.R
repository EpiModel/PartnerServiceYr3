#
#
# Adding eligible screened negatives to psy table2


#set up processed output datasets --------------------------------------------------------
rm(list = ls())


#load libraries
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyr")
  library("dplyr")
  library("ggplot2")
})


#set context (tells where to pull scenarios files from)
context<-"hpc" 


#set output file dir
dir <- paste0("C:/Users/Uonwubi/OneDrive - Emory University/Desktop/Personal/RSPH EPI Docs/RA2/GitRepos/PartnerServicesYr3/data/intermediate/",context,"/processed")


#get data
tbl2ab_fulldata<-readRDS(paste(dir,'/tbl2ab_fulldata.rds', sep=""))
tbl2cd_fulldata<-readRDS(paste(dir,'/tbl2cd_fulldata.rds', sep=""))
tbl3_fulldata<-readRDS(paste(dir,'/tbl3_fulldata.rds', sep=""))

#source function file
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")
source("R/utils-tbl_output_functions.R")


#B. Process outcome_sims data---------------------------------------------------
getSims_tb2 <- function(dat){
  simdat<-get_outcome_sims_tbl2(dat) %>% 
    select(tbl, scenario.num, scenario.new, scenario_name,sim,
           ir.yr10, incid.cum, nia, pia, nnt,
           prepCov.yr10, diagCov.yr10, artCov.yr10, vSuppCov.yr10,
           prepStartAll,
           elig.indexes.all, found.indexes.all, prp.indexes.found.all,
           elig.partners.all, found.partners.all, prp.partners.found.all,
           partners.per.index, 
           tot.tests.pbt, positive.part, negative.part,
           elig.prepStartPart, prepStartPart, part.start.tx, part.reinit.tx, pp.reinit.tx)
  return(simdat)
}

outcomes_sims_tbl2ab <- getSims_tb2(tbl2ab_fulldata)
outcomes_sims_tbl2cd <- getSims_tb2(tbl2cd_fulldata)


getSims_tbl3 <- function(dat){
  simdat <- get_outcome_sims_tbl3(dat) %>% 
    select(tbl, scenario.num, scenario.new, scenario_name,sim,
           ir.yr10, incid.cum, nia, pia, nnt,
           prepCov.yr10, diagCov.yr10, artCov.yr10, vSuppCov.yr10,
           prepStartAll,
           elig.indexes.all, found.indexes.all, prp.indexes.found.all,
           elig.partners.all, found.partners.all, prp.partners.found.all,
           partners.per.index, 
           tot.tests.pbt, positive.part, negative.part,
           part.scrnd, scrnd.noprep, scrnd.prepon, scrnd.noprepnorisk,
           elig.prepStartPart, prepStartPart, part.start.tx, part.reinit.tx, pp.reinit.tx)
}

outcomes_sims_tbl3 <- getSims_tbl3(tbl3_fulldata) 


#C. Process outcome_scenario data----------------------------------------------
getSce <- function(dat){
  scedat <- dat %>%
    select(- c(sim)) %>%
    group_by(tbl, scenario.num, scenario.new, scenario_name) %>%
    summarise(across(everything(),list(
      low = ~ quantile(.x, 0.025, na.rm = TRUE),
      med = ~ quantile(.x, 0.50, na.rm = TRUE),
      high = ~ quantile(.x, 0.975, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}")
      ) %>% 
    mutate(across(where(is.numeric), ~round (., 5))) %>% 
    ungroup()%>% 
    arrange(tbl, scenario.num, scenario.new, scenario_name)
}


outcomes_scenarios_tbl2ab <- getSce(outcomes_sims_tbl2ab) 
outcomes_scenarios_tbl2cd <- getSce(outcomes_sims_tbl2cd) 
outcomes_scenarios_tbl3 <- getSce(outcomes_sims_tbl3) 


saveRDS(outcomes_sims_tbl2ab, paste0("data/intermediate/",context,"/processed_eligprep/tbl2ab_outcomes_sims.rds"))
saveRDS(outcomes_sims_tbl2cd, paste0("data/intermediate/",context,"/processed_eligprep/tbl2cd_outcomes_sims.rds"))
saveRDS(outcomes_sims_tbl3, paste0("data/intermediate/",context,"/processed_eligprep/tbl3_outcomes_sims.rds"))

saveRDS(outcomes_scenarios_tbl2ab, paste0("data/intermediate/",context,"/processed_eligprep/tbl2ab_outcomes_scenarios.rds"))
saveRDS(outcomes_scenarios_tbl2cd, paste0("data/intermediate/",context,"/processed_eligprep/tbl2cd_outcomes_scenarios.rds"))
saveRDS(outcomes_scenarios_tbl3, paste0("data/intermediate/",context,"/processed_eligprep/tbl3_outcomes_scenarios.rds"))

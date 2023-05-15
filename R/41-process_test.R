##
## 11. Epidemic Model Parameter Calibration, Processing of the simulation files
##
context<-"hpc"

# Setup ------------------------------------------------------------------------
#get context and scenarios functions
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")
source("R/utils-output_functions0.R")



#get batch info
batches_infos <- EpiModelHPC::get_scenarios_batches_infos(
  paste0("data/intermediate/",context,"/scenarios_test")
)




#A. Process intervdata ------------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyr")
  library("dplyr")
  library("ggplot2")
})

sims_dir <- paste0("data/intermediate/",context,"/scenarios_test")


#get sim files
sim_files <- list.files(
  sims_dir,
  pattern = "^sim__.*rds$",
  full.names = TRUE
)


#Process each batch in parallel 
intervds <- future.apply::future_lapply(
  sim_files,
  process_fulldata,
  ts = interv_start - (5 * 52) + 1   #gets data from 5 years prior to intervention start
)


#Merge all batches  
full_intervdata <- bind_rows(intervds)



#B. Process outcome_sims and outcome_scenario data----------------------------------------
outcomes_sims <- get_outcome_sims_tbl2(full_intervdata) %>% 
  select(scenario.num, scenario.new, scenario_name,sim,
         ir.yr10, incid.cum, nia, pia, nnt,
         prepCov.yr10, diagCov.yr10, artCov.yr10, vSuppCov.yr10,
         prepStartAll,
         elig.indexes.all, found.indexes.all, prp.indexes.found.all,
         elig.partners.all, found.partners.all, prp.partners.found.all,
         partners.per.index, 
         tot.tests.pbt, positive.part, negative.part, 
         part.scrnd, scrnd.noprep, scrnd.prepon, scrnd.noprepnorisk,
         elig.prepStartPart, prepStartPart, part.start.tx, part.reinit.tx, pp.reinit.tx)


outcomes_scenarios <- outcomes_sims %>%
  select(- c(sim)) %>%
  group_by(scenario.num, scenario.new, scenario_name) %>%
  summarise(across(everything(),list(
    low = ~ quantile(.x, 0.025, na.rm = TRUE),
    med = ~ quantile(.x, 0.50, na.rm = TRUE),
    high = ~ quantile(.x, 0.975, na.rm = TRUE)
  ),
  .names = "{.col}__{.fn}"
  )) %>% 
  mutate(across(where(is.numeric), ~round (., 5))) %>% ungroup()%>% 
  arrange(scenario.num, scenario.new, scenario_name)





#Save the processed data
saveRDS(outcomes_sims, paste0("data/intermediate/",context,"/processed_test/outcomes_sims.rds"))
saveRDS(outcomes_scenarios, paste0("data/intermediate/",context,"/processed_test/outcomes_scenarios.rds"))
saveRDS(full_intervdata, paste0("data/intermediate/",context,"/processed_test/fulldata.rds"))

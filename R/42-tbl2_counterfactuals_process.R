##
## 42. Processing counterfactual scenarios
##
context<-"hpc"

# Setup ------------------------------------------------------------------------
#get context and scenarios functions
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")
source("R/utils-tbl_output_functions.R")



#get batch info
batches_infos <- EpiModelHPC::get_scenarios_batches_infos(
  paste0("data/intermediate/",context,"/scenarios_tbl2")
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

sims_dir <- paste0("data/intermediate/",context,"/scenarios_tbl2")


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
full_intervdata_tbl2 <- bind_rows(intervds)



#B. Process outcome_sims and outcome_scenario data----------------------------------------
outcomes_sims_tbl2 <- get_outcome_sims_tbl2(full_intervdata_tbl2) %>% 
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


outcomes_scenarios_tbl2 <- outcomes_sims_tbl2 %>%
  select(- c(sim)) %>%
  group_by(tbl, scenario.num, scenario.new, scenario_name) %>%
  summarise(across(everything(),list(
    low = ~ quantile(.x, 0.025, na.rm = TRUE),
    med = ~ quantile(.x, 0.50, na.rm = TRUE),
    high = ~ quantile(.x, 0.975, na.rm = TRUE)
  ),
  .names = "{.col}__{.fn}"
  )) %>% 
  mutate(across(where(is.numeric), ~round (., 5))) %>% ungroup()%>% 
  arrange(tbl, scenario.num, scenario.new, scenario_name)





#Save the processed data
saveRDS(outcomes_sims_tbl2, paste0("data/intermediate/",context,"/processed/tbl2_outcomes_sims.rds"))
saveRDS(outcomes_scenarios_tbl2, paste0("data/intermediate/",context,"/processed/tbl2_outcomes_scenarios.rds"))
#readr::write_csv(outcomes_scenarios_med95si, "data/intermediate/processed/outcomes_scenarios.csv")
saveRDS(full_intervdata_tbl2, paste0("data/intermediate/",context,"/processed/tbl2_fulldata.rds"))





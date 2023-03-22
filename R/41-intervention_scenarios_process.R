##
## 11. Epidemic Model Parameter Calibration, Processing of the simulation files
##
context<-"hpc"

# Setup ------------------------------------------------------------------------
#get context and scenarios functions
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")
source("R/utils-scenarios_outcomes.R")



#get batch info
batches_infos <- EpiModelHPC::get_scenarios_batches_infos(
  paste0("data/intermediate/",context,"/scenarios")
)




#A. Process intervdata ------------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyr")
  library("dplyr")
  library("ggplot")
})

sims_dir <- paste0("data/intermediate/",context,"/scenarios")


#get sim files
sim_files <- list.files(
  sims_dir,
  pattern = "^sim__.*rds$",
  full.names = TRUE
)


#Process each batch in parallel 
intervds <- future.apply::future_lapply(
  sim_files,
  process_plotdat,
  ts = interv_start - (5 * 52) + 1   #gets data from 5 years prior to intervention start
)


#Merge all batches  
intervdata <- bind_rows(intervds)



#B. Process outcome_sims and outcome_scenario data----------------------------------------
outcomes_sims <- get_outcome_sims(intervdata)
outcomes_scenarios <- outcomes_sims %>%
  select(- c(sim)) %>%
  group_by(scenario_name, scenario.new) %>%
  summarise(across(everything(),list(
    low = ~ quantile(.x, 0.025, na.rm = TRUE),
    med = ~ quantile(.x, 0.50, na.rm = TRUE),
    high = ~ quantile(.x, 0.975, na.rm = TRUE)
  ),
  .names = "{.col}__{.fn}"
  )) %>% 
  mutate(across(where(is.numeric), ~round (., 2))) %>% ungroup()%>% arrange(scenario.new)




#Save the processed data
saveRDS(outcomes_sims, paste0("data/intermediate/",context,"/processed/outcomes_sims.rds"))
saveRDS(outcomes_scenarios, paste0("data/intermediate/",context,"/processed/outcomes_scenarios.rds"))
#readr::write_csv(outcomes_scenarios_med95si, "data/intermediate/processed/outcomes_scenarios.csv")
saveRDS(intervdata, paste0("data/intermediate/",context,"/processed/allscenarios.rds"))





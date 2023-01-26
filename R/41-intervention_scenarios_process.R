##
## 11. Epidemic Model Parameter Calibration, Processing of the simulation files
##


# Setup ------------------------------------------------------------------------
#get context and scenarios functions
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")
source("R/utils-scenarios_outcomes.R")



#get batch info
batches_infos <- EpiModelHPC::get_scenarios_batches_infos(
  paste0("data/intermediate/",context,"/scenarios")
)



#get cumm and yr10 outcomes per sim
outcomes_raw <- lapply(
  seq_len(nrow(batches_infos)),
  function(i) process_one_scenario_batch(batches_infos[i, ]))



#bind cumm metrics from sim batches in one data frame 
outcomes_sims <- bind_rows(outcomes_raw) %>%  
  group_by(scenario_name) %>%               #re-number sims for each scenario
  arrange(batch_number) %>% 
  mutate(sim2 = sim) %>% 
  mutate(sim = row_number()) %>% 
  select(-sim2)  %>% 
  #rename scenarios
  mutate(scenario.new = ifelse(scenario_name == "base","Base",
                             ifelse(scenario_name == "interv1","+ PP retests",
                                    ifelse(scenario_name == "interv2","+ Wave 2 PS","+ Both")))) %>% 
  mutate(scenario.new = factor(scenario.new, levels = c("Base","+ PP retests","+ Wave 2 PS","+ Both"))) %>% 
  ungroup() %>% 
  arrange(scenario.new, batch_number, sim)

saveRDS(outcomes_sims, paste0("data/intermediate/",context,"/processed/outcomes_sims.rds"))



# get cumm and yr10 outcomes per scenario with median, 50% SI summaries
outcomes_scenarios <- outcomes_sims %>%
  select(- c(sim, batch_number)) %>%
  group_by(scenario_name, scenario.new) %>%
  summarise(across(everything(),list(
      low = ~ quantile(.x, 0.025, na.rm = TRUE),
      med = ~ quantile(.x, 0.50, na.rm = TRUE),
      high = ~ quantile(.x, 0.975, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) %>% ungroup()%>% arrange(scenario.new)

saveRDS(outcomes_scenarios, paste0("data/intermediate/",context,"/processed/outcomes_scenarios.rds"))
#readr::write_csv(outcomes_scenarios_med95si, "data/intermediate/processed/outcomes_scenarios.csv")




# Process plot data ------------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyverse")
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



# Merge all batches  
intervdata <- bind_rows(intervds)
saveRDS(intervdata, paste0("data/intermediate/",context,"/processed/allscenarios.rds"))



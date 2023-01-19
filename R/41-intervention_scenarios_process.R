##
## 11. Epidemic Model Parameter Calibration, Processing of the simulation files
##
#context<-"hpc"
context <- if (interactive()) "local" else "hpc"

# Setup ------------------------------------------------------------------------
#source("R/utils-0_project_settings.R")
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")
source("R/utils-scenarios_outcomes.R")

#get batch info
batches_infos <- EpiModelHPC::get_scenarios_batches_infos(
  "data/intermediate/scenarios"
)



#process each batch of simulation in parallel
outcomes_raw <- lapply(
  seq_len(nrow(batches_infos)),
  function(i) process_one_scenario_batch(batches_infos[i, ])
)



#bind all batches (and simulation rows) into 1 data frame 
#output=all scenarios with 1 row per unique simulation
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
saveRDS(outcomes_sims, "data/intermediate/processed/outcomes_sims.rds")



#summarise over simulations per scenario using median, 50% SI
outcomes_scenarios <- outcomes_sims %>%
  select(- c(sim, batch_number)) %>%
  group_by(scenario_name, scenario.new) %>%
  summarise(across(everything(),list(
      low = ~ quantile(.x, 0.25, na.rm = TRUE),
      med = ~ quantile(.x, 0.50, na.rm = TRUE),
      high = ~ quantile(.x, 0.75, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) %>% ungroup()%>% arrange(scenario.new)



# Save the result --------------------------------------------------------------
saveRDS(outcomes_scenarios, "data/intermediate/processed/outcomes_scenarios.rds")
#readr::write_csv(outcomes_scenarios_med95si, "data/intermediate/processed/outcomes_scenarios.csv")



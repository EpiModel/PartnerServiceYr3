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
  paste0("data/intermediate/",context,"/figdata")
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

sims_dir <- paste0("data/intermediate/",context,"/figdata")


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
outcomes_sims <- get_outcome_sims_tbl3(full_intervdata) %>% 
  select(tbl, scenario.num, scenario.new, scenario_name, sim,
         pia)
saveRDS(outcomes_sims, paste0("data/intermediate/",context,"/processed/figdata_outcomes_sims.rds"))


outcomes_scenarios<- outcomes_sims%>%
  select(- c(sim)) %>%
  group_by(tbl, scenario.num, scenario.new, scenario_name) %>%
  summarise(across(everything(),list(
    #low = ~ quantile(.x, 0.025, na.rm = TRUE),
    med = ~ quantile(.x, 0.50, na.rm = TRUE),
    #high = ~ quantile(.x, 0.975, na.rm = TRUE)
  ),
  .names = "{.col}"
  )) %>% 
  mutate(across(where(is.numeric), ~round (., 6))) %>% ungroup()%>% 
  arrange(tbl, scenario.num, scenario.new, scenario_name)
saveRDS(outcomes_scenarios, paste0("data/intermediate/",context,"/processed/figdata_outcomes_scenarios.rds"))





# #C. Get contour plot data  ---------------------------------------------------------------
# #recreate grid
# xgrid <- seq(0.65, 1, by=0.05)
# ygrid <- seq(0.65, 1, by=0.05)
# sim.grid <-expand.grid(part.index.prob = xgrid, 
#                        part.ppindex.prob = ygrid) %>% 
#   mutate(num = as.character(row_number())) %>% 
#   mutate(zers = sprintf("%03d",as.numeric(num)) ) %>% 
#   mutate(scenario.id.base = paste0("b",zers, sep=""), .before = "index.nd") %>% 
#   mutate(scenario.id.max = paste0("e",zers, sep=""), .before = "index.nd") %>% 
#   select(-c(num, zers))





#Save the processed data
#saveRDS(full_intervdata, paste0("data/intermediate/",context,"/processed/figdata_fulldata.rds"))





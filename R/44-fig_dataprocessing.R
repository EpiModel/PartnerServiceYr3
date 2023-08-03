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
  select(tbl, scenario.num, scenario_name, sim,
         incid.cum, pia) %>% 
  mutate(scenario.new = stringr::str_split_i(scenario_name, "_", 1),
         part.index.prob = as.numeric(stringr::str_split_i(scenario_name, "_", 2)),
         part.ppindex.prob = as.numeric(stringr::str_split_i(scenario_name, "_", 3)),
         tblname = as.numeric(stringr::str_split_i(scenario_name, "_", 4)))

tblnam <- outcomes_sims$tblname[2]
saveRDS(outcomes_sims, paste0("data/intermediate/",context,"/processed/figdata_outcomes_sims_",tblnam,".rds"))




outcomes_scenarios<- outcomes_sims%>%
  select(- c(sim)) %>%
  group_by(tbl, scenario.num, scenario.new, scenario_name) %>%
  summarise(across(everything(),list(
    #low = ~ quantile(.x, 0.025, na.rm = TRUE),
    med = ~ quantile(.x, 0.50, na.rm = TRUE)
    #high = ~ quantile(.x, 0.975, na.rm = TRUE)
    ),
  .names = "{.col}")) %>% 
  mutate(across(where(is.numeric), ~round (., 6))) %>% ungroup()%>% 
  mutate(part.index.prob = as.numeric(stringr::str_split_i(scenario_name, "_", 2)),
         part.ppindex.prob = as.numeric(stringr::str_split_i(scenario_name, "_", 3)),
         tblname = as.numeric(stringr::str_split_i(scenario_name, "_", 4))) %>% 
  arrange(tbl, scenario.num, scenario.new, scenario_name, tblname)

saveRDS(outcomes_scenarios, paste0("data/intermediate/",context,"/processed/figdata_outcomes_scenarios_", tblnam,".rds"))





#C. Get contour plot data  ---------------------------------------------------------------
#newdata (wider grid)
xgrid <- seq(min(outcomes_scenarios$part.index.prob), max(outcomes_scenarios$part.index.prob), length.out = 100)
ygrid <- seq(min(outcomes_scenarios$part.ppindex.prob), max(outcomes_scenarios$part.ppindex.prob), length.out = 100)
loepreddat <- expand.grid(part.index.prob = xgrid, part.ppindex.prob = ygrid)

#run loess models on scenarios data 
loe.fit <- loess(pia ~ part.index.prob * part.ppindex.prob, outcomes_scenarios)

#predict pia in newdata
loepreddat$pia <- as.numeric(predict(loe.fit, newdata = loepreddat))
loepreddat$tbl <- tblnam

saveRDS(loepreddat, paste0("data/intermediate/",context,"/processed/figdata_loepreddat_", tblnam,".rds"))


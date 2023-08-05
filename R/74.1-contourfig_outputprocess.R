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
save_dir <- paste0("data/intermediate/",context,"processed")

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
sims.out <- get_outcome_sims_tbl3(full_intervdata) %>% 
  select(tbl, scenario.num, scenario_name, sim,
         incid.cum, pia) %>% 
  mutate(scenario.new = stringr::str_split_i(scenario_name, "_", 1),
         index_inits = as.numeric(stringr::str_split_i(scenario_name, "_", 2)),
         partner_idents = as.numeric(stringr::str_split_i(scenario_name, "_", 3)),
         paramvalues = as.numeric(stringr::str_split_i(scenario_name, "_", 4)))

tbl <- sims.out$tbl[2]
paramvalues <- sims.out$paramvalues[2]
saveRDS(sims.out, paste0(save_dir, "/figdt_simsout_",tbl,"_",paramvalues,".rds"))

scen.out <- sims.out%>%
  select(- c(sim)) %>%
  group_by(tbl, scenario.num, scenario.new, scenario_name) %>%
  summarise(across(everything(),list(
    med = ~ quantile(.x, 0.50, na.rm = TRUE)
    ),
  .names = "{.col}")) %>% 
  mutate(across(where(is.numeric), ~round (., 6))) %>% ungroup()%>% 
  mutate(index_inits = as.numeric(stringr::str_split_i(scenario_name, "_", 2)),
         partner_idents = as.numeric(stringr::str_split_i(scenario_name, "_", 3)),
         paramvalues = as.numeric(stringr::str_split_i(scenario_name, "_", 4))) %>% 
  arrange(tbl, scenario.num, scenario.new, scenario_name, tblname)

saveRDS(scen.out, paste0(save_dir, "/figdt_scenout_",tbl,"_",paramvalues,".rds"))





#C. Get contour plot data  ---------------------------------------------------------------
griddat <- expand.grid(list(
  index_inits = seq(min(scen.out$index_inits), max(scen.out$index_inits), length.out = 100),
  partner_idents = seq(min(scen.out$partner_idents), max(scen.out$partner_idents), length.out = 100)
  )
)
  
loe.fit <- loess(pia ~ index_inits * partner_idents, scen.out)

griddat$pia <- as.numeric(predict(loe.fit, newdata = griddat))
griddat$tbl <- tblnam
griddat$paramvalues <- paramvalues

saveRDS(griddat, paste0(save_dir, "/figdt_griddat_", tbl,"_",paramvalues,".rds"))


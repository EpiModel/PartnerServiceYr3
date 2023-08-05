##
## 42. Processing counterfactual scenarios
##


# Setup ----------------------------------------------------------------------------------
context<-"hpc"

source("R/utils-tbl_output_functions.R")


sims_dir <- paste0("data/intermediate/",context,"/scenarios_tbl3")
save_dir <- paste0("data/intermediate/",context,"/processed")


#batch info
batches_infos <- EpiModelHPC::get_scenarios_batches_infos(
  paste0("data/intermediate/",context,"/scenarios_tbl3")
)




#A. Process intervdata -------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyr")
  library("dplyr")
  library("ggplot2")
})


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
  ts = 3901 - (5 * 52) + 1    #gets data from 5 years prior to intervention start
)


#Merge all batches  
fulldata_tbl3 <- bind_rows(intervds)

tblnam <- fulldata_tbl3$tbl[2]

saveRDS(fulldata_tbl3, paste0(save_dir, "/tbl3", tblnam, "_fulldata.rds"))




#B. Process outcome_sims and outcome_scenario data----------------------------------------
base_incid <- get_cumulative_outcomes(fulldata_tbl3) %>% 
  filter(tbl == "A" & scenario_name == "a001base")
saveRDS(base_incid, paste0(save_dir, "/baseincid/tbl3", tblnam, "_baseincid.rds"))



outcomes_sims_tbl3 <- get_outcome_sims_tbl3(fulldata_tbl3) %>% 
  select(tbl, scenario.num, scenario.new, scenario_name,sim,
         ir.yr10, incid.cum, nia, pia, nnt,
         prepCov.yr10, diagCov.yr10, artCov.yr10, vSuppCov.yr10,
         i.num.yr10, diag.yr10, artCurr.yr10, vSupp.yr10,
         diagCov2.yr10, artCov2.yr10, vSuppCov2.yr10,
         prepStartAll,
         
         elig.indexes.nd, found.indexes.nd, prp.indexes.found.nd,
         elig.indexes.pp, found.indexes.pp, prp.indexes.found.pp,
         elig.indexes.all, found.indexes.all, prp.indexes.found.all,
         
         elig.partners, found.partners, prp.partners.found.gen1, posPart.indexes, negunkPart.indexes, 
         elig.partners.gen2, found.partners.gen2, prp.partners.found.gen2, posPart.gen2, negunkPart.gen2, 
         elig.partners.all, found.partners.all, prp.partners.found.all, posPart.indexes.all, negunkPart.indexes.all,
         
         partners.per.index,
         
         tot.part.ident, elig.for.scrn,
         part.scrnd.tst, positive.part, negative.part, 
         
         part.scrnd.prep, scrnd.neg, scrnd.pos, diff.scrnd.pos, 
         scrnd.prepon, scrnd.noprep, scrnd.noprepnorisk,
         elig.prepStartPart, prepStartPart,
         
         part.start.tx,
         part.reinit.tx,
         gen.start.tx,
         pp.reinit.tx)
saveRDS(outcomes_sims_tbl3, paste0(save_dir, "/tbl3", tblnam, "_outcomes_sims.rds"))



outcomes_scenarios_tbl3 <- outcomes_sims_tbl3 %>%
  select(- c(sim)) %>%
  group_by(tbl, scenario.num, scenario.new, scenario_name) %>%
  summarise(across(everything(),
                   list(
                     low = ~ quantile(.x, 0.025, na.rm = TRUE),
                     med = ~ quantile(.x, 0.50, na.rm = TRUE),
                     high = ~ quantile(.x, 0.975, na.rm = TRUE)
                     ), 
                   .names = "{.col}__{.fn}"
                   )
            ) %>% 
  mutate(across(where(is.numeric), ~round (., 5))) %>% 
  ungroup()%>% 
  arrange(tbl, scenario.num, scenario.new, scenario_name)
saveRDS(outcomes_scenarios_tbl3, paste0(save_dir, "/tbl3",tblnam, "_outcomes_scenarios.rds"))









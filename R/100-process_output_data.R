##
## 100-Get intervention data for local analysis

#context<-"local"
context <- if (interactive()) "local" else "hpc"

# Setup ------------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyverse")
})
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")
source("R/utils-scenarios_outcomes.R")


modtst_dir <- paste0("data/intermediate/",context,"/scenarios")



#get sim batches
modtst_files <- list.files(
  modtst_dir,
  pattern = "^sim__.*rds$",
  full.names = TRUE
)



#Process each batch in parallel 
process_sim <- function(file_name, ts) {
  # keep only the file name without extension and split around `__`
  name_elts <- fs::path_file(file_name) %>%
  fs::path_ext_remove() %>%
  strsplit(split = "__")
  
  scenario_name <- name_elts[[1]][2]
  batch_number <- as.numeric(name_elts[[1]][3])
  
  d <- as_tibble(readRDS(file_name))
  d <- d %>%
    mutate(scenario_name = scenario_name, batch_number = batch_number) %>% 
    group_by(scenario_name, batch_number, sim) %>% 
    filter(time >= ts) %>% 
    mutate(time=row_number()) %>% 
    ungroup() %>% 
    group_by(scenario_name) %>%    #re-number sims for each scenario
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
    select(scenario_name, scenario.new, batch_number, sim, time,
           
           #HIV incidence measures
           incid, 
           
           #HIV screening
           tot.tests, tot.tests.ibt, tot.tests.pbt, 
           tot.tests.ibtNegunk, tot.tests.ibtPrEP, tot.tests.ibtPP,
           eligPP.for.retest, pp.tests.nic,pp.tests.ic,
           tot.part.ident, elig.part, tot.tests.pbt, positive.part,
           
           #PS measures - Indexes
           recent.newdiagn, elig.indexes.nd, found.indexes.nd,
           recent.ppretested, elig.indexes.pp, found.indexes.pp, found.indexes.pp.un,
           
           #PS measures - Wave 1 partners
           elig.partners, found.partners,
           negunkPart.indexes, posPart.indexes, elig.indexes.posPart, 
           
           #PS measures - Wave 2 partners
           elig.partners.gen2, found.partners.gen2,
           
           #all found partners
           found.partners.all,
           
           #PrEP initiation trackers
           prepStartPart, prepStartGen, prepStartAll,
           
           #ART initiation trackers
           part.start.tx, gen.start.tx,
           
           #ART re-engagement trackers
           gen.ident, gen.elig.for.reinit, gen.reinit.tx,
           part.ident, part.elig.for.reinit, part.reinit.tx,
           pp.ident, pp.elig.for.reinit, pp.reinit.tx, 
           all.reinit.tx) %>% 
    arrange(scenario.new, batch_number, sim) %>% 
  
  return(d)
}



intervds <- future.apply::future_lapply(
  modtst_files,
  process_sim,
  ts = interv_start - (5 * 52) + 1   #gets data from 5 years prior to intervention start
)



# Merge all batches  
intervdata <- bind_rows(intervds)
saveRDS(intervdata, paste0("data/intermediate/",context,"/processed/allscenarios.rds"))


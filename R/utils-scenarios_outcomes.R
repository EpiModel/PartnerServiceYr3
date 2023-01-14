library(dplyr)

# create the elements of the outcomes step by step
mutate_outcomes <- function(d) {
  d %>%
    mutate(
    cc.prep.B  = s_prep__B / s_prep_elig__B,
    cc.prep.H  = s_prep__H / s_prep_elig__H,
    cc.prep.W  = s_prep__W / s_prep_elig__W,
    prep_users = s_prep__B + s_prep__H + s_prep__W
    )
}



#calculate outcomes for year 10
get_yr10_outcomes <- function(d) {
  d %>%
    filter(time >= max(time) - 52) %>%
    group_by(scenario_name, batch_number, sim) %>%
    summarise(across(c(incid), sum, .names = "{.col}_yr10")) %>%
    mutate(ir.yr10=incid_yr10/networks_size*100) %>% 
    ungroup() %>% 
    select(-incid_yr10)
}



#get cumulative outcomes over the intervention period
get_cumulative_outcomes <- function(d) {
  #d<-d_sim
  d %>%
    filter(time >= interv_start) %>%
    group_by(scenario_name, batch_number, sim) %>%
    summarise(across(everything(),~ sum(.x, na.rm = TRUE))) %>%
    ungroup() %>% 
    select(-time)
}



#Process each batch of simulations
# the output is a data frame with one row per simulation in the batch
# each simulation can be uniquely identified with `scenario_name`,
# `batch_number` and `sim` (all 3 are needed)
process_one_scenario_batch <- function(scenario_infos) {
  #scenario_infos<-batches_infos[1,]
  
  #get batch sim data
  batch_of_sims <- readRDS(scenario_infos$file_name)
  
  #convert to tibble
  d_sim <- as_tibble(batch_of_sims) %>% 
    select(sim, time,
                  
          #HIV incidence measures
          incid,
          
          #PrEP initiation trackers
          prepStartPart, prepStartAll, prepStartGen, 
          
          #ART re-engagement trackers
          gen.ident, gen.elig.for.reinit, gen.reinit.tx,
          part.ident, part.elig.for.reinit, part.reinit.tx,
          pp.reinit.tx, pp.elig.for.reinit, pp.ident, 
          all.reinit.tx,
          
          #HIV screening measures
          tot.tests, tot.tests.ibt,  
          tot.tests.ibtNegunk, tot.tests.ibtPrEP, tot.tests.ibtPP,
          eligPP.for.retest, pp.tests.nic,pp.tests.ic,
          
          #pbt
          tot.part.ident, elig.part, tot.tests.pbt, positive.part,
          
          #PS measures - Indexes
          recent.diagn, elig.indexes.nd, found.indexes.nd,
          recent.retests, elig.indexes.pp, found.indexes.pp, found.indexes.pp.un,
          
          #PS measures - Wave 1 partners
          elig.partners, found.partners,
          negunkPart.indexes, posPart.indexes, elig.indexes.posPart, 
          
          #PS measures - Wave 2 partners
          elig.partners.gen2, found.partners.gen2,
          
          #all partners
          found.partners.all)
  
  #d_sim <- mutate_outcomes(d_sim)
  d_sim <- mutate(
    d_sim,
    scenario_name = scenario_infos$scenario_name,
    batch_number = as.numeric(scenario_infos$batch_number)
  )

  d_last <- get_yr10_outcomes(d_sim)
  d_cum <- get_cumulative_outcomes(d_sim)

  left_join(d_last, d_cum, by = c("scenario_name", "batch_number", "sim"))
}


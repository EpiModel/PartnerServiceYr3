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




#Function 1: Get yr10 outcomes  -------------------------------------------------------
get_yr10_outcomes <- function(d) {
  d %>%
    filter(time >= max(time) - 52) %>%
    group_by(scenario_name, batch_number, sim) %>%
    summarise(across(c(incid), sum, .names = "{.col}_yr10")) %>%
    mutate(ir.yr10=incid_yr10/networks_size*100) %>% 
    ungroup() %>% 
    select(-incid_yr10)
}




#Function 2: Get cumulative outcomes over intervention period ------------------------
get_cumulative_outcomes <- function(d) {
  #d<-d_sim
  d %>%
    filter(time >= interv_start) %>%
    group_by(scenario_name, batch_number, sim) %>%
    summarise(across(everything(),~ sum(.x, na.rm = TRUE))) %>%
    ungroup() %>% 
    select(-c(time,num, i.num))
}





#Function 3: Process cumm and yr10 outcomes ----------------------------------------------
#Process each batch of simulations: the output is a data frame with one row per simulation in the batch
#each simulation can be uniquely identified with `scenario_name`,`batch_number` and `sim` 
process_one_scenario_batch <- function(scenario_infos) {
  batch_of_sims <- readRDS(scenario_infos$file_name)
  d_sim <- as_tibble(batch_of_sims) %>% 
    mutate(negative.part=tot.tests.pbt-positive.part) %>% 
    select(sim, time,
                  
           #HIV incidence measures
           num, incid, i.num,
           
           #HIV screening
           tot.tests, tot.tests.ibt, tot.tests.pbt, 
           tot.tests.ibtNegunk, tot.tests.ibtPrEP, tot.tests.ibtPP,
           eligPP.for.retest, pp.tests.nic,pp.tests.ic,
           tot.part.ident, elig.part, tot.tests.pbt, positive.part, negative.part,
           
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
           all.reinit.tx)
  
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





#Function 4: Process plot data -----------------------------------------------------------
process_plotdat <- function(file_name, ts) {
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
    mutate(sim2=ifelse(batch_number > 1,
                       sim + ((batch_number-1) * max_cores),
                       sim)) %>% 
    select(-sim) %>%
    rename(sim=sim2) %>%
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

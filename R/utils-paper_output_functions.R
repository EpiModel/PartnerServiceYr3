#Functions for model output processing

library(dplyr)


#Function 1: Process plot data -----------------------------------------------------------
process_fulldata <- function(file_name, ts) {
  
  # file_name <-sim_files[1]
  # ts <- interv_start - (5 * 52) + 1
  
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
    mutate(scenario.new = scenario_name) %>% 
    ungroup() %>% 
    mutate(
      recent.indexes.all = recent.newdiagn + recent.ppretested,
      elig.indexes.all   = elig.indexes.nd + elig.indexes.pp,
      found.indexes.all  = found.indexes.nd + found.indexes.pp,
      
      elig.partners.all  = elig.partners + elig.partners.gen2,
      negative.part = tot.tests.pbt - positive.part) %>% 
    mutate(
      prp.indexes.found.nd = found.indexes.nd / elig.indexes.nd,
      prp.indexes.found.pp = found.indexes.pp / elig.indexes.pp,
      prp.indexes.found.all = found.indexes.all / elig.indexes.all,
      prp.partners.found.gen1 = found.partners / elig.partners,
      prp.partners.found.gen2 = found.partners.gen2 / elig.partners.gen2,
      prp.partners.found.all = found.partners.all / elig.partners.all,
      partners.per.index  = found.partners.all / found.indexes.all) %>% 
    select(scenario_name, scenario.new, batch_number, sim, time,

           #Distal impacts: HIV incidence measures
             incid, 
             num, 

           #Intermediate impacts: PrEP & ART coverage
             #PrEP
             prepElig, 
             elig.prepStartPart, prepStartPart,
             elig.prepStartGen, prepStartGen,
             prepStartAll,
             prep.all.stop,
             prepCurr, 
             prepCov, 

             #ART
             i.num, 
             diag, 
             diagCov,
             elig.part.start.tx, part.start.tx,
             elig.gen.start.tx, gen.start.tx,
             txStop,
             gen.elig.for.reinit, gen.reinit.tx,
             part.elig.for.reinit, part.reinit.tx,
             pp.elig.for.reinit, pp.reinit.tx,
             all.reinit.tx,
             artCurr, 
             artCov, 
             vSupp,
             vSuppCov,

           #Proximal impacts: HIV screening and PS participation
             #PS measures - Indexes
             recent.newdiagn, elig.indexes.nd, found.indexes.nd, prp.indexes.found.nd,
             recent.ppretested, elig.indexes.pp, found.indexes.pp, prp.indexes.found.pp,
             recent.indexes.all, elig.indexes.all, found.indexes.all, prp.indexes.found.all,
           
             #PS measures - Wave 1 partners
             elig.partners, found.partners, prp.partners.found.gen1,
             negunkPart.indexes, posPart.indexes, elig.indexes.posPart,

             #PS measures - Wave 2 partners
             elig.partners.gen2, found.partners.gen2, prp.partners.found.gen2,

             #all partners
             elig.partners.all, found.partners.all, prp.partners.found.all,
             partners.per.index,
           
           #HIV screening
             tot.tests, 
             tot.tests.ibt, tot.tests.pbt,
             tot.tests.ibtNegunk, tot.tests.ibtPrEP, tot.tests.ibtPP,
             eligPP.for.retest, pp.tests.nic,pp.tests.ic,
             tot.part.ident, elig.part, tot.tests.pbt, positive.part, negative.part

          ) %>%
    arrange(scenario.new, batch_number, sim) %>%
    
    return(d)
}



# #Function 2a: Get mean outcomes in year preceding intervention ---------------------------
# get_yr0_outcomes <- function(d) {
#   d %>% filter(time < 5 * 52 & time > 4*52) %>% 
#     select(scenario_name, scenario.new, sim, 
#            incid,  
#            prepCov, 
#            diagCov,
#            artCov,
#            vSuppCov) %>% 
#     group_by(scenario_name, scenario.new, sim) %>%
#     summarise(
#       across(c(incid), ~sum (.x, na.rm = T), .names = "{.col}_yr0"),
#       across(c(prepCov, diagCov, artCov, vSuppCov), ~ mean(.x, na.rm = T), .names = "{.col}_yr0")) %>% 
#     mutate(ir.yr0 = incid_yr0 / networks_size * 100) %>% 
#     select(scenario_name, scenario.new, sim,
#            ir.yr0, prepCov_yr0, diagCov_yr0, artCov_yr0, vSuppCov_yr0)  
# } 



#Function 2b: Get mean outcomes in intervention year 10 ----------------------------------
get_yr10_outcomes <- function(d) {
  d %>%
    filter(time >= max(time) - 52) %>% 
    select(scenario_name, scenario.new, sim, 
           incid,  
           prepCov, 
           diagCov,
           artCov,
           vSuppCov) %>% 
    group_by(scenario_name, scenario.new, sim) %>%
    summarise(
      across(c(incid), ~sum (.x, na.rm = T), .names = "{.col}.yr10"),
      across(c(prepCov, diagCov, artCov, vSuppCov), ~ mean(.x, na.rm = T), .names = "{.col}.yr10")) %>% 
    mutate(ir.yr10 = incid.yr10 / networks_size * 100) %>% 
    select(scenario_name, scenario.new, sim, ends_with(".yr10"))
}



#Function 2c: Get mean outcomes over intervention period ---------------------------------
get_yrMu_outcomes <- function(d) {
  d %>% filter(time > 5 * 52) %>%
    select(scenario_name, scenario.new, sim,
           prp.indexes.found.nd, 
           prp.indexes.found.pp,
           prp.indexes.found.all,
           prp.partners.found.gen1,
           prp.partners.found.gen2,
           prp.partners.found.all,
           partners.per.index) %>% 
    group_by(scenario_name, scenario.new, sim) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = T))) 
}



#Function 2d: Get cumulative HIV incidence over intervention period ----------------------
get_cumulative_outcomes <- function(d) {
  d %>% filter(time > 5 * 52) %>% 
    select(scenario_name, scenario.new, sim, time, incid) %>% 
    group_by(scenario_name, scenario.new, sim) %>%
    summarise(across(c(incid),~ sum(.x, na.rm = TRUE), .names = "{.col}.cum")) 
}


get_niapia <- function(d) {
  d %>% filter(time > 5 * 52) %>% 
    select(scenario_name, scenario.new, sim, time, incid) %>% 
    group_by(scenario_name, scenario.new, sim) %>%
    summarise(across(c(incid),~ sum(.x, na.rm = TRUE))) %>%  
    group_by(sim) %>% 
    arrange(scenario_name) %>% 
    mutate(nia = (.[[4]][1] - .[[4]]),
           pia = (.[[4]][1] - .[[4]]) / .[[4]][1]) %>% 
    ungroup() %>% 
   select(scenario_name, scenario.new, sim, nia, pia)
}



#Function 2e: Get summed and average outcomes over intervention period -------------------
get_sumave_outcomes <- function(d) {
  d %>% filter(time > 5 * 52) %>% 
    select(scenario_name, scenario.new, sim, 
           prepStartAll, 
           elig.indexes.all, found.indexes.all,
           elig.partners.all, found.partners.all,
           tot.tests.pbt, positive.part, negative.part,
           prepStartPart,
           part.start.tx,
           part.reinit.tx,
           pp.reinit.tx) %>% 
    group_by(scenario_name, scenario.new, sim) %>%
    summarise(across(everything(), ~ sum(.x, na.rm = T) / 10)) 
} 



#Function 2f: Get outcome sims data ------------------------------------------------------
get_outcome_sims<-function(d){
  #d_yr0<-get_yr0_outcomes(d)

  d_yr10<-get_yr10_outcomes(d)
  d_cum<-get_cumulative_outcomes(d)
  d_niapia<- get_niapia(d)
  d_yrMu<-get_yrMu_outcomes(d)
  d_yrmean<-get_sumave_outcomes(d)
  
  # #join all
  # d_join0<-left_join(d_yr0, d_yr10, by = c("scenario_name", "scenario.new", "sim"))
  # d_join1<-left_join(d_join0, d_yrMu, by = c("scenario_name", "scenario.new", "sim"))
  # d_join2<-left_join(d_join1, d_cum, by = c("scenario_name", "scenario.new", "sim"))
  # left_join(d_join2, d_yrmean, by = c("scenario_name", "scenario.new", "sim"))
  
  d_join0<-left_join(d_yr10, d_cum, by = c("scenario_name", "scenario.new", "sim"))
  d_join1<-left_join(d_join0, d_niapia, by = c("scenario_name", "scenario.new", "sim"))
  d_join2<-left_join(d_join1, d_yrMu, by = c("scenario_name", "scenario.new", "sim"))
  left_join(d_join2, d_yrmean, by = c("scenario_name", "scenario.new", "sim"))
}

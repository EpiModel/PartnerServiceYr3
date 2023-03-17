library(dplyr)




#Function 1: Process plot data -----------------------------------------------------------
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
                                        ifelse(scenario_name == "interv2","+ Wave 2 partn.","+ Both")))) %>% 
    mutate(scenario.new = factor(scenario.new, levels = c("Base","+ PP retests","+ Wave 2 partn.","+ Both"))) %>% 
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
           prepElig, prepElig.stat, 
           elig.prepStartPart, prepStartPart, 
           elig.prepStartGen, prepStartGen, 
           prepStartAll,
           
           #ART initiation trackers
           elig.part.start.tx, part.start.tx, 
           elig.gen.start.tx, gen.start.tx,
           
           #ART re-engagement trackers
           gen.ident, gen.elig.for.reinit, gen.reinit.tx,
           part.ident, part.elig.for.reinit, part.reinit.tx,
           pp.ident, pp.elig.for.reinit, pp.reinit.tx, 
           all.reinit.tx) %>% 
    arrange(scenario.new, batch_number, sim) %>% 
    
    return(d)
}




#Function 2a: Get yr10 outcomes  -------------------------------------------------------
get_yr10_outcomes <- function(d) {
  d %>%
    filter(time >= max(time) - 52) %>% 
    group_by(scenario_name, scenario.new, sim) %>%
    summarise(across(c(incid), sum, .names = "{.col}_yr10")) %>%
    mutate(ir.yr10=incid_yr10/networks_size*100) %>% 
    select(-incid_yr10)
}



#Function 2b: Get cumulative outcomes over intervention period ------------------------
get_cumulative_outcomes <- function(d) {
  d %>% filter(time>5*52) %>% group_by(scenario_name, scenario.new, sim) %>%
    summarise(across(incid,~ sum(.x, na.rm = TRUE)))
}



#Function 2c: Get mean outcomes (summed and averaged) over intervention period ------------------------
get_yrmean_outcomes <- function(d) {
  d %>% filter(time>5*52) %>% group_by(scenario_name, scenario.new, sim) %>%
    summarise(across(everything(),~ sum(.x, na.rm = TRUE)/10)) %>% 
    select(-c(incid, time, batch_number)) 
} 


#Function 2d: Get outcome sims data
get_outcome_sims<-function(d){
  d_yr10<-get_yr10_outcomes(d)
  d_cum<-get_cumulative_outcomes(d)
  d_yrmean<-get_yrmean_outcomes(d)
  
  #join them
  d_join<-left_join(d_yr10, d_cum, by = c("scenario_name", "scenario.new", "sim"))
  left_join(d_join, d_yrmean, by = c("scenario_name", "scenario.new", "sim"))
}

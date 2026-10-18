#Functions for model output processing

library(dplyr)


#Function 1: Process plot data -----------------------------------------------------------
process_fulldata <- function(file_name, ts) {
  
  # file_name <-sim_files[1]
  # ts <- 3901 - (5 * 52) + 1
  
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
                       sim + ((batch_number-1) * 32),
                       sim)) %>% 
    select(-sim) %>%
    rename(sim=sim2) %>%
    mutate(
      tbl0                     = toupper(substr(scenario_name,1,1)),
      scenario.num            = as.numeric(substr(scenario_name,2,4)),
      scenario.new            = substr(scenario_name,5,nchar(scenario_name))
      ) %>% 
    mutate(tbl = factor(tbl0, levels=c("A","B","P","C"))) %>% 
    ungroup() %>% 
    mutate(
      recent.indexes.all      = recent.newdiagn + recent.ppretested,
      elig.indexes.all        = elig.indexes.nd + elig.indexes.pp,
      found.indexes.all       = found.indexes.nd + found.indexes.pp,
      elig.partners.all       = elig.partners + elig.partners.gen2,
      negative.part           = part.scrnd.tst - positive.part,
      diff.scrnd.pos          = positive.part - scrnd.pos,
      negunkPart.indexes.all  = negunkPart.indexes + negunkPart.gen2,
      posPart.indexes.all     = posPart.indexes + posPart.gen2,
      diagCov2                = diag/i.num,
      artCov2                 = artCurr/diag,
      vSuppCov2               = vSupp/diag,
      numPP.1                 = (i.prev.dx * num) / 52,
      eligPP.for.retest       = eligPP.for.retest / 52,
      recent.ppretested2      = recent.ppretested / 52,
      ) %>% 
    mutate(
      prp.indexes.found.nd    = found.indexes.nd / elig.indexes.nd,
      prp.indexes.found.pp    = found.indexes.pp / elig.indexes.pp,
      prp.indexes.found.all   = found.indexes.all / elig.indexes.all,
      prp.partners.found.gen1 = found.partners / elig.partners,
      prp.partners.found.gen2 = found.partners.gen2 / elig.partners.gen2,
      prp.partners.found.all  = found.partners.all / elig.partners.all,
      partners.per.index      = found.partners.all / found.indexes.all,
      prp.allPP.eligandnic1   = eligPP.for.retest / numPP.1,
      eligPPforRetest.ooc     = eligPP.for.retest - eligPPforRetest.rxnaive,
      prp.eligPP.rxnaive      = eligPPforRetest.rxnaive / eligPP.for.retest
      ) %>% 
    mutate(
      prp.eligPP.ooc          = eligPPforRetest.ooc / eligPP.for.retest) %>% 
    select(
      tbl, scenario.num, scenario.new, scenario_name, batch_number, sim, time,

      #Distal impacts: HIV incidence measures
      incid, 
      num, ir100,
  
      #Intermediate impacts: PrEP & ART coverage
      #PrEP
      prepElig, 
      part.scrnd.prep, scrnd.neg, scrnd.pos, diff.scrnd.pos, 
      scrnd.prepon, scrnd.noprep, scrnd.noprepnorisk,
      elig.prepStartPart, prepStartPart,
      elig.prepStartGen, prepStartGen,
      prepStartAll,
      prep.all.stop,
      prepCurr, 
      prepCov, 
  
      #ART
      i.num, 
      diag, 
      diagCov, diagCov2,
      elig.part.start.tx, part.start.tx,
      elig.gen.start.tx, gen.start.tx,
      txStop,
      gen.elig.for.reinit, gen.reinit.tx,
      part.elig.for.reinit, part.reinit.tx,
      pp.elig.for.reinit, pp.reinit.tx,
      all.reinit.tx,
      artCurr, 
      artCov, artCov2,
      vSupp,
      vSuppCov, vSuppCov2,
  
      #Proximal impacts: HIV screening and PS participation
      #PS measures - Indexes
      recent.newdiagn, elig.indexes.nd, found.indexes.nd, prp.indexes.found.nd,
      recent.ppretested, recent.ppretested2, elig.indexes.pp, found.indexes.pp, prp.indexes.found.pp,
      recent.indexes.all, elig.indexes.all, found.indexes.all, prp.indexes.found.all,
     
      #PS measures - Wave 1 partners
      elig.partners, found.partners, prp.partners.found.gen1,
      negunkPart.indexes, posPart.indexes, elig.indexes.posPart,
  
      #PS measures - Wave 2 partners
      elig.partners.gen2, found.partners.gen2, prp.partners.found.gen2,
      posPart.gen2, negunkPart.gen2, 
  
      #all partners
      elig.partners.all, found.partners.all, prp.partners.found.all,
      negunkPart.indexes.all, posPart.indexes.all, partners.per.index,
     
      #HIV screening
      tot.tests, 
      tot.tests.ibt, 
      tot.tests.ibtNegunk, tot.tests.ibtPrEP, tot.tests.ibtPP,
      i.prev.dx, numPP.1, eligPP.for.retest, prp.allPP.eligandnic1, 
      eligPPforRetest.rxnaive, prp.eligPP.rxnaive, eligPPforRetest.ooc, prp.eligPP.ooc, 
      tot.part.ident, elig.for.scrn, part.scrnd.tst, positive.part, negative.part
      ) %>%
    arrange(tbl, scenario.num, scenario.new, scenario_name, batch_number, sim)
    
    return(d)
}


#Function 2a: Get cumulative HIV incidence over 10 yr intervention period ----------------
get_cumulative_outcomes <- function(d) {
  d %>% 
    filter(time > 5 * 52) %>% 
    select(tbl, scenario.num, scenario.new, scenario_name, sim, time, incid) %>% 
    group_by(tbl, scenario.num, scenario.new, scenario_name, sim) %>%
    summarise(across(c(incid),~ sum(.x, na.rm = TRUE), .names = "{.col}.cum")) 
}


#Function 2b: Get mean in yr-10 (process measures) ---------------------------------------
get_yr10_outcomes <- function(d) {
  d %>%
    filter(time >= max(time) - 52) %>% 
    select(
      tbl, scenario.num, scenario.new, scenario_name, sim, 
      incid, 
      prepCov, diagCov, artCov, vSuppCov,
      artCurr, vSupp, num, i.num, diag, 
      diagCov2, artCov2, vSuppCov2
      ) %>% 
    group_by(tbl, scenario.num, scenario.new, scenario_name, sim) %>%
    summarise(
      across(c(incid), ~sum (.x, na.rm = T)),
      across(c(prepCov, diagCov, artCov, vSuppCov,
               num, i.num, diag, artCurr, vSupp,
               diagCov2, artCov2, vSuppCov2),
             ~ mean(.x, na.rm = T), 
             .names = "{.col}.yr10"
             )
      ) %>% 
    mutate(
      ir.yr10 = incid / 100000 * 100,
      ir2.yr10 = incid / num.yr10 *100
      ) %>% 
    select(tbl, scenario.num, scenario.new, scenario_name, sim, ends_with(".yr10"))
}



#Function 2c: Get nia, pia, nnt ----------------------------------------------------------
get_niapiannt_tbl2 <- function(d) {
  d %>% 
    filter(time > 5 * 52) %>% 
    select(
      tbl, scenario.num, scenario.new, scenario_name, sim, time, 
      incid, found.indexes.all
      ) %>% 
    group_by(tbl, scenario.num, scenario.new, scenario_name, sim) %>%
    summarise(across(
      c(incid, found.indexes.all), ~ sum(.x, na.rm = TRUE)
      ))  %>% 
    arrange(tbl, sim, scenario.num) %>% 
    group_by(tbl, sim) %>% 
    mutate(base_incid = incid[1]) %>% 
    mutate(
      nia = base_incid - incid,
      pia = (base_incid - incid) / base_incid
      ) %>% 
    mutate(nnt = found.indexes.all / nia) %>% 
    ungroup() %>% 
   select(tbl, scenario.num, scenario.new, scenario_name, sim, nia, pia, nnt)
}

get_niapiannt_tbl3 <- function(d) {
  
  # base_df <- readRDS(paste0(save_dir, "/baseincid/tbl3A_baseincid.rds", sep=""))
  # base_incid <- base_df$incid
  
  d %>% 
    filter(time > 5 * 52) %>% 
    select(
      tbl, scenario.num, scenario.new, scenario_name, sim, time, 
      incid, found.indexes.all
      ) %>% 
    group_by(tbl, scenario.num, scenario.new, scenario_name, sim) %>%
    summarise(across(
      c(incid, found.indexes.all), ~ sum(.x, na.rm = TRUE)
      ))  %>% 
    arrange(tbl, sim, scenario.num) %>% 
    group_by(sim) %>% 
    mutate(base_incid = incid[1]) %>% 
    mutate(
      nia = base_incid - incid,
      pia = (base_incid - incid) / base_incid
      ) %>% 
    mutate(nnt = found.indexes.all / nia) %>% 
    ungroup() %>% 
    arrange(tbl, scenario.num, scenario.new, scenario_name, sim) %>% 
    select(tbl, scenario.num, scenario.new, scenario_name, sim, nia, pia, nnt)
}



#Function 2d: Get mean outcomes over intervention period (mean/interv yr) ----------------
get_yrMu_outcomes <- function(d) {
  d %>% 
    filter(time > 5 * 52) %>%
    select(
      tbl, scenario.num, scenario.new, scenario_name, sim,
      prp.indexes.found.nd, 
      prp.indexes.found.pp,
      prp.indexes.found.all,
      prp.partners.found.gen1,
      prp.partners.found.gen2,
      prp.partners.found.all,
      partners.per.index,
      i.prev.dx, 
      prp.allPP.eligandnic1,
      #prp.allPP.eligandnic2,
      prp.eligPP.rxnaive,
      prp.eligPP.ooc
      ) %>% 
    group_by(tbl, scenario.num, scenario.new, scenario_name, sim) %>%
    summarise(across(
      everything(), 
      ~ mean(.x, na.rm = T)
      )
      ) 
}



#Function 2e: Get summed and average outcomes over intervention period -------------------
get_sumave_outcomes <- function(d) {
  d %>% 
    filter(time > 5 * 52) %>% 
    select(
      tbl, scenario.num, scenario.new, scenario_name, sim, 
      prepStartAll, 
      # elig.indexes.nd, found.indexes.nd, 
      # elig.indexes.pp, found.indexes.pp, 
      # elig.indexes.all, found.indexes.all,
      
      elig.indexes.nd, found.indexes.nd,
      numPP.1, eligPP.for.retest, eligPPforRetest.rxnaive, eligPPforRetest.ooc, recent.ppretested, recent.ppretested2,
      elig.indexes.pp, found.indexes.pp,
      elig.indexes.all, found.indexes.all,
      
      elig.partners, found.partners, negunkPart.indexes, posPart.indexes,
      elig.partners.gen2, found.partners.gen2, posPart.gen2, negunkPart.gen2, 
      elig.partners.all, found.partners.all, negunkPart.indexes.all, posPart.indexes.all,
     
      tot.part.ident, elig.for.scrn,
      part.scrnd.tst, positive.part, negative.part, 
     
      part.scrnd.prep, scrnd.neg, scrnd.pos, diff.scrnd.pos,
      scrnd.prepon, scrnd.noprep, scrnd.noprepnorisk,
      elig.prepStartPart, prepStartPart,
     
      part.start.tx,
      part.reinit.tx,
      gen.start.tx,
      pp.reinit.tx
      ) %>% 
    group_by(tbl, scenario.num, scenario.new, scenario_name, sim) %>%
    summarise(across(
      everything(), 
      ~ sum(.x, na.rm = T) / 10
      )
      ) 
} 



#Function 2f: Get processed data ---------------------------------------------------------
get_outcome_sims_tbl2 <- function(d){
  
  d_yr10      <- get_yr10_outcomes(d)
  d_cum       <- get_cumulative_outcomes(d)
  d_niapiannt <- get_niapiannt_tbl2(d)
  d_yrMu      <- get_yrMu_outcomes(d)
  d_yrmean    <- get_sumave_outcomes(d)
  
  #merge files
  d_join0 <- left_join(
    d_yr10, 
    d_cum, 
    by = c("tbl","scenario.num", "scenario.new","scenario_name", "sim")
    )
  d_join1 <- left_join(
    d_join0, 
    d_niapiannt, 
    by = c("tbl","scenario.num", "scenario.new","scenario_name", "sim")
    )
  d_join2 <- left_join(
    d_join1, 
    d_yrMu, 
    by = c("tbl","scenario.num", "scenario.new","scenario_name", "sim")
    )
  
  left_join(
    d_join2, 
    d_yrmean, 
    by = c("tbl","scenario.num", "scenario.new","scenario_name", "sim")
    )
}


get_outcome_sims_tbl3 <- function(d){

  d_yr10      <- get_yr10_outcomes(d)
  d_cum       <- get_cumulative_outcomes(d)
  d_niapiannt <- get_niapiannt_tbl3(d)
  d_yrMu      <- get_yrMu_outcomes(d)
  d_yrmean    <- get_sumave_outcomes(d)
  
  #merge files
  d_join0 <- left_join(d_yr10, d_cum, 
                       by = c("tbl","scenario.num", "scenario.new","scenario_name", "sim"))
  d_join1 <- left_join(d_join0, d_niapiannt, 
                       by = c("tbl","scenario.num", "scenario.new","scenario_name", "sim"))
  d_join2 <- left_join(d_join1, d_yrMu, 
                       by = c("tbl","scenario.num", "scenario.new","scenario_name", "sim"))
  
  left_join(d_join2, d_yrmean, 
            by = c("tbl","scenario.num", "scenario.new","scenario_name", "sim"))
}



# get_niapiannt_tbl3 <- function(d) {
#   d %>% 
#     filter(time > 5 * 52) %>% 
#     select(
#       tbl, scenario.num, scenario.new, scenario_name, sim, time, 
#       incid, found.indexes.all
#     ) %>% 
#     group_by(tbl, scenario.num, scenario.new, scenario_name, sim) %>%
#     summarise(across(
#       c(incid, found.indexes.all),
#       ~ sum(.x, na.rm = TRUE)
#     )
#     )  %>% 
#     arrange(sim, tbl, scenario.num) %>% 
#     group_by(sim) %>% 
#     mutate(base_incid = incid[1]) %>% 
#     mutate(
#       nia = base_incid - incid,
#       pia = (base_incid - incid) / base_incid
#     ) %>% 
#     mutate(nnt = found.indexes.all / nia) %>% 
#     ungroup() %>% 
#     arrange(tbl, scenario.num, scenario.new, scenario_name, sim) %>% 
#     select(tbl, scenario.num, scenario.new, scenario_name, sim, nia, pia, nnt)
# }

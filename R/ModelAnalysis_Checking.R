#Model output
  #Summary netsim object
  sim
  
  #Epidemic plots
  par(mfrow=c(2,2))
  plot(sim, leg.cex = 0.4)
  plot(sim, y=c("incid.B","incid.H","incid.W"), qnts = F, legend=T, leg.cex = 0.5)
  plot(sim, y=c("prepElig.B","prepElig.H","prepElig.W"), legend=T, leg.cex = 0.5)
  plot(sim, y=c("prepCurr.B","prepCurr.H","prepCurr.W"), legend=T, leg.cex = 0.5)
  
  par(mfrow=c(2,2))
  plot(sim, y=c("incid","incid.B","incid.H","incid.W"), qnts = F, legend=T, leg.cex = 0.5, main="incident HIV)")
  plot(sim, y=c("tot.tests","tot.tests.nprep"), legend=T, leg.cex = 0.5, main="Total tests + non-PrEP tests")
  plot(sim, y=c("tot.tests.B","tot.tests.H","tot.tests.W"), legend=T, leg.cex = 0.5, main="Total tests in B,H,W")
  plot(sim, y=c("tot.tests.ibt","tot.tests.pbt"), legend=T, leg.cex = 0.5, main="Total IBT vs PBT")
  

  #Data extraction
    #all sims
    df.allsims<-as.data.frame(sim)
    #head(df.allsims,10); tail(df.allsims,10)
    
    library(dplyr)
    library(tidyverse)
    #Tested for HIV B4 PS
    p_tb4PS<-df.allsims %>% filter(time<1*52) %>% group_by(sim) %>% 
      summarise(totpop=1000, 
                incidHIV=sum(incid, na.rm = T), 
                ndindexes=sum(recent_diagn, na.rm = T), 
                ndindexes.elig=sum(elig_indexes,na.rm = T), 
                ndindexes.initPS=sum(found_indexes, na.rm = T),
                ndpart.elicit=sum(elig_partners, na.rm = T), 
                ndpart.ident=sum(found_partners, na.rm = T), 
                ndpart.ident2=sum(tot.part.ident, na.rm=T), 
                ndpart.eligTst=sum(elig.part.for.pbt, na.rm = T),
                ndpart.tested=sum(tot.tests.pbt, na.rm = T),
                ave.pp.elig.retst=round(mean(eligPP.forTst, na.rm = T),0),
                pp.tested=sum(tot.tests.PP, na.rm = T),
                tot.tests.ibt=sum(tot.tests.ibt, na.rm=T), 
                tot.tested=sum(tot.tests, na.rm=T))
    
    #Tested for HIV after PS
    p_tafterPS<-df.allsims %>% filter(time>=1*52) %>% group_by(sim) %>% 
      summarise(totpop=1000, 
                incidHIV=sum(incid, na.rm = T), 
                ndindexes=sum(recent_diagn, na.rm = T), 
                ndindexes.elig=sum(elig_indexes,na.rm = T), 
                ndindexes.initPS=sum(found_indexes, na.rm = T),
                ndpart.elicit=sum(elig_partners, na.rm = T), 
                ndpart.ident=sum(found_partners, na.rm = T), 
                ndpart.ident2=sum(tot.part.ident, na.rm=T), 
                ndpart.eligTst=sum(elig.part.for.pbt, na.rm = T),
                ndpart.tested=sum(tot.tests.pbt, na.rm = T),
                ave.pp.elig.retst=round(mean(eligPP.forTst, na.rm = T),0),
                pp.tested=sum(tot.tests.PP, na.rm = T),
                tot.tests.ibt=sum(tot.tests.ibt, na.rm=T), 
                tot.tested=sum(tot.tests, na.rm=T))
    
    p_t<-data.frame(rbind(b4=p_tb4PS,after=p_tafterPS)) %>% tibble::add_column(time=c("b4","after"), .before="sim") %>% select(-c(sim, totpop));p_t
    
  

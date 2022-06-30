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
    
    #Testing by year
    #p_tyr<-df.allsims %>% mutate(yr=cut(time,breaks = c(seq(0,10*52,by=52)), labels = c("1","2","3","4","5","6","7","8","9","10"),include.lowest = T)) %>% group_by(yr) %>% 
  
    p_tyrND<-as.data.frame(df.allsims) %>% mutate(yr=cut(time,breaks = c(seq(0,4*52,by=52)), labels = c("1","2","3","4"),include.lowest = T)) %>% group_by(yr) %>% 
      summarise(totpop=1000, 
                incidHIV=sum(incid, na.rm = T),                                 
                new.diag=sum(recent_diagn, na.rm = T), 
                nd.elig.for.ps=sum(elig_indexesND,na.rm = T), 
                nd.initiat.ps=sum(found_indexesND, na.rm = T),
                ndpart.elicited=sum(elig_partnersND, na.rm = T), 
                ndpart.found=sum(found_partnersND, na.rm = T), 
                part.ident.at.psND=sum(tot.part.identND, na.rm = T),
                ndpart.elig.for.hivtest=sum(elig.part.for.testND, na.rm = T),
                pbt.doneND=sum(tot.tests.pbtND, na.rm = T),        
                pbt.pos.testsND=sum(tot.pos.tests.pbtND, na.rm = T))       
                
    p_tyrPP<-as.data.frame(df.allsims) %>% mutate(yr=cut(time,breaks = c(seq(0,4*52,by=52)), labels = c("1","2","3","4"),include.lowest = T)) %>% group_by(yr) %>% 
                summarise(totpop=1000, 
                #ave.pp.elig.retest.pertimestep=round(mean(eligPP.for.retest, na.rm = T),0),
                tot.ibt.amongPP=sum(tot.retests.PP, na.rm = T),
                pp.retests=sum(recent_retests, na.rm = T),               
                pp.elig.for.ps=sum(elig_indexesPP,na.rm = T), 
                pp.initiat.ps=sum(found_indexesPP, na.rm = T),
                pppart.elicited=sum(elig_partnersPP, na.rm = T), 
                pppart.found=sum(found_partnersPP, na.rm = T), 
                part.ident.at.psPP=sum(tot.part.identPP, na.rm = T),
                pppart.elig.for.hivtest=sum(elig.part.for.testPP, na.rm = T),
                pbt.donePP=sum(tot.tests.pbtPP, na.rm = T),        
                pbt.pos.testsPP=sum(tot.pos.tests.pbtPP, na.rm = T))
    
    p_tyrTOT<-as.data.frame(df.allsims) %>% mutate(yr=cut(time,breaks = c(seq(0,4*52,by=52)), labels = c("1","2","3","4"),include.lowest = T)) %>% group_by(yr) %>% 
      summarise(totpop=1000,     
                #tot.part.identified.at.ps=sum(tot.part.ident, na.rm = T),
                tot.nonPP.ibt=sum(tot.tests.ibt, na.rm=T), 
                tot.pos.nonPP.ibt=sum(tot.pos.tests.ibt, na.rm = T),
                tot.PP.ibt=sum(tot.retests.PP, na.rm = T),
                tot.pos.PP.ibt=sum(tot.retests.PP, na.rm = T),
                tot.pbt=sum(tot.tests.pbtND, na.rm = T)+sum(tot.tests.pbtPP, na.rm = T),
                tot.pos.pbt=sum(tot.pos.tests.pbtND, na.rm = T) + sum(tot.pos.tests.pbtPP, na.rm = T),
                tot.tests=sum(tot.tests, na.rm=T),
                tot.pos.tests=sum(tot.pos.tests.ibt, na.rm = T)+sum(tot.retests.PP, na.rm = T)+sum(tot.pos.tests.pbtND, na.rm = T) + sum(tot.pos.tests.pbtPP, na.rm = T)) %>% 
      mutate(ppn_pos_tests_arePP=tot.pos.PP.ibt/tot.pos.tests)
    
    p_tyrND
    p_tyrPP
    p_tyrTOT
    

  

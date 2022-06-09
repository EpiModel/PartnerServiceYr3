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
    #Tested for HIV
    NewHIV<-df.allsims %>% group_by(sim) %>% summarise(totpop=1000,
                                                       tot.tested=sum(tot.tests, na.rm=T), tot.tests.nprep=sum(tot.tests.nprep, na.rm=T), tot.tests.ibt=sum(tot.tests.ibt, na.rm=T), tot.tests.pbt=sum(tot.tests.pbt, na.rm=T),
                                                       totNewHIV=sum(incid, na.rm = T), 
                                                       totpop.B=round(mean(num.B, na.rm=T),0), tested.B=sum(tot.tests.B, na.rm = T), totNewHIV.B=sum(incid.B, na.rm = T), 
                                                       totpop.H=round(mean(num.H, na.rm=T),0), tested.H=sum(tot.tests.H, na.rm = T), totNewHIV.H=sum(incid.H, na.rm = T),
                                                       totpop.W=round(mean(num.W, na.rm=T),0), tested.W=sum(tot.tests.W, na.rm = T), totNewHIV.W=sum(incid.W, na.rm = T)) %>% 
      mutate(incidpct.tot=totNewHIV/totpop, incidpct.B=totNewHIV.B/totpop.B, incidpct.H=totNewHIV.H/totpop.H, incidpct.W=totNewHIV.W/totpop.W);NewHIV
    
    p_t<-df.allsims %>% group_by(sim) %>% 
      summarise(totpop=1000, incidHIV=sum(incid, na.rm = T), ndindexes=sum(recent_diagn, na.rm = T), ndindexes.elig=sum(elig_indexes,na.rm = T), ndindexes.initPS=sum(found_indexes, na.rm = T),
                ndpart.elicit=sum(elig_partners, na.rm = T), ndpart.ident=sum(found_partners, na.rm = T), ndpart.ident2=sum(tot.part.ident, na.rm=T), ndpart.eligTst=sum(elig.part.for.pbt, na.rm = T),ndpart.tested=sum(tot.tests.pbt, na.rm = T),
                tot.tests.ibt=sum(tot.tests.ibt, na.rm=T), tot.tested=sum(tot.tests, na.rm=T)); p_t
    
    #just the means
    df.means<-as.data.frame(sim, out="mean")
    head(df.means,10)
    tail(df.means,10)
  
  #NetworkDynamic objects
  nw1<-get_network(sim, sim=1); nw1
  df.nw1<-as.data.frame(nw1)
  head(df.nw1,10)
  
plot(sim, type = "formation")
sim2<-mutate_epi(sim,i.num=)
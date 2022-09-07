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
#mutate(yr=cut(time,breaks = c(seq(0,4*52,by=52)), labels = c("1","2","3","4"),include.lowest = T)) %>% group_by(yr) %>

tst_summ<-as.data.frame(df.allsims) %>% mutate(yr=cut(time,breaks = c(seq(0,3*52,by=52)), labels = c("1","2","3"),include.lowest = T)) %>% group_by(yr) %>%
  summarise(totpop=10000,     
            tot.ibt.noPP=sum(tot.tests.ibt, na.rm=T), 
            pos.ibt.nonPP=sum(tot.pos.tests.ibt, na.rm = T),
            
            tot.ibt.PP=sum(tot.retests.PP, na.rm = T),
            pos.ibt.PP=sum(tot.retests.PP, na.rm = T),
            nic.ibt.PP=sum(retested.nic, na.rm = T),
            nic.pct=nic.ibt.PP/tot.ibt.PP,
            norx.nic=sum(retested.nic.rxnaive, na.rm = T),
            norx.pct=norx.nic/nic.ibt.PP,
            
            gen12.partn.ident.4pbt=sum(tot.part.ident, na.rm = T),
            gen12.partn.elig.4pbt=sum(elig.part, na.rm = T),
            tot.pbt=sum(tested.part, na.rm = T),
            pos.pbt=sum(positive.part, na.rm = T),
            
            all.tests=sum(tot.tests, na.rm=T),
            all.pos.tests=sum(tot.pos.tests.ibt, na.rm = T) + sum(tot.retests.PP, na.rm = T) + sum(positive.part, na.rm = T)) %>% 
  mutate(ppn_pos_tests_arePP=round(pos.ibt.PP/all.pos.tests,3))

psGen1_summ<-as.data.frame(df.allsims) %>% mutate(yr=cut(time,breaks = c(seq(0,3*52,by=52)), labels = c("1","2","3"),include.lowest = T)) %>% group_by(yr) %>%
  summarise(totpop=10000, 
            incidHIV=sum(incid, na.rm = T),   
            
            new.diag=sum(recent.diagn, na.rm = T), 
            nd.elig.for.ps=sum(elig.indexes.nd,na.rm = T), 
            nd.initiate.ps=sum(found.indexes.nd, na.rm = T),
            
            pp.retests=sum(recent.retests, na.rm = T),               
            pp.elig.for.ps=sum(elig.indexes.pp, na.rm = T), 
            pp.initiate.ps=sum(found.indexes.pp, na.rm = T),
            
            gen1.partn.eligible=sum(elig.partners, na.rm = T), 
            gen1.partn.found=sum(found.partners, na.rm = T)) 
            

psGen2_summ<-as.data.frame(df.allsims) %>% mutate(yr=cut(time,breaks = c(seq(0,3*52,by=52)), labels = c("1","2","3"),include.lowest = T)) %>% group_by(yr) %>% 
  summarise(totpop=10000,
            gen1.partn.found=sum(found.partners, na.rm = T), 
            
            gen1.posPart.indexes=sum(posPart.indexes, na.rm = T),               
            gen1.posPart.elig.for.ps=sum(elig.indexes.posPart, na.rm = T), 
            
            gen2.partn.eligible=sum(elig.partners.gen2, na.rm = T),
            gen2.partn.found=sum(found.partners.gen2, na.rm = T),

            all.partn.found=sum(found.partners.all, na.rm = T),
            
            partn.ident.4pbt=sum(tot.part.ident, na.rm = T),
            partn.elig.4pbt=sum(elig.part, na.rm = T),
            partn.tested=sum(tested.part, na.rm = T),        
            partn.positive=sum(positive.part, na.rm = T))    


ppreinit_summ<-as.data.frame(df.allsims) %>% mutate(yr=cut(time,breaks = c(seq(0,10*52,by=52)), labels = c("1","2","3","4","5","6","7","8","9","10"),include.lowest = T)) %>% group_by(yr) %>% 
  summarise(totpop=10000,
            pp.retested=sum(tot.retests.PP, na.rm = T),
            pp.positive=sum(tot.retests.PP, na.rm = T),
            
            pp.retests.psside=sum(recent.retests, na.rm = T),               
            pp.elig.for.ps=sum(elig.indexes.pp, na.rm = T), 
            pp.found=sum(found.indexes.pp, na.rm = T),
            pp.found2=sum(found.indexes.pp.un, na.rm = T),
            
            pp.elig.reinit=sum(pp.elig.for.reinit, na.rm = T),
            pp.reinit=sum(pp.reinit.tx, na.rm = T))



knitr::kable(tst_summ)
knitr::kable(psGen1_summ)
knitr::kable(psGen2_summ)
knitr::kable(ppreinit_summ)




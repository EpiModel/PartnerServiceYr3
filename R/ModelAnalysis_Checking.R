#Model output
#Summary netsim object
sim

#Epidemic plots
par(mfrow=c(2,2))
plot(sim, leg.cex = 0.4)
plot(sim, y=c("incid.B","incid.H","incid.W"), qnts = F, legend=T, leg.cex = 0.5, main="HIV incidence")
abline(v=seq(0, 10*52, by=52), col="grey84", lty=c(2,2))
plot(sim, y=c("prepElig.B","prepElig.H","prepElig.W"), legend=T, leg.cex = 0.5, main="PrEP eligibility")
abline(v=seq(0, 10*52, by=52), col="grey84", lty=c(2,2))
plot(sim, y=c("prepCurr.B","prepCurr.H","prepCurr.W"), legend=T, leg.cex = 0.5, main='Current PrEP use')
abline(v=seq(0, 10*52, by=52), col="grey84", lty=c(2,2))


par(mfrow=c(2,2))
plot(sim, y=c("tot.tests","tot.tests.ibt", "tot.tests.pbt"), legend=T, leg.cex = 0.5, main="Indiv-based vs Partner-based testing")
abline(v=seq(0*52, 9*52, by=52), col="grey94", lty=c(2,2))
abline(v=4*52, col="grey64", lty=2)
plot(sim, y=c("tot.tests.ibt","tot.tests.ibtNegunk", "tot.tests.ibtLateAIDs", "tot.tests.ibtPrEP", "tot.tests.ibtPP"), legend=T, leg.cex = 0.5, main="Indiv-based testing by groups")
abline(v=seq(0*52, 9*52, by=52), col="grey94", lty=c(2,2))
abline(v=4*52, col="grey64", lty=2)
plot(sim, y=c("tot.tests.ibtPP","pp.tests.nic","pp.tests.ic"), legend=T, leg.cex = 0.5, main="PP retesting by HIV care status")
abline(v=seq(0*52, 9*52, by=52), col="grey94", lty=c(2,2))
abline(v=4*52, col="grey64", lty=2)
plot(sim, y=c("tot.tests.ibtPP","pp.tests.nicrxnaive","pp.tests.nicooc"), legend=T, leg.cex = 0.5, main="PP retesting by cummul. tx status")
abline(v=seq(0*52, 9*52, by=52), col="grey94", lty=c(2,2))
abline(v=4*52, col="grey64", lty=2)


#Data extraction
library(dplyr)
library(tidyverse)

#all sims
# df.allsims<-as.data.frame(sim)

df.allsims<-do.call("rbind",d_list) %>% 
  rename(scenario_name=scenario) %>% 
  mutate(batch=1)

df<-df.allsims %>% 
  filter(time>=max(time)-(2*52)+1) %>% 
  mutate(scenario=ifelse(scenario_name=="base",1,
                         ifelse(scenario_name=="interv1",2,
                                ifelse(scenario_name=="interv2",3,4)))) %>% 
  mutate(scenario1=ifelse(scenario_name=="base","Base",
                          ifelse(scenario_name=="interv1","+ PP retests",
                                 ifelse(scenario_name=="interv2","+ Wave 2 PS","+ Both")))) %>% 
  mutate(scenario1=fct_relevel(scenario1,"Base","+ PP retests","+ Wave 2 PS","+ Both")) %>% 
  mutate(sim2=ifelse(batch>1,sim +((batch-1)*10),sim)) %>% 
  select(-sim) %>% rename(sim=sim2) %>% 
  group_by(scenario, sim) %>% 
  mutate(time2=row_number()) %>% ungroup() %>% 
  select(-time) %>% rename(time=time2) %>% 
  mutate(year=cut(time, 
                  breaks = seq(0,max(time),52),
                  labels = c(1:2))) %>% 
  #filter(!is.na(year)) %>% 
  select(scenario, scenario1, sim, batch, time, year,
         num, incid, i.num,
         tot.tests, tot.tests.ibt, tot.tests.pbt, 
         tot.tests.ibtNegunk, tot.tests.ibtLateAIDs, tot.tests.ibtPrEP, tot.tests.ibtPP,
         pp.tests.nic,pp.tests.ic,
         recent.diagn, elig.indexes.nd, found.indexes.nd,
         recent.retests, elig.indexes.pp, found.indexes.pp,
         elig.partners, found.partners,
         posPart.indexes, elig.indexes.posPart, 
         elig.partners.gen2, found.partners.gen2,
         found.partners.all)


#Functions
getYrMeanTbl<-function(dat,var){
  colnam<-enquo(var)
  tbl<-dat %>%
    filter(!is.na(year)) %>% 
    group_by(scenario,scenario1, sim, .groups='drop') %>%
    summarise(tot=sum({{var}})) %>% 
    mutate(simyr.mu=tot/10) %>%  #get mean tests per year for each sim
    group_by(scenario1, .groups='drop') %>% 
    summarise(med=median(simyr.mu)) %>% 
    mutate_if(is.numeric,round,digits=0) %>% 
    select(scenario1,med)
  colnames(tbl)[2]= as_label(colnam)
  return(tbl)
}



#HIV Incidence
  #Plot: Incidence
  incid.mu<-df %>% 
    group_by(scenario, scenario1, time) %>% 
    summarise(mean.incid=mean(incid)) 
  
  
  ggplot()+
    geom_smooth(data=incid.mu, aes(time, mean.incid), se=F)+
    labs(x="Time (wks)",
         y="Incident HIV")+
    #scale_x_continuous(name="",limits = c(0,10*52),breaks = seq(0,10*52,52),expand = c(0.01,0.01))+
    theme_bw()+
    theme(panel.grid.major.y = element_line(colour = "grey96", size=0.1),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank())+
    facet_wrap(scenario1~.)
  
  #Table
  incid.yr10<-df %>% filter(time >= max(time)-1*52+1) %>% group_by(scenario1, sim) %>% 
    summarise(incid=sum(incid, na.rm = T)) %>% 
    mutate(ir=incid/10000*100) %>% 
    group_by(scenario1) %>% 
    summarise(ir.50=median(ir)) %>% 
    mutate_if(is.numeric,round,digits=2) %>% 
    select(scenario1,ir.50)
  
  cum.incid<-df %>% group_by(scenario1, sim) %>% 
    summarise(incid=sum(incid, na.rm = T)) %>% 
    group_by(scenario1) %>% 
    summarise(incid.50=median(incid)) %>% 
    mutate_if(is.numeric,round,digits=0) %>% 
    select(scenario1,incid.50)
  
  base<-cum.incid$incid.50[1];base
  nia<-base-cum.incid$incid.50; nia
  pia<-nia/base;pia
  
  hiv.out<-left_join(incid.yr10,cum.incid,by="scenario1") 
  hiv.out<-cbind(hiv.out,nia,pia) %>% 
    mutate_if(is.numeric,round,digits=4) %>% 
    mutate(pia2=paste(pia*100,"%",sep="")) %>% 
    mutate(nia=ifelse(nia==0,"-",nia),
           pia2=ifelse(nia=="-","-",pia2)) %>% 
    select(-pia) %>% rename(pia=pia2,IR=ir.50)
  
  kable(hiv.out) %>% kable_classic() %>% kable_styling(latex_options="hold_position",position="center")

  ##Plot - Incidence Rate by year
  incid.yr<-df %>% 
    group_by(scenario1, sim, year) %>% 
    summarise(incid=sum(incid)) %>% 
    mutate(ir=incid/10000*100) %>%  #rate per 100PY
    group_by(scenario1, year) %>% 
    summarise(ir.50=median(ir)) %>% 
    filter(!is.na(year))
  
  ggplot(data=incid.yr)+
    geom_line(aes(x=as.factor(year), y=ir.50, group=scenario1, color=scenario1), lwd=1)+
    #geom_ribbon(aes(x=as.factor(year), ymin=ir.low, ymax=ir.high, group=scenario1, fill=scenario1), alpha=0.25)+
    labs(x="Year",
         y="Incidence Rate (per 100PY)")+
    scale_x_discrete(expand = c(0.01,0.01))+
    #scale_y_continuous(limits=c(0,2),breaks=seq(0,2,0.25),expand=c(0.01,0.01)) +
    theme_bw()+
    theme(panel.grid.major.y = element_line(colour = "grey86", size=0.1),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank(),
          legend.title = element_blank())
 
#HIV screening
  #All
  alltst<-df %>% 
    select(scenario1,sim,time,
           tot.tests, tot.tests.ibt, tot.tests.pbt) %>% 
    group_by(scenario1, time) %>% 
    summarise(all=mean(tot.tests),
              ibt=mean(tot.tests.ibt),
              pbt=mean(tot.tests.pbt)) %>% 
    reshape2::melt(.,id.vars=c("scenario1","time"))
  
  ggplot(data=alltst, aes(x=time, y=value, group=variable, color=variable))+
    geom_smooth(se=F)+
    labs(x="Time (wks)",
         y="Count")+
    scale_x_continuous(expand = c(0.01,0.01))+
    scale_color_manual(name="",values=c("black","firebrick","steelblue"), labels=c("All tests","Individual-based Tests","Partner-based Tests"))+
    theme_bw()+
    theme(panel.grid.major.y = element_line(colour = "grey96", size=0.1),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank(),
          legend.position = "bottom", legend.direction = "horizontal",
          legend.margin = margin(-10, 0, 0, 0),)+
    facet_wrap(~scenario1)
  
  #Table - All HIV testing
  tot<-getYrMeanTbl(df,tot.tests)
  tot.ibt<-getYrMeanTbl(df,tot.tests.ibt)
  tot.pbt<-getYrMeanTbl(df,tot.tests.pbt)
  
  tottests.out<-left_join(tot,tot.ibt,by="scenario1") 
  tottests.out<-left_join(tottests.out,tot.pbt, by="scenario1") 
  
  kable(tottests.out,
        col.names = c("Scenario","Total Tests","Individual-based Tests","Partner-based Tests"),
        caption = "Median (95\\% SI) average tests per intervention year") %>% 
    kable_classic() %>% kable_styling(latex_options="hold_position",position="center") 

  #Table - IBT
  NegUnk<-getYrMeanTbl(df,tot.tests.ibtNegunk)
  LateAIDs<-getYrMeanTbl(df,tot.tests.ibtLateAIDs)
  PrEP<-getYrMeanTbl(df,tot.tests.ibtPrEP)
  PrevPos<-getYrMeanTbl(df,tot.tests.ibtPP)
  
  ibt.tests<-left_join(NegUnk,LateAIDs, by="scenario1")
  ibt.tests<-left_join(ibt.tests,PrEP, by="scenario1")
  ibt.tests<-left_join(ibt.tests,PrevPos, by="scenario1")
  ibt.tests<-left_join(ibt.tests,tot.ibt, by="scenario1")
  
  kable(ibt.tests,
        col.names = c("Scenario","Among HIV Neg/Unk","Among Late/AIDS","Among PrEP Users", "Among Prev. Pos.","Total"),
        caption = "Individual-Based HIV Screening (Median [95\\% SI])") %>% 
    kable_classic() %>% kable_styling(latex_options="hold_position",position="center") 

  #Plot-Testing PBT (Base and Wave scenarios only)
  pbt<-df %>% 
    filter(scenario %in% c(1,2,3)) %>% 
    select(scenario, scenario1,sim,time, tot.tests.pbt) %>% 
    group_by(scenario,scenario1, time) %>% 
    summarise(mu=mean(tot.tests.pbt)) 
  
  ggplot(data=pbt,aes(x=time,y=mu))+
    geom_smooth(se=F)+
    #geom_line()+
    labs(x="Time (wks)",
         y="Count")+
    scale_x_continuous(expand = c(0.01,0.01))+
    #scale_color_manual(name="",values=c("black","steelblue","green","red"))+
    theme_bw()+
    theme(panel.grid.major.y = element_line(colour = "grey86", size=0.1),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank())+
    facet_grid(~scenario1)  
  
  
#PS measures
  #Table 3 - Indexes
  recentND<-getYrMeanTbl(df,recent.diagn)
  eligND<-getYrMeanTbl(df,elig.indexes.nd)
  foundND<-getYrMeanTbl(df,found.indexes.nd)
  
  recentPP<-getYrMeanTbl(df,recent.retests)
  eligPP<-getYrMeanTbl(df,elig.indexes.pp)
  foundPP<-getYrMeanTbl(df,found.indexes.pp)
  
  foundALL<-df %>% group_by(scenario1, sim) %>% 
    summarise(found.indexes.nd=sum(found.indexes.nd), found.indexes.pp=sum(found.indexes.pp)) %>% 
    mutate(found.indexes=found.indexes.nd+found.indexes.pp) %>% 
    mutate(val=found.indexes/10) %>% 
    group_by(scenario1) %>% 
    summarise(med=median(val)) %>% 
    mutate_if(is.numeric,round,digits=0) %>% 
    select(scenario1,med)
  
  pspartND<-left_join(recentND,eligND,by="scenario1") ; pspartND<-left_join(pspartND,foundND, by="scenario1")
  pspartPP<-left_join(recentPP,eligPP,by="scenario1") ; pspartPP<-left_join(pspartPP,foundPP, by="scenario1")
  pspart<-left_join(pspartND, pspartPP, by="scenario1")#; pspart<-left_join(pspart,foundALL, by="scenario1") 
  
  kable(pspart,
        col.names = c("Scenario","Recent Diagn.","Eligible Index-ND","Found Index-ND", 
                      "PP Retests","Eligible Index-PP","Found Index-PP"),
        caption = "Median (95\\% SI) Partner Service Measures: Index Cases") %>% kable_classic() %>% kable_styling(latex_options=c("hold_position","scale_down"),position="left") %>% 
    footnote(general = "ND=newly diagnosed cases; PP=Previous positive cases", 
             general_title = "Note:",
             footnote_as_chunk = T)

  #Table 4 - Wave 1 Partners
  eligPart<-getYrMeanTbl(df,elig.partners)
  foundPart<-getYrMeanTbl(df,found.partners)
  
  pspartPart<-left_join(foundALL,eligPart,by="scenario1"); pspartPart<-left_join(pspartPart,foundPart,by="scenario1")
  
  kable(pspartPart, 
        col.names = c("Scenario","Found Indexes (ND+PP)","Eligible Wave 1 Partners","Found Wave 1 Partners"),
        caption = "Median (95\\% SI) Partner Service Measures: Wave 1 Partners") %>% kable_classic() %>% kable_styling(latex_options=c("hold_position"),position="left") %>% 
    footnote(general = "ND=newly diagnosed cases; PP=Previous positive cases; Wave 1=Partners to index cases", 
             general_title = "Note:",
             footnote_as_chunk = T)  
  
  #Table 4 - Wave 2 Partners
  pospart.indexes<-getYrMeanTbl(df,posPart.indexes)
  eligindexes.pospart<-getYrMeanTbl(df,elig.indexes.posPart)
  eligPart.gen2<-getYrMeanTbl(df,elig.partners.gen2)
  foundPart.gen2<-getYrMeanTbl(df,found.partners.gen2)
  
  
  pospartIN<-left_join(foundPart, pospart.indexes,by="scenario1")
  pospartIN<-left_join(pospartIN, eligindexes.pospart,by="scenario1") 
  pospartPA<-left_join(eligPart.gen2,foundPart.gen2,by="scenario1")
  pospart<-left_join(pospartIN,pospartPA,by="scenario1") %>% 
    mutate_all(funs(stringr::str_replace(.,"0(0-0)","0")))
  
  
  kable(pospart, 
        col.names = c("Scenario","Found Wave 1 Partners", "HIV+ Wave 1 Partners",
                      "Eligible Pos. Partner Index","Eligible Wave 2 Partners","Found Wave 2 Partners"),
        caption = "Median (95\\% SI) Partner Service Measures: Wave 2 Partners") %>% kable_classic() %>% kable_styling(latex_options=c("hold_position"),position="left") %>% 
    footnote(general = "ND=newly diagnosed cases; PP=Previous positive cases; Wave 1=Partners to index cases", 
             general_title = "Note:",
             footnote_as_chunk = T)

  
  #Table -  Found Partners v PBT
  foundpartAll<-getYrMeanTbl(df, found.partners.all)
  
  foundvPBT<-left_join(foundPart, foundPart.gen2,by="scenario1")
  foundvPBT<-left_join(foundvPBT, foundpartAll,by="scenario1")
  foundvPBT<-left_join(foundvPBT, tot.pbt,by="scenario1")
  
  
  kable(foundvPBT, 
        col.names = c("Scenario","Found Wave 1 Partners", "Found Wave 2 Partners","All Partners (Unique)","Partner-based Tests"),
        caption = "Median (95\\% SI) Partner Service Measures: Wave 2 Partners") %>% kable_classic() %>% kable_styling(latex_options=c("hold_position"),position="left") %>% 
    footnote(general = "ND=newly diagnosed cases; PP=Previous positive cases; Wave 1=Partners to index cases", 
             general_title = "Note:",
             footnote_as_chunk = T)

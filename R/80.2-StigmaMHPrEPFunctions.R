#
# Functions related to MH outcomes
#


#Combined Factor Models (IPW) 
#-----------------------------------------------------------------------------------------
#step 1: add ipweights to dataset
ipwdat <- function(dat){
  #dat<-latvardat_ind
  dat <- dat %>%  fastDummies::dummy_cols(., select_columns = "stigma")
  
  #step 1: Regress smi on all confounders (logistic regression) & create ip weights
  smi.fit.denom <- glm(smi ~ stigma_1 + stigma_2 + stigma_3 + age1 + age2 + age3 +  
                         raceb + raceh + raceo + fb + poor + regw + regmw + regs + 
                         urblsu + urbsmu + urbr,
                family = "binomial", data=dat); summary(smi.fit.denom)
  smi.fit.numer <- glm(smi ~ 1,
                family = "binomial", data=dat); summary(smi.fit.numer)
  
  smi.pred.denom <- ifelse(dat$smi == 0, 1 - predict(smi.fit.denom, type = "response"),
                     predict(smi.fit.denom, type = "response"))
  smi.pred.numer <- ifelse(dat$smi == 0, 1 - predict(smi.fit.numer, type = "response"),
                     predict(smi.fit.numer, type = "response"))
  
  dat$smi.wt <- 1/smi.pred.denom; summary(dat$smi.wt);  sd(dat$smi.wt)
  dat$smi.swt <- smi.pred.numer/smi.pred.denom; summary(dat$smi.swt); sd(dat$smi.swt)

  
  #step 2: Regress sexual stigma on all confounders (multinomial logistic regression) & create ip weights
  dat$stigmacat <- relevel(as.factor(dat$stigma), ref = 4)
  levels(dat$stigmacat) <- c("high","anticipated_hc","fam_gensoc","minimal") 
  
  stigma.fit.denom <- nnet::multinom(stigmacat ~ age1 + age2 + age3 + raceb + raceh + raceo + 
                        fb + regw + regmw + regs + urblsu + urbsmu + urbr + eduhs + edupg,
                        data=dat); summary(stigma.fit.denom)
  stigma.fit.numer <- nnet::multinom(stigmacat ~ 1,
                                        data=dat); summary(stigma.fit.numer)
  
  stigmacat.pred.denom <- ifelse(dat$stigmacat == 4, 1 - predict(stigma.fit.denom, type = "prob"),
                           predict(stigma.fit.denom, type = "prob"))
  stigmacat.pred.numer <- ifelse(dat$stigmacat == 4, 1 - predict(stigma.fit.numer, type = "prob"),
                           predict(stigma.fit.numer, type = "prob"))
  
  dat$stigmacat.wt <- 1/stigmacat.pred.denom; summary(dat$stigmacat.wt);  sd(dat$stigmacat.wt)
  dat$stigmacat.swt <- stigmacat.pred.numer/stigmacat.pred.denom; summary(dat$stigmacat.swt); sd(dat$stigmacat.swt)
  
  
  #step 3: create combined stigma_mh weights
  dat$stigsmi.wt <- dat$smi.wt * dat$stigmacat.wt; summary(dat$stigsmi.wt); sd(dat$stigsmi.wt)
  dat$stigsmi.swt <- dat$smi.swt * dat$stigmacat.swt;summary(dat$stigsmi.swt); sd(dat$stigsmi.swt)
  
  dat <- dat %>% 
    mutate(id2 = as.character(id))
  
  return(as.data.frame(dat))
}




# Function: get ip weighted strata PRs
#-----------------------------------------------------------------------------------------
ipwStrata <- function(dat, outcome){
  #dat<-latvardat_indipw; outcome <- "p_used"

  #step 4: regress outcome on exposures and ipweights
  #crude
  ipwcrude.fit <- geepack::geeglm(
    formula = as.formula(paste0(outcome, "~ stigma_1 + stigma_2 + stigma_3 + smi + 
                                stigma_1*smi + stigma_2*smi + stigma_3*smi")),
    data=dat, family = poisson(link="log"), id=id2, corstr="independence"); #summary(ipwcrude.fit)
  
  beta <- coef(ipwcrude.fit)
  se <- coef(summary(ipwcrude.fit))[,2]
  lcl <- beta - 1.96*se 
  ucl <- beta + 1.96*se
  pval <- coef(summary(ipwcrude.fit))[,4]
  ipwcrude_tbl <- as.data.frame(cbind(est=exp(beta), ll=exp(lcl), ul=exp(ucl), pval=pval))%>% 
    tibble::rownames_to_column(var="var");ipwcrude_tbl
  
  
  K <- rbind("stigsmi11 v 41" = c(0, 1, 0, 0, 0, 1, 0, 0),
             "stigsmi21 v 41" = c(0, 0, 1, 0, 0, 0, 1, 0),
             "stigsmi31 v 41" = c(0, 0, 0, 1, 0, 0, 0, 1),
             "stigsmi10 v 40" = c(0, 1, 0, 0, 0, 0, 0, 0),
             "stigsmi20 v 40" = c(0, 0, 1, 0, 0, 0, 0, 0),
             "stigsmi30 v 40" = c(0, 0, 0, 1, 0, 0, 0, 0))
  
  contras <- doBy::esticon(ipwcrude.fit,K); #summary(contras)
  
  strataPRs.cru <- as.data.frame(cbind(exp(coef(contras)), exp(confint.default(contras)), contras$p.value)) %>% 
    tibble::rownames_to_column(.) %>% 
    mutate(stigma = substr(rowname, start=8, stop=8),
           smi = substr(rowname, start = 9, stop = 9)) %>%
    rename(est = V1,
           ll = `2.5 %`,
           ul = `97.5 %`,
           pval = V4) %>% 
    group_modify(~ add_row(.x, .before = 4)) %>% 
    mutate(stigma = ifelse(is.na(stigma), 4, stigma),
           smi = ifelse(is.na(smi), 1, smi)); strataPRs.cru
  
  
  #adjusted
  ipwadj.fit <- geepack::geeglm(
    formula = as.formula(paste0(outcome, "~ stigma_1 + stigma_2 + stigma_3 + smi + 
                                stigma_1*smi + stigma_2*smi + stigma_3*smi")),
    data=dat, family = poisson(link="log"), id=id2, corstr="independence", weights = stigsmi.swt); #summary(ipwadj.fit)
  
  beta <- coef(ipwadj.fit)
  se <- coef(summary(ipwadj.fit))[,2]
  lcl <- beta - 1.96*se 
  ucl <- beta + 1.96*se
  pval <- coef(summary(ipwadj.fit))[,4]
  ipwadj_tbl <- as.data.frame(cbind(est=exp(beta), ll=exp(lcl), ul=exp(ucl), pval=pval)) %>% 
    tibble::rownames_to_column(var="var");ipwadj_tbl
  
  K <- rbind("stigsmi11 v 41" = c(0, 1, 0, 0, 0, 1, 0, 0),
             "stigsmi21 v 41" = c(0, 0, 1, 0, 0, 0, 1, 0),
             "stigsmi31 v 41" = c(0, 0, 0, 1, 0, 0, 0, 1),
             "stigsmi10 v 40" = c(0, 1, 0, 0, 0, 0, 0, 0),
             "stigsmi20 v 40" = c(0, 0, 1, 0, 0, 0, 0, 0),
             "stigsmi30 v 40" = c(0, 0, 0, 1, 0, 0, 0, 0))
  
  contras <- doBy::esticon(ipwadj.fit,K); #summary(contras)
  
  strataPRs.adj <- as.data.frame(cbind(exp(coef(contras)), exp(confint.default(contras)), contras$p.value)) %>% 
    tibble::rownames_to_column(.) %>% 
    mutate(stigma = substr(rowname, start=8, stop=8),
           smi = substr(rowname, start = 9, stop = 9)) %>%
    rename(est = V1,
           ll = `2.5 %`,
           ul = `97.5 %`,
           pval = V4) %>% 
    group_modify(~ add_row(.x, .before = 4)) %>% 
    mutate(stigma = ifelse(is.na(stigma), 4, stigma),
           smi = ifelse(is.na(smi), 1, smi)); strataPRs.adj
  
  #merge tbls
  tbl1 <- as.data.frame(left_join(ipwcrude_tbl,ipwadj_tbl, by="var",suffix=c("_crude","_adj")));tbl1
  
  #bind strata tables
  tbl2 <- left_join(strataPRs.cru, 
                   strataPRs.adj, 
                   by = c("rowname", "stigma", "smi"), 
                   suffix = c(".crude", ".adj")) %>% 
    mutate(stigsmi = as.numeric(row_number()),
           smi = as.numeric(smi),
           stigma = as.numeric(stigma)); tbl2
  
  return(list(
    interaction.tbl = as.data.frame(tbl1),
    strata.tbl = as.data.frame(tbl2)))
}




# Function: get ip weighted comref PRs
#-----------------------------------------------------------------------------------------
ipwComref <- function(dat, outcome){
  #dat<-latvardat_indipw; outcome <- "p_used"
  
  #step 4: regress outcome on exposures and ipweights
  #crude
  ipwcrude.fit <- geepack::geeglm(
    formula = as.formula(paste0(outcome, "~ stigsmi11 + stigsmi21 + stigsmi31 + 
                               stigsmi41 + stigsmi10 + stigsmi20 + stigsmi30")),
    data=dat, family = poisson(link="log"), id=id2, corstr="independence"); #summary(ipwcrude.fit)
  
  beta <- coef(ipwcrude.fit)
  se <- coef(summary(ipwcrude.fit))[,2]
  lcl <- beta - 1.96*se 
  ucl <- beta + 1.96*se
  pval <- coef(summary(ipwcrude.fit))[,4]
  ipwcrude_tbl <- as.data.frame(cbind(est=exp(beta), ll=exp(lcl), ul=exp(ucl), pval=pval))%>% 
    tibble::rownames_to_column(var="var") %>% 
    filter(var != "(Intercept)") %>% 
    mutate(stigma = substr(var, start=8, stop=8),
           smi = substr(var, start = 9, stop = 9));ipwcrude_tbl
  
  
  #adjusted
  ipwadj.fit <- geepack::geeglm(
    formula = as.formula(paste0(outcome, "~ stigsmi11 + stigsmi21 + stigsmi31 + 
                               stigsmi41 + stigsmi10 + stigsmi20 + stigsmi30")),
    data=dat, family = poisson(link="log"), id=id2, corstr="independence", weights = stigsmi.swt); #summary(ipwadj.fit)
  
  beta <- coef(ipwadj.fit)
  se <- coef(summary(ipwadj.fit))[,2]
  lcl <- beta - 1.96*se 
  ucl <- beta + 1.96*se
  pval <- coef(summary(ipwadj.fit))[,4]
  ipwadj_tbl <- as.data.frame(cbind(est=exp(beta), ll=exp(lcl), ul=exp(ucl), pval=pval)) %>% 
    tibble::rownames_to_column(var="var")%>% 
    filter(var != "(Intercept)") %>% 
    mutate(stigma = substr(var, start=8, stop=8),
           smi = substr(var, start = 9, stop = 9));ipwadj_tbl
  
  #merge tbls
  tbl1 <- as.data.frame(left_join(ipwcrude_tbl,ipwadj_tbl, 
                                  by=c("var","stigma","smi"),suffix=c("_crude","_adj"))) %>% 
    mutate(stigsmi = as.numeric(row_number()));tbl1
  

  return(as.data.frame(tbl1))
}


#Plot Data: Get Strata Plot data
#-----------------------------------------------------------------------------------------
strataplotdat_fun <- function(dat){
  #dat <- p_used_cfstrata_assoc$strata.tbl
  bl_row <- c(8,4,0,1,NA,NA)
  
  plotdat <- dat %>%
    select(stigsmi, stigma, smi,est.adj, ll.adj, ul.adj) %>% 
    rbind(.,bl_row) %>% 
    mutate(est.adj = ifelse(stigma == 4 & smi == 1,1,est.adj)) %>% 
    mutate(stigma.cat = factor(stigma,
                               levels=c(1,2,3,4),
                               labels=c("High Sexual Stigma",
                                        "Ancipated Healthcare Stigma",
                                        "Fam./Gen. Social Stigma",
                                        "**Reference** Minimal Stigma")),
           smi.cat = factor(smi,
                            levels=c(1,0),
                            labels=c("MSM with SPD", "MSM without SPD"))) %>% 
    filter(stigma!=4)
  
  return(as.data.frame(plotdat))
}



#Plot Data: Get Comref Plot data
#-----------------------------------------------------------------------------------------
comrefplotdat_fun <- function(dat){
  #dat <- p_used_cfcomref_assoc
  rowbl <- c(stigsmi = 8, stigma = NA, smi = NA, est_adj = NA, ll_adj = NA, ul_adj = NA)
  rowref <- c(stigsmi = 9, stigma = 4, smi = 0, est_adj = 1, ll_adj = 1, ul_adj = 1)
  
  
  plotdat <- dat %>%
    select(stigsmi, stigma, smi,est_adj, ll_adj, ul_adj) %>% 
    rbind(.,rowbl,rowref) %>% 
    mutate(stigsmi.cat = factor(stigsmi,
                               levels=c(1,2,3,4,5,6,7,8,9),
                               labels=c("High SSP \n(SPD)",
                                        "Anticip. Healthcare SSP \n(SPD)",
                                        "Family/Gen. Social SSP \n(SPD)",
                                        "Minimal SSP \n(SPD)",
                                        "High SSP \n(No SPD)",
                                        "Anticip. Healthcare  SSP\n(No SPD)",
                                        "Fam./Gen. Social SSP \n(No SPD)",
                                        "",
                                        "**Reference** \nMinimal SSP \n(No SPD)"))) 
  
  return(as.data.frame(plotdat))
}



effDecomp <- function(dat,outcome){
  #dat<-ipwdat14;   outcome<-"p_used"
  dat <- dat %>% mutate(stigma2 = ifelse(stigma==4,0,1))
  
  #fit adjusted
  p_used_fit <- geepack::geeglm(formula = as.formula(paste0(outcome, "~ stigma2*smi")),
                                data=dat, family = poisson(link="log"), id=id2, 
                                corstr="independence", weights = stigsmi.swt); summary(p_used_fit)

  stigma_alone <- (exp(coef(p_used_fit)[2]) - 1) / (exp(coef(p_used_fit)[2] + coef(p_used_fit)[3] + coef(p_used_fit)[4]) - 1)
  smi_alone <- (exp(coef(p_used_fit)[3]) - 1) / (exp(coef(p_used_fit)[2] + coef(p_used_fit)[3] + coef(p_used_fit)[4]) - 1)
  reri <- exp(coef(p_used_fit)[2] + coef(p_used_fit)[3] + coef(p_used_fit)[4]) - exp(coef(p_used_fit)[2]) - exp(coef(p_used_fit)[3]) + 1
  stigsmi_inter <- reri/(exp(coef(p_used_fit)[2] + coef(p_used_fit)[3] + coef(p_used_fit)[4]))
  
  #merge them
  tbl <- as.data.frame(cbind(outcome=outcome, 
                             stigma=as.numeric(stigma_alone), 
                             smi=as.numeric(smi_alone), 
                             reri = as.numeric(reri),
                             stigsmi=as.numeric(stigsmi_inter)))
  
  return(as.data.frame(tbl))
}


getSampleJED <- function(dat){
  p_used <- effDecomp(dat, "p_used")
  p_noadh <-effDecomp(dat, "p_noadh")
  p_per6 <- effDecomp(dat, "p_per6")
  p_per12 <- effDecomp(dat, "p_per12")
  
  tbl <- as.data.frame(rbind(p_used,p_noadh,p_per6,p_per12)) %>% 
    mutate(stigma=as.numeric(stigma),
           smi=as.numeric(smi),
           reri = as.numeric(reri),
           stigsmi=as.numeric(stigsmi)) %>% 
    mutate_if(is.numeric, ~round(.x, 2))
  
  return(as.data.frame(tbl))
}

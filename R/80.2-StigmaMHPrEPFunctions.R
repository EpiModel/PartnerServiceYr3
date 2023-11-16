#
# Functions related to MH outcomes
#



#Add IP Weights to study data
#-----------------------------------------------------------------------------------------
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







#Get IP weighted comref PRs
#-----------------------------------------------------------------------------------------
ipwComref <- function(dat, outcome){
  #dat<-dat_reclass_ipw; outcome <- "p_used"
  
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
  
  return(as.data.frame(ipwadj_tbl))
}



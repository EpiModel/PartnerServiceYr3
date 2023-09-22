#
# Functions related to MH outcomes
#

# Function: Prevalences
#-----------------------------------------------------------------------------------------
getPrevalences <- function(dat, outcome){
  #dat<-latvardat2 ; outcome <- sui_thk
  
  tbl <- as.data.frame(left_join(
    check<-dat %>% 
      group_by(stigpoor) %>% 
      summarise(n_nmiss = length({{outcome}}[!is.na({{outcome}})]),
                outcome_ct = length(which({{outcome}} == 1)),
                outcome_pct = mean({{outcome}}, na.rm=T)*100),
    
    dat %>% select(stigma, poor, stigpoor) %>% distinct() %>% 
      arrange(stigpoor),
    
    by = "stigpoor")) %>% 
    mutate(stigpoor = as.numeric(stigpoor)) %>% 
    select(stigpoor, stigma, poor, everything())
    
    return(tbl)
}



#Function to check interaction by poor (wald)
#-----------------------------------------------------------------------------------------
checkInterPoor <- function(dat, outcome){
  #dat<-latvardat2; outcome<-"smi"
  
  library("lmtest")
  library("sandwich")
  
  dat <- dat %>% mutate(stigma = factor(stigma, levels=c("4","1","2","3")))
  fit <- glm(formula = paste0(outcome, " ~ ", "stigma*poor", collapse = ""),
             dat, family = poisson(link="log")); summary(fit)
  
  results <- coeftest(fit, vcov = sandwich)
  tbl <- as.data.frame(results[,4]) %>%
    tibble::rownames_to_column(., "rowname" ) %>% 
    rename(p = 2) %>% 
    filter(grepl(":poor",rowname)) %>% 
    mutate(deci = ifelse(p<0.05, "Yes", ""))
  
  return(as.data.frame(tbl))
}
#-----------------------



# Function: get strata PRs
#-----------------------------------------------------------------------------------------
getStrataPRs <- function(dat, outcome){

  #crude model
  cfit <- glm(formula = paste0(outcome, " ~ ", "stigma_1 + stigma_2 + stigma_3 + poor + 
                               stigma_1*poor + stigma_2*poor + stigma_3*poor", collapse = ""), 
                   dat, family = poisson(link="log")); summary(cfit)

  K <- rbind("stigpoor11 v 41" = c(0, 1, 0, 0, 0, 1, 0, 0),
             "stigpoor21 v 41" = c(0, 0, 1, 0, 0, 0, 1, 0),
             "stigpoor31 v 41" = c(0, 0, 0, 1, 0, 0, 0, 1),
             "stigpoor10 v 40" = c(0, 1, 0, 0, 0, 0, 0, 0),
             "stigpoor20 v 40" = c(0, 0, 1, 0, 0, 0, 0, 0),
             "stigpoor30 v 40" = c(0, 0, 0, 1, 0, 0, 0, 0))
  
  contras <- multcomp::glht(cfit, linfct = K); summary(contras)
  
  strataPRs.cru <- as.data.frame(cbind(exp(coef(contras)), exp(confint.default(contras)))) %>% 
    tibble::rownames_to_column(.) %>% 
    mutate(stigma = substr(rowname, start=9, stop=9),
           poor = substr(rowname, start = 10, stop = 10)) %>%
    rename(est = V1,
           ll = `2.5 %`,
           ul = `97.5 %`) %>% 
    group_modify(~ add_row(.x, .before = 4)) %>% 
    mutate(stigma = ifelse(is.na(stigma), 4, stigma),
           poor = ifelse(is.na(poor), 1, poor)); strataPRs.cru
  
  
  
  # adjusted model
  afit <- glm(formula = paste0(outcome," ~ ", "stigma_1 + stigma_2 + stigma_3 + poor + 
                               stigma_1*poor + stigma_2*poor + stigma_3*poor + 
                               age1 + age2 + age3 + raceb + raceh + raceo + fb +  
                               regw + regmw + regs + urblsu + urbsmu + urbr", collapse = ""),
                   data = dat, family = poisson(link="log")); summary(afit)

  K <- rbind("stigpoor11 v 41" = c(0, 1, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   1, 0, 0),
             "stigpoor21 v 41" = c(0, 0, 1, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0, 1, 0),
             "stigpoor31 v 41" = c(0, 0, 0, 1, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0, 0, 1),
             "stigpoor10 v 40" = c(0, 1, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0, 0, 0),
             "stigpoor20 v 40" = c(0, 0, 1, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0, 0, 0),
             "stigpoor30 v 40" = c(0, 0, 0, 1, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0, 0, 0))
  
  contras <- multcomp::glht(afit, linfct = K); summary(contras)
  
  strataPRs.adj<- as.data.frame(cbind(exp(coef(contras)), exp(confint.default(contras)))) %>% 
    tibble::rownames_to_column(.) %>% 
    mutate(stigma = substr(rowname, start=9, stop=9),
           poor = substr(rowname, start = 10, stop = 10)) %>%
    rename(est = V1,
           ll = `2.5 %`,
           ul = `97.5 %`) %>% 
    group_modify(~ add_row(.x, .before = 4)) %>% 
    mutate(stigma = ifelse(is.na(stigma), 4, stigma),
           poor = ifelse(is.na(poor), 1, poor)); strataPRs.adj
  
 
  #bind strata tables
  tbl <- left_join(strataPRs.cru, 
                   strataPRs.adj, 
                   by = c("rowname", "stigma", "poor"), 
                   suffix = c(".crude", ".adj")) %>% 
    mutate(stigpoor = as.numeric(row_number()),
           poor = as.numeric(poor),
           stigma = as.numeric(stigma)); tbl
  
  return(as.data.frame(tbl))
  
}



# Function: get common ref PRs
#-----------------------------------------------------------------------------------------
getComRefPRs <- function(dat, outcome){
  
  #crude model
  cfit <- glm(formula = paste0(outcome, " ~ ", "stigpoor11 + stigpoor21 + stigpoor31 + 
                               stigpoor41 + stigpoor10 + stigpoor20 + stigpoor30", collapse = ""), 
              dat, family = poisson(link="log")); summary(cfit)
  
  K <- rbind("stigpoor11 v 40" = c(0, 1, 0, 0, 0, 0, 0, 0),    
             "stigpoor21 v 40" = c(0, 0, 1, 0, 0, 0, 0, 0),
             "stigpoor31 v 40" = c(0, 0, 0, 1, 0, 0, 0, 0),
             "stigpoor41 v 40" = c(0, 0, 0, 0, 1, 0, 0, 0),
             "stigpoor10 v 40" = c(0, 0, 0, 0, 0, 1, 0, 0),
             "stigpoor20 v 40" = c(0, 0, 0, 0, 0, 0, 1, 0),
             "stigpoor30 v 40" = c(0, 0, 0, 0, 0, 0, 0, 1))
  
  contras <- multcomp::glht(cfit, linfct = K); summary(contras)
  
  comRefPRs.cru <- as.data.frame(cbind(exp(coef(contras)), exp(confint.default(contras)))) %>% 
    tibble::rownames_to_column(.) %>% 
    mutate(stigma = substr(rowname, start=9, stop=9),
           poor = substr(rowname, start = 10, stop = 10)) %>%
    rename(est = V1,
           ll = `2.5 %`,
           ul = `97.5 %`) ; comRefPRs.cru
  
  
  
  # adjusted model
  afit <- glm(formula = paste0(outcome, " ~ ", "stigpoor11 + stigpoor21 + stigpoor31 + 
                               stigpoor41 + stigpoor10 + stigpoor20 + stigpoor30 + 
                               age1 + age2 + age3 + raceb + raceh + raceo + fb +  
                               regw + regmw + regs + urblsu + urbsmu + urbr", collapse = ""), 
                    dat, family = poisson(link="log")); summary(afit)
  
  K <- rbind("stigpoor11 v 40" = c(0, 1, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0),    
             "stigpoor21 v 40" = c(0, 0, 1, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0),
             "stigpoor31 v 40" = c(0, 0, 0, 1, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0),
             "stigpoor41 v 40" = c(0, 0, 0, 0, 1, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0),
             "stigpoor10 v 40" = c(0, 0, 0, 0, 0, 1, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0),
             "stigpoor20 v 40" = c(0, 0, 0, 0, 0, 0, 1, 0,  0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0),
             "stigpoor30 v 40" = c(0, 0, 0, 0, 0, 0, 0, 1,  0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0))
  
  contras <- multcomp::glht(afit, linfct = K); summary(contras)
  
  comrefPRs.adj <- as.data.frame(cbind(exp(coef(contras)), exp(confint.default(contras)))) %>% 
    tibble::rownames_to_column(.) %>% 
    mutate(stigma = substr(rowname, start=9, stop=9),
           poor = substr(rowname, start = 10, stop = 10)) %>%
    rename(est = V1,
           ll = `2.5 %`,
           ul = `97.5 %`) ; comrefPRs.adj
  
  
  #bind strata tables
  tbl <- left_join(comRefPRs.cru, 
                   comrefPRs.adj, 
                   by = c("rowname", "stigma", "poor"), 
                   suffix = c(".crude", ".adj")) %>% 
    mutate(stigpoor = as.numeric(row_number()),
           poor = as.numeric(poor),
           stigma = as.numeric(stigma)); tbl
  
  return(as.data.frame(tbl))
  
}


# Function: get RERI_RR with CI using bootstrapped method
reri_bootci <- function(df, outcome){
  #df <- df_14; M<-10; outcome<-"smi"
  
  #get reri
  fit <- glm(formula = paste0(outcome," ~ ", "stigma_rec*poor +  
                               age1 + age2 + age3 + raceb + raceh + raceo + fb +  
                               regw + regmw + regs + urblsu + urbsmu + urbr", collapse = ""),
             data = df, family = poisson(link="log")); summary(fit)
  
  reri <- exp(coef(fit)[2] + coef(fit)[3] + coef(fit)[17]) - exp(coef(fit)[2]) - exp(coef(fit)[3]) + 1
  ap <- reri/(exp(coef(fit)[2] + coef(fit)[3] + coef(fit)[17]))
  si <- (exp(coef(fit)[2] + coef(fit)[3] + coef(fit)[17]) - 1)/(( exp(coef(fit)[2]) - 1) + (exp(coef(fit)[3]) - 1))
  multinter <- exp(coef(fit)[2] + coef(fit)[3] + coef(fit)[17])/(exp(coef(fit)[2]) * exp(coef(fit)[3]))
  
  tbl <- as.data.frame(cbind(reri, ap, multinter))

  return(list(tbl = as.data.frame(tbl)))
  
}


getRERIs <- function(dat, outcome){
  #dat<-latvardat2; outcome<-"smi"; M<-50
  
  df_14 <- dat %>% filter(stigma %in% c(1,4)) %>% mutate(stigma_rec = ifelse(stigma==4, 0, 1))
  df_24 <- dat %>% filter(stigma %in% c(2,4)) %>% mutate(stigma_rec = ifelse(stigma==4, 0, 1))
  df_34 <- dat %>% filter(stigma %in% c(3,4)) %>% mutate(stigma_rec = ifelse(stigma==4, 0, 1))
  
  sp14_vcomref <- reri_bootci(df_14, outcome)
  sp24_vcomref <- reri_bootci(df_24, outcome)
  sp34_vcomref <- reri_bootci(df_34, outcome)
  
  reri_tbl <- (rbind(sp14_vcomref$tbl, sp24_vcomref$tbl, sp34_vcomref$tbl)) %>% 
    tibble::remove_rownames() %>% 
    mutate(stigpoor = as.numeric(row_number())) %>% 
    select(stigpoor, everything())
  
  return(list(
    reri_tbl = reri_tbl))
}

#
# Association between sexual stigma and mental health distress
# step 4: Misclassification bias adjustment of risk model 


#rm(list = ls())


# libraries
install.packages("MCMCprecision")
install.packages("fastDummies")
library("tidyr")
library("dplyr")
library("ggplot2")



mplus_dir <- paste0("data/aim1/mplusdat_prep")
save_dir <- paste0("data/aim1/output")



#Get validation data 
#-----------------------------------------------------------------------------------------
p1_c_valdat <- readRDS(paste0(mplus_dir, "/p1_c_valdat.rds"))
p1_nc_valdat <- readRDS(paste0(mplus_dir, "/p1_nc_valdat.rds"))



#Run PBA
#-----------------------------------------------------------------------------------------
source("R/80.2-StigmaMHPrEPFunctions.R")

#shape stats  - beta dist and dirichlet dist
p1_c_alpha_c1 <- p1_c_valdat[1,1]; p1_c_tot_c1 <- p1_c_valdat[1,5]
p1_c_alpha_c2 <- p1_c_valdat[2,2]; p1_c_tot_c2 <- p1_c_valdat[2,5]
p1_c_alpha_c3 <- p1_c_valdat[3,3]; p1_c_tot_c3 <- p1_c_valdat[3,5]
p1_c_alpha_c4 <- p1_c_valdat[4,4]; p1_c_tot_c4 <- p1_c_valdat[4,5]
p1_c_rclv_c1 <- c(p1_c_valdat[1,2],p1_c_valdat[1,3], p1_c_valdat[1,4])
p1_c_rclv_c2 <- c(p1_c_valdat[2,1],p1_c_valdat[2,3], p1_c_valdat[2,4])
p1_c_rclv_c3 <- c(p1_c_valdat[3,1],p1_c_valdat[3,2], p1_c_valdat[3,4])
p1_c_rclv_c4 <- c(p1_c_valdat[4,1],p1_c_valdat[4,2], p1_c_valdat[4,3])

p1_nc_alpha_c1 <- p1_nc_valdat[1,1]; p1_nc_tot_c1 <- p1_nc_valdat[1,5]
p1_nc_alpha_c2 <- p1_nc_valdat[2,2]; p1_nc_tot_c2 <- p1_nc_valdat[2,5]
p1_nc_alpha_c3 <- p1_nc_valdat[3,3]; p1_nc_tot_c3 <- p1_nc_valdat[3,5]
p1_nc_alpha_c4 <- p1_nc_valdat[4,4]; p1_nc_tot_c4 <- p1_nc_valdat[4,5]
p1_nc_rclv_c1 <- c(p1_nc_valdat[1,2],p1_nc_valdat[1,3], p1_nc_valdat[1,4])
p1_nc_rclv_c2 <- c(p1_nc_valdat[2,1],p1_nc_valdat[2,3], p1_nc_valdat[2,4])
p1_nc_rclv_c3 <- c(p1_nc_valdat[3,1],p1_nc_valdat[3,2], p1_nc_valdat[3,4])
p1_nc_rclv_c4 <- c(p1_nc_valdat[4,1],p1_nc_valdat[4,2], p1_nc_valdat[4,3])


#iterations and empty vectors/dfs
M <- 10 * 100

p1_c_ppv <- p1_nc_ppv <-as.data.frame(matrix(NA, M, 4))
p1_c_rclp_c1 <- p1_c_rclp_c2 <- p1_c_rclp_c3  <- p1_c_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
p1_nc_rclp_c1 <- p1_nc_rclp_c2 <- p1_nc_rclp_c3  <- p1_nc_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
p1_c_clchg_prp <- p1_nc_clchg_prp <- rep(NA, M)


latvardat <- readRDS(paste0(mplus_dir, "/newdata.rds")) %>% mutate(stigma=N) 
names(latvardat) <- tolower(names(latvardat))

p1_c_dat <- latvardat %>% filter(p_used == 1)
p1_nc_dat <- latvardat %>% filter(p_used == 0)


p1_c_stigma_orig <- p1_c_dat$stigma
p1_c_index_ppv <- p1_c_index_class <- p1_c_keepclass <- rep(NA, nrow(p1_c_dat))
p1_c_stigma_new <- rep(NA, nrow(p1_c_dat))

p1_nc_stigma_orig <- p1_nc_dat$stigma
p1_nc_index_ppv <- p1_nc_index_class <- p1_nc_keepclass <- rep(NA, nrow(p1_nc_dat))
p1_nc_stigma_new <- rep(NA, nrow(p1_nc_dat))


p1_pusedadj <- as.data.frame(matrix(NA, M, 7))
p1_pusedadj.boot <- as.data.frame(matrix(NA, M, 7))



#pba
set.seed(123)

for (i in 1: M) {
  #i<-1
  #A. Generate ppvs 
  p1_c_ppv[i,] <- c(rbeta(1, p1_c_alpha_c1, p1_c_tot_c1 - p1_c_alpha_c1), 
                 rbeta(1, p1_c_alpha_c2, p1_c_tot_c2 - p1_c_alpha_c2), 
                 rbeta(1, p1_c_alpha_c3, p1_c_tot_c3 - p1_c_alpha_c3), 
                 rbeta(1, p1_c_alpha_c4, p1_c_tot_c4 - p1_c_alpha_c4))
  
  p1_nc_ppv[i,] <- c(rbeta(1, p1_nc_alpha_c1, p1_nc_tot_c1 - p1_nc_alpha_c1), 
                  rbeta(1, p1_nc_alpha_c2, p1_nc_tot_c2 - p1_nc_alpha_c2), 
                  rbeta(1, p1_nc_alpha_c3, p1_nc_tot_c3 - p1_nc_alpha_c3), 
                  rbeta(1, p1_nc_alpha_c4, p1_nc_tot_c4 - p1_nc_alpha_c4))
  
  
  #B. Generate reclassification probs
  p1_c_rclp_c1[i,] <- MCMCprecision::rdirichlet(1, p1_c_rclv_c1)
  p1_c_rclp_c2[i,] <- MCMCprecision::rdirichlet(1, p1_c_rclv_c2)
  p1_c_rclp_c3[i,] <- MCMCprecision::rdirichlet(1, p1_c_rclv_c3)
  p1_c_rclp_c4[i,] <- MCMCprecision::rdirichlet(1, p1_c_rclv_c4)
  p1_c_rclp_tbl    <- as.data.frame(
    rbind(
      c(1, NA,                p1_c_rclp_c1[i,1], p1_c_rclp_c1[i,2], p1_c_rclp_c1[i,3]),
      c(2, p1_c_rclp_c2[i,1], NA,                p1_c_rclp_c2[i,2], p1_c_rclp_c2[i,3]),
      c(3, p1_c_rclp_c3[i,1], p1_c_rclp_c3[i,2], NA,                p1_c_rclp_c3[i,3]),
      c(4, p1_c_rclp_c4[i,1], p1_c_rclp_c4[i,2], p1_c_rclp_c4[i,3], NA)
    )
  ) 
  colnames(p1_c_rclp_tbl) = c("index_class","1","2","3","4")
  
  
  p1_nc_rclp_c1[i,] <- MCMCprecision::rdirichlet(1, p1_nc_rclv_c1)
  p1_nc_rclp_c2[i,] <- MCMCprecision::rdirichlet(1, p1_nc_rclv_c2)
  p1_nc_rclp_c3[i,] <- MCMCprecision::rdirichlet(1, p1_nc_rclv_c3)
  p1_nc_rclp_c4[i,] <- MCMCprecision::rdirichlet(1, p1_nc_rclv_c4)
  p1_nc_rclp_tbl    <- as.data.frame(
    rbind(
      c(1, NA,                 p1_nc_rclp_c1[i,1], p1_nc_rclp_c1[i,2], p1_nc_rclp_c1[i,3]),
      c(2, p1_nc_rclp_c2[i,1], NA,                 p1_nc_rclp_c2[i,2], p1_nc_rclp_c2[i,3]),
      c(3, p1_nc_rclp_c3[i,1], p1_nc_rclp_c3[i,2], NA,                 p1_nc_rclp_c3[i,3]),
      c(4, p1_nc_rclp_c4[i,1], p1_nc_rclp_c4[i,2], p1_nc_rclp_c4[i,3], NA)
    )
  ) 
  colnames(p1_nc_rclp_tbl) = c("index_class","1","2","3","4")
  
  
  #C. Determine whether to reclassify each obs
  #cases
  for (j in 1:nrow(p1_c_dat)){
    #j<-1
    #trial (keep or reclassify)
    p1_c_index_class[j] = p1_c_stigma_orig[j]                          #get stigma class for index obs
    p1_c_index_ppv[j]   = p1_c_ppv[i, p1_c_index_class[j]]                #get ppv corresp. to class
    p1_c_keepclass[j] = rbinom(1, 1, p1_c_index_ppv[j])                #Bernoulli trial to keep or reclassify
    
    if (p1_c_keepclass[j] == 1){
      p1_c_stigma_new[j] <- p1_c_index_class[j]
    }
    
    if (p1_c_keepclass[j] == 0){
      p1_c_p_tbl <- as.data.frame(t(p1_c_rclp_tbl[p1_c_index_class[j], ])) %>% 
        tibble::rownames_to_column(var = "index") %>% 
        rename(p = 2) %>% 
        filter(index != "index_class" & !is.na(p)) %>% 
        arrange(p)
      
      p1_c_ran.val <-runif(1)
      
      p1_c_stigma_new[j] <- as.numeric(
        ifelse(p1_c_ran.val <= p1_c_p_tbl$p[1], p1_c_p_tbl$index[1],
               ifelse(p1_c_ran.val > p1_c_p_tbl$p[1] & 
                        p1_c_ran.val <= p1_c_p_tbl$p[2], p1_c_p_tbl$index[2], p1_c_p_tbl$index[3])))
    }
  }
  

  #non-cases
  for (j in 1:nrow(p1_nc_dat)){
    #trial (keep or reclassify)
    p1_nc_index_class[j] = p1_nc_stigma_orig[j]                          #get stigma class for index obs
    p1_nc_index_ppv[j]   = p1_nc_ppv[i, p1_nc_index_class[j]]                #get ppv corresp. to class
    p1_nc_keepclass[j] = rbinom(1, 1, p1_nc_index_ppv[j])                #Bernoulli trial to keep or reclassify
    
    if (p1_nc_keepclass[j] == 1){
      p1_nc_stigma_new[j] <- p1_nc_index_class[j]
    }
    
    if (p1_nc_keepclass[j] == 0){
      p1_nc_p_tbl <- as.data.frame(t(p1_nc_rclp_tbl[p1_nc_index_class[j], ])) %>% 
        tibble::rownames_to_column(var = "index") %>% 
        rename(p = 2) %>% 
        filter(index != "index_class" & !is.na(p)) %>% 
        arrange(p)
      
      p1_nc_ran.val <-runif(1)
      
      p1_nc_stigma_new[j] <- as.numeric(
        ifelse(p1_nc_ran.val <= p1_nc_p_tbl$p[1], p1_nc_p_tbl$index[1],
               ifelse(p1_nc_ran.val > p1_nc_p_tbl$p[1] & 
                        p1_nc_ran.val <= p1_nc_p_tbl$p[2], p1_nc_p_tbl$index[2], p1_nc_p_tbl$index[3])))
    }
  }
  
  p1_c_clchg_prp[i] <- 1 - mean(p1_c_keepclass)     #get prp of records reclassified
  p1_nc_clchg_prp[i] <- 1 - mean(p1_nc_keepclass)
  
  p1_c_dat_reclass <- cbind(p1_c_dat, p1_c_stigma_new)    #merge new class with dat
  p1_nc_dat_reclass <- cbind(p1_nc_dat, p1_nc_stigma_new)
  
  #create reconstructed data and create the stigpoor var with new stigma variable
  dat_reclass <- rbind(p1_c_dat_reclass %>% rename(stigma_new = p1_c_stigma_new), 
                       p1_nc_dat_reclass %>% rename(stigma_new = p1_nc_stigma_new)) %>% 
    rename(stigma_orig = stigma)
  names(dat_reclass) <- tolower(names(dat_reclass))
  
  exp_grps <- tidyr::crossing(stigma = dat_reclass$stigma_new, smi = dat_reclass$smi) %>% 
    arrange(desc(smi), stigma) %>% 
    mutate(stigsmi = as.character(row_number())) %>% 
    mutate(stigsmi.cat = paste0("stigsmi", stigsmi, sep = ""))
  
  
  dat_reclass2 <- left_join(dat_reclass, exp_grps, by = c("stigma_new" = "stigma","smi")) %>% 
    rename(stigma = stigma_new) %>% 
    fastDummies::dummy_cols(., select_columns = "stigma") %>% 
    fastDummies::dummy_cols(., select_columns = "stigsmi") %>% 
    rename(stigsmi11 = stigsmi_1,
           stigsmi21 = stigsmi_2,
           stigsmi31 = stigsmi_3,
           stigsmi41 = stigsmi_4,
           stigsmi10 = stigsmi_5,
           stigsmi20 = stigsmi_6,
           stigsmi30 = stigsmi_7,
           stigsmi40 = stigsmi_8) 
  
  #outcome regression with reconstructed data 
  #add ipweights
  dat_reclass2_ipw <- ipwdat(dat_reclass2)
  
  #poisson gee with comref (for bias-adjusted estimates & 95% SI incorporating error from bias params only)
  p1_pusedadj[i,] <- ipwComref(dat_reclass2_ipw, "p_used")$est_adj 
  
  
  #bias-adjusted estimates & 95% CI incorporating total error
    #get bootstrapped sample
    bootsample.ids <- sample(1:nrow(dat_reclass2), size=nrow(dat_reclass2), replace = T)
    bootsample <- dat_reclass2[bootsample.ids,]
    
    #add ipweights
    bootsample_ipw <- ipwdat(bootsample)
    
    #poisson gee with comref (total error)
    p1_pusedadj.boot[i,] <- ipwComref(bootsample_ipw, "p_used")$est_adj 
    
}



#save products
#reg est
saveRDS(p1_pusedadj, paste0(save_dir, "/p1_pusedadj.rds"))
saveRDS(p1_pusedadj.boot, paste0(save_dir, "/p1_pusedadj.boot.rds"))

#proportion of observations reclassified per iteration
prp_obs_chg <- as.data.frame(cbind(p1_c_clchg_prp, p1_nc_clchg_prp)) 
saveRDS(prp_obs_chg, paste0(save_dir, "/p1_prp_obs_chg.rds"))


# p1_c_df<-as.data.frame(cbind(
#   p1_c_stigma_orig = p1_c_stigma_orig, 
#   p1_c_index_class = p1_c_index_class, 
#   p1_c_index_ppv = p1_c_index_ppv, 
#   p1_c_keepclass = p1_c_keepclass,
#   p1_c_stigma_new = p1_c_stigma_new))
# saveRDS(p1_c_df, paste0(save_dir, "/p1_c_df.rds"))
# 
# 
# p1_nc_df<-as.data.frame(cbind(
#   p1_nc_stigma_orig = p1_nc_stigma_orig, 
#   p1_nc_index_class = p1_nc_index_class, 
#   p1_nc_index_ppv = p1_nc_index_ppv, 
#   p1_nc_keepclass = p1_nc_keepclass,
#   p1_nc_stigma_new = p1_nc_stigma_new))
# saveRDS(p1_nc_df, paste0(save_dir, "/p1_nc_df.rds"))

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
p4_c_valdat <- readRDS(paste0(mplus_dir, "/p4_c_valdat.rds"))
p4_nc_valdat <- readRDS(paste0(mplus_dir, "/p4_nc_valdat.rds"))



#Run PBA
#-----------------------------------------------------------------------------------------
source("R/80.2-StigmaMHPrEPFunctions.R")

#shape stats  - beta dist and dirichlet dist
p4_c_alpha_c1 <- p4_c_valdat[1,1]; p4_c_tot_c1 <- p4_c_valdat[1,5]
p4_c_alpha_c2 <- p4_c_valdat[2,2]; p4_c_tot_c2 <- p4_c_valdat[2,5]
p4_c_alpha_c3 <- p4_c_valdat[3,3]; p4_c_tot_c3 <- p4_c_valdat[3,5]
p4_c_alpha_c4 <- p4_c_valdat[4,4]; p4_c_tot_c4 <- p4_c_valdat[4,5]
p4_c_rclv_c1 <- c(p4_c_valdat[1,2],p4_c_valdat[1,3], p4_c_valdat[1,4])
p4_c_rclv_c2 <- c(p4_c_valdat[2,1],p4_c_valdat[2,3], p4_c_valdat[2,4])
p4_c_rclv_c3 <- c(p4_c_valdat[3,1],p4_c_valdat[3,2], p4_c_valdat[3,4])
p4_c_rclv_c4 <- c(p4_c_valdat[4,1],p4_c_valdat[4,2], p4_c_valdat[4,3])

p4_nc_alpha_c1 <- p4_nc_valdat[1,1]; p4_nc_tot_c1 <- p4_nc_valdat[1,5]
p4_nc_alpha_c2 <- p4_nc_valdat[2,2]; p4_nc_tot_c2 <- p4_nc_valdat[2,5]
p4_nc_alpha_c3 <- p4_nc_valdat[3,3]; p4_nc_tot_c3 <- p4_nc_valdat[3,5]
p4_nc_alpha_c4 <- p4_nc_valdat[4,4]; p4_nc_tot_c4 <- p4_nc_valdat[4,5]
p4_nc_rclv_c1 <- c(p4_nc_valdat[1,2],p4_nc_valdat[1,3], p4_nc_valdat[1,4])
p4_nc_rclv_c2 <- c(p4_nc_valdat[2,1],p4_nc_valdat[2,3], p4_nc_valdat[2,4])
p4_nc_rclv_c3 <- c(p4_nc_valdat[3,1],p4_nc_valdat[3,2], p4_nc_valdat[3,4])
p4_nc_rclv_c4 <- c(p4_nc_valdat[4,1],p4_nc_valdat[4,2], p4_nc_valdat[4,3])


#iterations and empty vectors/dfs
M <- 10 * 100

p4_c_ppv <- p4_nc_ppv <-as.data.frame(matrix(NA, M, 4))
p4_c_rclp_c1 <- p4_c_rclp_c2 <- p4_c_rclp_c3  <- p4_c_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
p4_nc_rclp_c1 <- p4_nc_rclp_c2 <- p4_nc_rclp_c3  <- p4_nc_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
p4_c_clchg_prp <- p4_nc_clchg_prp <- rep(NA, M)


latvardat <- readRDS(paste0(mplus_dir, "/newdata.rds")) %>% mutate(stigma=N) 
names(latvardat) <- tolower(names(latvardat))

p4_c_dat <- latvardat %>% filter(p_per12 == 1)
p4_nc_dat <- latvardat %>% filter(p_per12 == 0)


p4_c_stigma_orig <- p4_c_dat$stigma
p4_c_index_ppv <- p4_c_index_class <- p4_c_keepclass <- rep(NA, nrow(p4_c_dat))
p4_c_stigma_new <- rep(NA, nrow(p4_c_dat))

p4_nc_stigma_orig <- p4_nc_dat$stigma
p4_nc_index_ppv <- p4_nc_index_class <- p4_nc_keepclass <- rep(NA, nrow(p4_nc_dat))
p4_nc_stigma_new <- rep(NA, nrow(p4_nc_dat))


p4_pper12adj <- as.data.frame(matrix(NA, M, 7))
p4_pper12adj.boot <- as.data.frame(matrix(NA, M, 7))



#pba
set.seed(123)

for (i in 1: M) {
  #i<-1
  #A. Generate ppvs 
  p4_c_ppv[i,] <- c(rbeta(1, p4_c_alpha_c1, p4_c_tot_c1 - p4_c_alpha_c1), 
                 rbeta(1, p4_c_alpha_c2, p4_c_tot_c2 - p4_c_alpha_c2), 
                 rbeta(1, p4_c_alpha_c3, p4_c_tot_c3 - p4_c_alpha_c3), 
                 rbeta(1, p4_c_alpha_c4, p4_c_tot_c4 - p4_c_alpha_c4))
  
  p4_nc_ppv[i,] <- c(rbeta(1, p4_nc_alpha_c1, p4_nc_tot_c1 - p4_nc_alpha_c1), 
                  rbeta(1, p4_nc_alpha_c2, p4_nc_tot_c2 - p4_nc_alpha_c2), 
                  rbeta(1, p4_nc_alpha_c3, p4_nc_tot_c3 - p4_nc_alpha_c3), 
                  rbeta(1, p4_nc_alpha_c4, p4_nc_tot_c4 - p4_nc_alpha_c4))
  
  
  #B. Generate reclassification probs
  p4_c_rclp_c1[i,] <- MCMCprecision::rdirichlet(1, p4_c_rclv_c1)
  p4_c_rclp_c2[i,] <- MCMCprecision::rdirichlet(1, p4_c_rclv_c2)
  p4_c_rclp_c3[i,] <- MCMCprecision::rdirichlet(1, p4_c_rclv_c3)
  p4_c_rclp_c4[i,] <- MCMCprecision::rdirichlet(1, p4_c_rclv_c4)
  p4_c_rclp_tbl    <- as.data.frame(
    rbind(
      c(1, NA,                p4_c_rclp_c1[i,1], p4_c_rclp_c1[i,2], p4_c_rclp_c1[i,3]),
      c(2, p4_c_rclp_c2[i,1], NA,                p4_c_rclp_c2[i,2], p4_c_rclp_c2[i,3]),
      c(3, p4_c_rclp_c3[i,1], p4_c_rclp_c3[i,2], NA,                p4_c_rclp_c3[i,3]),
      c(4, p4_c_rclp_c4[i,1], p4_c_rclp_c4[i,2], p4_c_rclp_c4[i,3], NA)
    )
  ) 
  colnames(p4_c_rclp_tbl) = c("index_class","1","2","3","4")
  
  
  p4_nc_rclp_c1[i,] <- MCMCprecision::rdirichlet(1, p4_nc_rclv_c1)
  p4_nc_rclp_c2[i,] <- MCMCprecision::rdirichlet(1, p4_nc_rclv_c2)
  p4_nc_rclp_c3[i,] <- MCMCprecision::rdirichlet(1, p4_nc_rclv_c3)
  p4_nc_rclp_c4[i,] <- MCMCprecision::rdirichlet(1, p4_nc_rclv_c4)
  p4_nc_rclp_tbl    <- as.data.frame(
    rbind(
      c(1, NA,                 p4_nc_rclp_c1[i,1], p4_nc_rclp_c1[i,2], p4_nc_rclp_c1[i,3]),
      c(2, p4_nc_rclp_c2[i,1], NA,                 p4_nc_rclp_c2[i,2], p4_nc_rclp_c2[i,3]),
      c(3, p4_nc_rclp_c3[i,1], p4_nc_rclp_c3[i,2], NA,                 p4_nc_rclp_c3[i,3]),
      c(4, p4_nc_rclp_c4[i,1], p4_nc_rclp_c4[i,2], p4_nc_rclp_c4[i,3], NA)
    )
  ) 
  colnames(p4_nc_rclp_tbl) = c("index_class","1","2","3","4")
  
  
  #C. Determine whether to reclassify each obs
  #cases
  for (j in 1:nrow(p4_c_dat)){
    #j<-1
    #trial (keep or reclassify)
    p4_c_index_class[j] = p4_c_stigma_orig[j]                          #get stigma class for index obs
    p4_c_index_ppv[j]   = p4_c_ppv[i, p4_c_index_class[j]]                #get ppv corresp. to class
    p4_c_keepclass[j] = rbinom(1, 1, p4_c_index_ppv[j])                #Bernoulli trial to keep or reclassify
    
    if (p4_c_keepclass[j] == 1){
      p4_c_stigma_new[j] <- p4_c_index_class[j]
    }
    
    if (p4_c_keepclass[j] == 0){
      p4_c_p_tbl <- as.data.frame(t(p4_c_rclp_tbl[p4_c_index_class[j], ])) %>% 
        tibble::rownames_to_column(var = "index") %>% 
        rename(p = 2) %>% 
        filter(index != "index_class" & !is.na(p)) %>% 
        arrange(p)
      
      p4_c_ran.val <-runif(1)
      
      p4_c_stigma_new[j] <- as.numeric(
        ifelse(p4_c_ran.val <= p4_c_p_tbl$p[1], p4_c_p_tbl$index[1],
               ifelse(p4_c_ran.val > p4_c_p_tbl$p[1] & 
                        p4_c_ran.val <= p4_c_p_tbl$p[2], p4_c_p_tbl$index[2], p4_c_p_tbl$index[3])))
    }
  }
  

  #non-cases
  for (j in 1:nrow(p4_nc_dat)){
    #trial (keep or reclassify)
    p4_nc_index_class[j] = p4_nc_stigma_orig[j]                          #get stigma class for index obs
    p4_nc_index_ppv[j]   = p4_nc_ppv[i, p4_nc_index_class[j]]                #get ppv corresp. to class
    p4_nc_keepclass[j] = rbinom(1, 1, p4_nc_index_ppv[j])                #Bernoulli trial to keep or reclassify
    
    if (p4_nc_keepclass[j] == 1){
      p4_nc_stigma_new[j] <- p4_nc_index_class[j]
    }
    
    if (p4_nc_keepclass[j] == 0){
      p4_nc_p_tbl <- as.data.frame(t(p4_nc_rclp_tbl[p4_nc_index_class[j], ])) %>% 
        tibble::rownames_to_column(var = "index") %>% 
        rename(p = 2) %>% 
        filter(index != "index_class" & !is.na(p)) %>% 
        arrange(p)
      
      p4_nc_ran.val <-runif(1)
      
      p4_nc_stigma_new[j] <- as.numeric(
        ifelse(p4_nc_ran.val <= p4_nc_p_tbl$p[1], p4_nc_p_tbl$index[1],
               ifelse(p4_nc_ran.val > p4_nc_p_tbl$p[1] & 
                        p4_nc_ran.val <= p4_nc_p_tbl$p[2], p4_nc_p_tbl$index[2], p4_nc_p_tbl$index[3])))
    }
  }
  
  p4_c_clchg_prp[i] <- 1 - mean(p4_c_keepclass)     #get prp of records reclassified
  p4_nc_clchg_prp[i] <- 1 - mean(p4_nc_keepclass)
  
  p4_c_dat_reclass <- cbind(p4_c_dat, p4_c_stigma_new)    #merge new class with dat
  p4_nc_dat_reclass <- cbind(p4_nc_dat, p4_nc_stigma_new)
  
  #create reconstructed data and create the stigpoor var with new stigma variable
  dat_reclass <- rbind(p4_c_dat_reclass %>% rename(stigma_new = p4_c_stigma_new), 
                       p4_nc_dat_reclass %>% rename(stigma_new = p4_nc_stigma_new)) %>% 
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
  p4_pper12adj[i,] <- ipwComref(dat_reclass2_ipw, "p_per12")$est_adj 
  
  
  #bias-adjusted estimates & 95% CI incorporating total error
    #get bootstrapped sample
    bootsample.ids <- sample(1:nrow(dat_reclass2), size=nrow(dat_reclass2), replace = T)
    bootsample <- dat_reclass2[bootsample.ids,]
    
    #add ipweights
    bootsample_ipw <- ipwdat(bootsample)
    
    #poisson gee with comref (total error)
    p4_pper12adj.boot[i,] <- ipwComref(bootsample_ipw, "p_per12")$est_adj 
    
}



#save products
#reg est
saveRDS(p4_pper12adj, paste0(save_dir, "/p4_pper12adj.rds"))
saveRDS(p4_pper12adj.boot, paste0(save_dir, "/p4_pper12adj.boot.rds"))

#proportion of observations reclassified per iteration
prp_obs_chg <- as.data.frame(cbind(p4_c_clchg_prp, p4_nc_clchg_prp)) 
saveRDS(prp_obs_chg, paste0(save_dir, "/p4_prp_obs_chg.rds"))


# p4_c_df<-as.data.frame(cbind(
#   p4_c_stigma_orig = p4_c_stigma_orig, 
#   p4_c_index_class = p4_c_index_class, 
#   p4_c_index_ppv = p4_c_index_ppv, 
#   p4_c_keepclass = p4_c_keepclass,
#   p4_c_stigma_new = p4_c_stigma_new))
# saveRDS(p4_c_df, paste0(save_dir, "/p4_c_df.rds"))
# 
# 
# p4_nc_df<-as.data.frame(cbind(
#   p4_nc_stigma_orig = p4_nc_stigma_orig, 
#   p4_nc_index_class = p4_nc_index_class, 
#   p4_nc_index_ppv = p4_nc_index_ppv, 
#   p4_nc_keepclass = p4_nc_keepclass,
#   p4_nc_stigma_new = p4_nc_stigma_new))
# saveRDS(p4_nc_df, paste0(save_dir, "/p4_nc_df.rds"))

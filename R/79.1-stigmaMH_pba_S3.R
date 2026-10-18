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



mplus_dir <- paste0("data/aim1/mplusdat_mh")
save_dir <- paste0("data/aim1/output")



#Get validation data 
#-----------------------------------------------------------------------------------------
s3_c_valdat <- readRDS(paste0(mplus_dir, "/s3_c_valdat.rds"))
s3_nc_valdat <- readRDS(paste0(mplus_dir, "/s3_nc_valdat.rds"))



#Run PBA
#-----------------------------------------------------------------------------------------
source("R/79.2-StigmaMHFunctions.R")

#shape stats  - beta dist and dirichlet dist
s3_c_alpha_c1 <- s3_c_valdat[1,1]; s3_c_tot_c1 <- s3_c_valdat[1,5]
s3_c_alpha_c2 <- s3_c_valdat[2,2]; s3_c_tot_c2 <- s3_c_valdat[2,5]
s3_c_alpha_c3 <- s3_c_valdat[3,3]; s3_c_tot_c3 <- s3_c_valdat[3,5]
s3_c_alpha_c4 <- s3_c_valdat[4,4]; s3_c_tot_c4 <- s3_c_valdat[4,5]
s3_c_rclv_c1 <- c(s3_c_valdat[1,2],s3_c_valdat[1,3], s3_c_valdat[1,4])
s3_c_rclv_c2 <- c(s3_c_valdat[2,1],s3_c_valdat[2,3], s3_c_valdat[2,4])
s3_c_rclv_c3 <- c(s3_c_valdat[3,1],s3_c_valdat[3,2], s3_c_valdat[3,4])
s3_c_rclv_c4 <- c(s3_c_valdat[4,1],s3_c_valdat[4,2], s3_c_valdat[4,3])

s3_nc_alpha_c1 <- s3_nc_valdat[1,1]; s3_nc_tot_c1 <- s3_nc_valdat[1,5]
s3_nc_alpha_c2 <- s3_nc_valdat[2,2]; s3_nc_tot_c2 <- s3_nc_valdat[2,5]
s3_nc_alpha_c3 <- s3_nc_valdat[3,3]; s3_nc_tot_c3 <- s3_nc_valdat[3,5]
s3_nc_alpha_c4 <- s3_nc_valdat[4,4]; s3_nc_tot_c4 <- s3_nc_valdat[4,5]
s3_nc_rclv_c1 <- c(s3_nc_valdat[1,2],s3_nc_valdat[1,3], s3_nc_valdat[1,4])
s3_nc_rclv_c2 <- c(s3_nc_valdat[2,1],s3_nc_valdat[2,3], s3_nc_valdat[2,4])
s3_nc_rclv_c3 <- c(s3_nc_valdat[3,1],s3_nc_valdat[3,2], s3_nc_valdat[3,4])
s3_nc_rclv_c4 <- c(s3_nc_valdat[4,1],s3_nc_valdat[4,2], s3_nc_valdat[4,3])


#iterations and empty vectors/dfs
M <- 10 * 1000

s3_c_ppv <- s3_nc_ppv <-as.data.frame(matrix(NA, M, 4))
s3_c_rclp_c1 <- s3_c_rclp_c2 <- s3_c_rclp_c3  <- s3_c_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
s3_nc_rclp_c1 <- s3_nc_rclp_c2 <- s3_nc_rclp_c3  <- s3_nc_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
s3_c_clchg_prp <- s3_nc_clchg_prp <- rep(NA, M)


latvardat <- readRDS(paste0(mplus_dir, "/newdata.rds")) %>% mutate(stigma=N) %>% 
  filter(!is.na(POOR))
names(latvardat) <- tolower(names(latvardat))

s3_c_dat <- latvardat %>% filter(sui_try == 1)
s3_nc_dat <- latvardat %>% filter(sui_try == 0 & !is.na(sui_thk))


s3_c_stigma_orig <- s3_c_dat$stigma
s3_c_index_ppv <- s3_c_index_class <- s3_c_keepclass <- rep(NA, nrow(s3_c_dat))
s3_c_stigma_new <- rep(NA, nrow(s3_c_dat))

s3_nc_stigma_orig <- s3_nc_dat$stigma
s3_nc_index_ppv <- s3_nc_index_class <- s3_nc_keepclass <- rep(NA, nrow(s3_nc_dat))
s3_nc_stigma_new <- rep(NA, nrow(s3_nc_dat))


s3_sui_tryadj <- as.data.frame(matrix(NA, M, 7))
s3_sui_tryadj.boot <- as.data.frame(matrix(NA, M, 7))

s3_emmtbl <- as.data.frame(matrix(NA, M, 9))
colnames(s3_emmtbl) <- c("multinter_14", "reri_14", "ap_14",
                         "multinter_24", "reri_24", "ap_24",
                         "multinter_34", "reri_34", "ap_34")


#pba
set.seed(123)

for (i in 1: M) {
  #i<-1
  #A. Generate ppvs 
  s3_c_ppv[i,] <- c(rbeta(1, s3_c_alpha_c1, s3_c_tot_c1 - s3_c_alpha_c1), 
                    rbeta(1, s3_c_alpha_c2, s3_c_tot_c2 - s3_c_alpha_c2), 
                    rbeta(1, s3_c_alpha_c3, s3_c_tot_c3 - s3_c_alpha_c3), 
                    rbeta(1, s3_c_alpha_c4, s3_c_tot_c4 - s3_c_alpha_c4))
  
  s3_nc_ppv[i,] <- c(rbeta(1, s3_nc_alpha_c1, s3_nc_tot_c1 - s3_nc_alpha_c1), 
                     rbeta(1, s3_nc_alpha_c2, s3_nc_tot_c2 - s3_nc_alpha_c2), 
                     rbeta(1, s3_nc_alpha_c3, s3_nc_tot_c3 - s3_nc_alpha_c3), 
                     rbeta(1, s3_nc_alpha_c4, s3_nc_tot_c4 - s3_nc_alpha_c4))
  
  
  #B. Generate reclassification probs
  s3_c_rclp_c1[i,] <- MCMCprecision::rdirichlet(1, s3_c_rclv_c1)
  s3_c_rclp_c2[i,] <- MCMCprecision::rdirichlet(1, s3_c_rclv_c2)
  s3_c_rclp_c3[i,] <- MCMCprecision::rdirichlet(1, s3_c_rclv_c3)
  s3_c_rclp_c4[i,] <- MCMCprecision::rdirichlet(1, s3_c_rclv_c4)
  s3_c_rclp_tbl    <- as.data.frame(
    rbind(
      c(1, NA,                s3_c_rclp_c1[i,1], s3_c_rclp_c1[i,2], s3_c_rclp_c1[i,3]),
      c(2, s3_c_rclp_c2[i,1], NA,                s3_c_rclp_c2[i,2], s3_c_rclp_c2[i,3]),
      c(3, s3_c_rclp_c3[i,1], s3_c_rclp_c3[i,2], NA,                s3_c_rclp_c3[i,3]),
      c(4, s3_c_rclp_c4[i,1], s3_c_rclp_c4[i,2], s3_c_rclp_c4[i,3], NA)
    )
  ) 
  colnames(s3_c_rclp_tbl) = c("index_class","1","2","3","4")
  
  
  s3_nc_rclp_c1[i,] <- MCMCprecision::rdirichlet(1, s3_nc_rclv_c1)
  s3_nc_rclp_c2[i,] <- MCMCprecision::rdirichlet(1, s3_nc_rclv_c2)
  s3_nc_rclp_c3[i,] <- MCMCprecision::rdirichlet(1, s3_nc_rclv_c3)
  s3_nc_rclp_c4[i,] <- MCMCprecision::rdirichlet(1, s3_nc_rclv_c4)
  s3_nc_rclp_tbl    <- as.data.frame(
    rbind(
      c(1, NA,                 s3_nc_rclp_c1[i,1], s3_nc_rclp_c1[i,2], s3_nc_rclp_c1[i,3]),
      c(2, s3_nc_rclp_c2[i,1], NA,                 s3_nc_rclp_c2[i,2], s3_nc_rclp_c2[i,3]),
      c(3, s3_nc_rclp_c3[i,1], s3_nc_rclp_c3[i,2], NA,                 s3_nc_rclp_c3[i,3]),
      c(4, s3_nc_rclp_c4[i,1], s3_nc_rclp_c4[i,2], s3_nc_rclp_c4[i,3], NA)
    )
  ) 
  colnames(s3_nc_rclp_tbl) = c("index_class","1","2","3","4")
  
  
  #C. Determine whether to reclassify each obs
  #cases
  for (j in 1:nrow(s3_c_dat)){
    #j<-1
    #trial (keep or reclassify)
    s3_c_index_class[j] = s3_c_stigma_orig[j]                          #get stigma class for index obs
    s3_c_index_ppv[j]   = s3_c_ppv[i, s3_c_index_class[j]]                #get ppv corresp. to class
    s3_c_keepclass[j] = rbinom(1, 1, s3_c_index_ppv[j])                #Bernoulli trial to keep or reclassify
    
    if (s3_c_keepclass[j] == 1){
      s3_c_stigma_new[j] <- s3_c_index_class[j]
    }
    
    if (s3_c_keepclass[j] == 0){
      s3_c_p_tbl <- as.data.frame(t(s3_c_rclp_tbl[s3_c_index_class[j], ])) %>% 
        tibble::rownames_to_column(var = "index") %>% 
        rename(p = 2) %>% 
        filter(index != "index_class" & !is.na(p)) %>% 
        arrange(p)
      
      s3_c_ran.val <-runif(1)
      
      s3_c_stigma_new[j] <- as.numeric(
        ifelse(s3_c_ran.val <= s3_c_p_tbl$p[1], s3_c_p_tbl$index[1],
               ifelse(s3_c_ran.val > s3_c_p_tbl$p[1] & 
                        s3_c_ran.val <= s3_c_p_tbl$p[2], s3_c_p_tbl$index[2], s3_c_p_tbl$index[3])))
    }
  }
  
  
  #non-cases
  for (j in 1:nrow(s3_nc_dat)){
    #trial (keep or reclassify)
    s3_nc_index_class[j] = s3_nc_stigma_orig[j]                          #get stigma class for index obs
    s3_nc_index_ppv[j]   = s3_nc_ppv[i, s3_nc_index_class[j]]                #get ppv corresp. to class
    s3_nc_keepclass[j] = rbinom(1, 1, s3_nc_index_ppv[j])                #Bernoulli trial to keep or reclassify
    
    if (s3_nc_keepclass[j] == 1){
      s3_nc_stigma_new[j] <- s3_nc_index_class[j]
    }
    
    if (s3_nc_keepclass[j] == 0){
      s3_nc_p_tbl <- as.data.frame(t(s3_nc_rclp_tbl[s3_nc_index_class[j], ])) %>% 
        tibble::rownames_to_column(var = "index") %>% 
        rename(p = 2) %>% 
        filter(index != "index_class" & !is.na(p)) %>% 
        arrange(p)
      
      s3_nc_ran.val <-runif(1)
      
      s3_nc_stigma_new[j] <- as.numeric(
        ifelse(s3_nc_ran.val <= s3_nc_p_tbl$p[1], s3_nc_p_tbl$index[1],
               ifelse(s3_nc_ran.val > s3_nc_p_tbl$p[1] & 
                        s3_nc_ran.val <= s3_nc_p_tbl$p[2], s3_nc_p_tbl$index[2], s3_nc_p_tbl$index[3])))
    }
  }
  
  s3_c_clchg_prp[i] <- 1 - mean(s3_c_keepclass)     #get prp of records reclassified
  s3_nc_clchg_prp[i] <- 1 - mean(s3_nc_keepclass)
  
  s3_c_dat_reclass <- cbind(s3_c_dat, s3_c_stigma_new)    #merge new class with dat
  s3_nc_dat_reclass <- cbind(s3_nc_dat, s3_nc_stigma_new)
  
  #create reconstructed data and create the stigpoor var with new stigma variable
  dat_reclass <- rbind(s3_c_dat_reclass %>% rename(stigma_new = s3_c_stigma_new), 
                       s3_nc_dat_reclass %>% rename(stigma_new = s3_nc_stigma_new)) %>% 
    rename(stigma_orig = stigma)
  names(dat_reclass) <- tolower(names(dat_reclass))
  
  exp_grps <- tidyr::crossing(stigma = dat_reclass$stigma_new, poor = dat_reclass$poor) %>% 
    arrange(desc(poor), stigma) %>% 
    mutate(stigpoor = as.character(row_number())) %>% 
    mutate(stigpoor.cat = paste0("stigpoor", stigpoor, sep = ""))
  
  dat_reclass2 <- left_join(dat_reclass, exp_grps, by = c("stigma_new" = "stigma","poor")) %>% 
    rename(stigma = stigma_new) %>% 
    fastDummies::dummy_cols(., select_columns = "stigma") %>% 
    fastDummies::dummy_cols(., select_columns = "stigpoor") %>% 
    rename(stigpoor11 = stigpoor_1,
           stigpoor21 = stigpoor_2,
           stigpoor31 = stigpoor_3,
           stigpoor41 = stigpoor_4,
           stigpoor10 = stigpoor_5,
           stigpoor20 = stigpoor_6,
           stigpoor30 = stigpoor_7,
           stigpoor40 = stigpoor_8) 
  
  #outcome regression with reconstructed data 
  #(for bias-adjusted estimates & 95% SI incorporating error from bias params only)
  s3_sui_tryadj[i,] <- getComRefPRs(dat_reclass2, "sui_try")$est.adj 
  
  #bias-adjusted estimates & 95% CI incorporating total error
  #get bootstrapped sample
  bootsample.ids <- sample(1:nrow(dat_reclass2), size=nrow(dat_reclass2), replace = T)
  bootsample <- dat_reclass2[bootsample.ids,]
  
  s3_sui_tryadj.boot[i,] <- getComRefPRs(bootsample, "sui_try")$est.adj 
  
  #get emm metrics
  emmtbl0 <- data.frame(getRERIs(bootsample, "sui_try")$reri_tbl) %>%
    select(stigpoor, multinter, reri, ap)
  s3_emmtbl[i,] <- c(emmtbl0[1,2:4], emmtbl0[2,2:4], emmtbl0[3,2:4])
  
}



#save products
#reg est
saveRDS(s3_sui_tryadj, paste0(save_dir, "/s3_sui_tryadj.rds"))
saveRDS(s3_sui_tryadj.boot, paste0(save_dir, "/s3_sui_tryadj.boot.rds"))
saveRDS(s3_emmtbl, paste0(save_dir, "/s3_emmtbl.rds"))

#proportion of observations reclassified per iteration
prp_obs_chg <- as.data.frame(cbind(s3_c_clchg_prp, s3_nc_clchg_prp)) 
saveRDS(prp_obs_chg, paste0(save_dir, "/s3_prp_obs_chg.rds"))


s3_c_df<-as.data.frame(cbind(
  s3_c_stigma_orig = s3_c_stigma_orig, 
  s3_c_index_class = s3_c_index_class, 
  s3_c_index_ppv = s3_c_index_ppv, 
  s3_c_keepclass = s3_c_keepclass,
  s3_c_stigma_new = s3_c_stigma_new))
saveRDS(s3_c_df, paste0(save_dir, "/s3_c_df.rds"))

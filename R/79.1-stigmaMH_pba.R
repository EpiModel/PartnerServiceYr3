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



mplus_dir <- paste0("data/aim1/mplusdat")
save_dir <- paste0("data/aim1/output")



#Get validation data 
#-----------------------------------------------------------------------------------------
c_valdat <- readRDS(paste0(mplus_dir, "/c_valdat.rds"))
nc_valdat <- readRDS(paste0(mplus_dir, "/nc_valdat.rds"))



#Run PBA
#-----------------------------------------------------------------------------------------
source("R/79.2-StigmaMHFunctions.R")

#shape stats  - beta dist and dirichlet dist
c_alpha_c1 <- c_valdat[1,1]; c_tot_c1 <- c_valdat[1,5]
c_alpha_c2 <- c_valdat[2,2]; c_tot_c2 <- c_valdat[2,5]
c_alpha_c3 <- c_valdat[3,3]; c_tot_c3 <- c_valdat[3,5]
c_alpha_c4 <- c_valdat[4,4]; c_tot_c4 <- c_valdat[4,5]
c_rclv_c1 <- c(c_valdat[1,2],c_valdat[1,3], c_valdat[1,4])
c_rclv_c2 <- c(c_valdat[2,1],c_valdat[2,3], c_valdat[2,4])
c_rclv_c3 <- c(c_valdat[3,1],c_valdat[3,2], c_valdat[3,4])
c_rclv_c4 <- c(c_valdat[4,1],c_valdat[4,2], c_valdat[4,3])

nc_alpha_c1 <- nc_valdat[1,1]; nc_tot_c1 <- nc_valdat[1,5]
nc_alpha_c2 <- nc_valdat[2,2]; nc_tot_c2 <- nc_valdat[2,5]
nc_alpha_c3 <- nc_valdat[3,3]; nc_tot_c3 <- nc_valdat[3,5]
nc_alpha_c4 <- nc_valdat[4,4]; nc_tot_c4 <- nc_valdat[4,5]
nc_rclv_c1 <- c(nc_valdat[1,2],nc_valdat[1,3], nc_valdat[1,4])
nc_rclv_c2 <- c(nc_valdat[2,1],nc_valdat[2,3], nc_valdat[2,4])
nc_rclv_c3 <- c(nc_valdat[3,1],nc_valdat[3,2], nc_valdat[3,4])
nc_rclv_c4 <- c(nc_valdat[4,1],nc_valdat[4,2], nc_valdat[4,3])


#iterations and empty vectors/dfs
M <- 100 # * 1000

c_ppv <- nc_ppv <-as.data.frame(matrix(NA, M, 4))
c_rclp_c1 <- c_rclp_c2 <- c_rclp_c3  <- c_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
nc_rclp_c1 <- nc_rclp_c2 <- nc_rclp_c3  <- nc_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
c_clchg_prp <- nc_clchg_prp <- rep(NA, M)


latvardat <- readRDS(paste0(mplus_dir, "/newdata.rds")) %>% mutate(stigma=N) %>% 
  filter(!is.na(POOR))
names(latvardat) <- tolower(names(latvardat))

c_dat <- latvardat %>% filter(smi == 1)
nc_dat <- latvardat %>% filter(smi == 0)


c_stigma_orig <- c_dat$stigma
c_index_ppv <- c_index_class <- c_keepclass <- rep(NA, nrow(c_dat))
c_stigma_new <- rep(NA, nrow(c_dat))

nc_stigma_orig <- nc_dat$stigma
nc_index_ppv <- nc_index_class <- nc_keepclass <- rep(NA, nrow(nc_dat))
nc_stigma_new <- rep(NA, nrow(nc_dat))


smiadj <- as.data.frame(matrix(NA, M, 7))
smiadj.boot <- as.data.frame(matrix(NA, M, 7))

emmtbl <- as.data.frame(matrix(NA, M, 9))
colnames(emmtbl) <- c("multinter_14", "reri_14", "ap_14",
                      "multinter_24", "reri_24", "ap_24",
                      "multinter_34", "reri_34", "ap_34")


#pba
set.seed(123)

for (i in 1: M) {
  #i<-1
  #A. Generate ppvs 
  c_ppv[i,] <- c(rbeta(1, c_alpha_c1, c_tot_c1 - c_alpha_c1), 
                 rbeta(1, c_alpha_c2, c_tot_c2 - c_alpha_c2), 
                 rbeta(1, c_alpha_c3, c_tot_c3 - c_alpha_c3), 
                 rbeta(1, c_alpha_c4, c_tot_c4 - c_alpha_c4))
  
  nc_ppv[i,] <- c(rbeta(1, nc_alpha_c1, nc_tot_c1 - nc_alpha_c1), 
                  rbeta(1, nc_alpha_c2, nc_tot_c2 - nc_alpha_c2), 
                  rbeta(1, nc_alpha_c3, nc_tot_c3 - nc_alpha_c3), 
                  rbeta(1, nc_alpha_c4, nc_tot_c4 - nc_alpha_c4))
  
  
  #B. Generate reclassification probs
  c_rclp_c1[i,] <- MCMCprecision::rdirichlet(1, c_rclv_c1)
  c_rclp_c2[i,] <- MCMCprecision::rdirichlet(1, c_rclv_c2)
  c_rclp_c3[i,] <- MCMCprecision::rdirichlet(1, c_rclv_c3)
  c_rclp_c4[i,] <- MCMCprecision::rdirichlet(1, c_rclv_c4)
  c_rclp_tbl    <- as.data.frame(
    rbind(
      c(1, NA,             c_rclp_c1[i,1], c_rclp_c1[i,2], c_rclp_c1[i,3]),
      c(2, c_rclp_c2[i,1], NA,             c_rclp_c2[i,2], c_rclp_c2[i,3]),
      c(3, c_rclp_c3[i,1], c_rclp_c3[i,2], NA,             c_rclp_c3[i,3]),
      c(4, c_rclp_c4[i,1], c_rclp_c4[i,2], c_rclp_c4[i,3], NA)
    )
  ) 
  colnames(c_rclp_tbl) = c("index_class","1","2","3","4")
  
  
  nc_rclp_c1[i,] <- MCMCprecision::rdirichlet(1, nc_rclv_c1)
  nc_rclp_c2[i,] <- MCMCprecision::rdirichlet(1, nc_rclv_c2)
  nc_rclp_c3[i,] <- MCMCprecision::rdirichlet(1, nc_rclv_c3)
  nc_rclp_c4[i,] <- MCMCprecision::rdirichlet(1, nc_rclv_c4)
  nc_rclp_tbl    <- as.data.frame(
    rbind(
      c(1, NA,              nc_rclp_c1[i,1], nc_rclp_c1[i,2], nc_rclp_c1[i,3]),
      c(2, nc_rclp_c2[i,1], NA,              nc_rclp_c2[i,2], nc_rclp_c2[i,3]),
      c(3, nc_rclp_c3[i,1], nc_rclp_c3[i,2], NA,              nc_rclp_c3[i,3]),
      c(4, nc_rclp_c4[i,1], nc_rclp_c4[i,2], nc_rclp_c4[i,3], NA)
    )
  ) 
  colnames(nc_rclp_tbl) = c("index_class","1","2","3","4")
  
  
  #C. Determine whether to reclassify each obs
  #cases
  for (j in 1:nrow(c_dat)){
    #j<-1
    #trial (keep or reclassify)
    c_index_class[j] = c_stigma_orig[j]                          #get stigma class for index obs
    c_index_ppv[j]   = c_ppv[i, c_index_class[j]]                #get ppv corresp. to class
    c_keepclass[j] = rbinom(1, 1, c_index_ppv[j])                #Bernoulli trial to keep or reclassify
    
    if (c_keepclass[j] == 1){
      c_stigma_new[j] <- c_index_class[j]
    }
    
    if (c_keepclass[j] == 0){
      c_p_tbl <- as.data.frame(t(c_rclp_tbl[c_index_class[j], ])) %>% 
        tibble::rownames_to_column(var = "index") %>% 
        rename(p = 2) %>% 
        filter(index != "index_class" & !is.na(p)) %>% 
        arrange(p)
      
      c_ran.val <-runif(1)
      
      c_stigma_new[j] <- as.numeric(
        ifelse(c_ran.val <= c_p_tbl$p[1], c_p_tbl$index[1],
               ifelse(c_ran.val > c_p_tbl$p[1] & 
                        c_ran.val <= c_p_tbl$p[2], c_p_tbl$index[2], c_p_tbl$index[3])))
    }
  }
  

  #non-cases
  for (j in 1:nrow(nc_dat)){
    #trial (keep or reclassify)
    nc_index_class[j] = nc_stigma_orig[j]                          #get stigma class for index obs
    nc_index_ppv[j]   = nc_ppv[i, nc_index_class[j]]                #get ppv corresp. to class
    nc_keepclass[j] = rbinom(1, 1, nc_index_ppv[j])                #Bernoulli trial to keep or reclassify
    
    if (nc_keepclass[j] == 1){
      nc_stigma_new[j] <- nc_index_class[j]
    }
    
    if (nc_keepclass[j] == 0){
      nc_p_tbl <- as.data.frame(t(nc_rclp_tbl[nc_index_class[j], ])) %>% 
        tibble::rownames_to_column(var = "index") %>% 
        rename(p = 2) %>% 
        filter(index != "index_class" & !is.na(p)) %>% 
        arrange(p)
      
      nc_ran.val <-runif(1)
      
      nc_stigma_new[j] <- as.numeric(
        ifelse(nc_ran.val <= nc_p_tbl$p[1], nc_p_tbl$index[1],
               ifelse(nc_ran.val > nc_p_tbl$p[1] & 
                        nc_ran.val <= nc_p_tbl$p[2], nc_p_tbl$index[2], nc_p_tbl$index[3])))
    }
  }
  
  c_clchg_prp[i] <- 1 - mean(c_keepclass)     #get prp of records reclassified
  nc_clchg_prp[i] <- 1 - mean(nc_keepclass)
  
  c_dat_reclass <- cbind(c_dat, c_stigma_new)    #merge new class with dat
  nc_dat_reclass <- cbind(nc_dat, nc_stigma_new)
  
  #create reconstructed data and create the stigpoor var with new stigma variable
  dat_reclass <- rbind(c_dat_reclass %>% rename(stigma_new = c_stigma_new), 
                       nc_dat_reclass %>% rename(stigma_new = nc_stigma_new)) %>% 
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
  smiadj[i,] <- getComRefPRs(dat_reclass2, "smi")$est.adj 
  
  #bias-adjusted estimates & 95% CI incorporating total error
    #get bootstrapped sample
    bootsample.ids <- sample(1:nrow(dat_reclass2), size=nrow(dat_reclass2), replace = T)
    bootsample <- dat_reclass2[bootsample.ids,]
    
    smiadj.boot[i,] <- getComRefPRs(bootsample, "smi")$est.adj 
    
    #get emm metrics 
    emmtbl0 <- getRERIs(bootsample, "smi", 1)$reri_tbl %>% 
      select(stigpoor, multinter, reri, ap) 
    emmtbl[i,] <- c(emmtbl0[1,2:4], emmtbl0[2,2:4], emmtbl0[3,2:4])
   
}



#save products
#reg est
saveRDS(smiadj, paste0(save_dir, "/smiadj.rds"))
saveRDS(smiadj.boot, paste0(save_dir, "/smiadj.boot.rds"))
saveRDS(emmtbl, paste0(save_dir, "/emmtbl.rds"))

#proportion of observations reclassified per iteration
prp_obs_chg <- as.data.frame(cbind(c_clchg_prp, nc_clchg_prp)) 
saveRDS(prp_obs_chg, paste0(save_dir, "/prp_obs_chg.rds"))


c_df<-as.data.frame(cbind(
  c_stigma_orig = c_stigma_orig, 
  c_index_class = c_index_class, 
  c_index_ppv = c_index_ppv, 
  c_keepclass = c_keepclass,
  c_stigma_new = c_stigma_new))
saveRDS(c_df, paste0(save_dir, "/c_df.rds"))


nc_df<-as.data.frame(cbind(
  nc_stigma_orig = nc_stigma_orig, 
  nc_index_class = nc_index_class, 
  nc_index_ppv = nc_index_ppv, 
  nc_keepclass = nc_keepclass,
  nc_stigma_new = nc_stigma_new))
saveRDS(nc_df, paste0(save_dir, "/nc_df.rds"))

#
# Association between sexual stigma and mental health distress
# step 4: Misclassification bias adjustment of risk model 


rm(list = ls())


# libraries
install.packages("MCMCprecision")
install.packages("fastDummies")
library("dplyr")
library("tidyr")



mplus_dir <- paste0("data/aim1/mplusdat")
save_dir <- paste0("data/aim1/output")


#Validation data 
#-----------------------------------------------------------------------------------------
#cases
valtbl_c <- cbind(
  c(0.897, 0,     0.045, 0.021),
  c(0,     0.882, 0.028, 0.062),
  c(0.081, 0.024, 0.896, 0.029),
  c(0.021, 0.094, 0.031, 0.888),
  c(190,   453,   261,   475  )
)
colnames(valtbl_c) <- c("1","4","2","3","tot")
valdat_c <- tibble::rownames_to_column(as.data.frame(valtbl_c),var = "likely") %>% 
  mutate(s_class = ifelse(likely == 1, 1, 
                          ifelse(likely==2, 4, 
                                 ifelse(likely==3,2, 3)))) %>% 
  arrange(s_class) %>% 
  select(s_class,"1","2","3","4","tot") %>% 
  mutate(across(c(2:5), ~ round(.x * tot, 0)))


#non-cases
valtbl_nc <- cbind(
  c(0.877, 0.060, 0.015, 0.013),
  c(0.006, 0.877, 0.034, 0    ),
  c(0.029, 0.063, 0.847, 0.060),
  c(0.035, 0,     0.104, 0.926),
  c(551,   512,   1922,  2355 )
)
colnames(valtbl_nc) <- c("2","1","3","4","tot")
valdat_nc <- tibble::rownames_to_column(as.data.frame(valtbl_nc),var = "likely") %>% 
  mutate(s_class = ifelse(likely == 1, 2, 
                          ifelse(likely==2, 1, 
                                 ifelse(likely==3,3, 4)))) %>% 
  arrange(s_class) %>% 
  select(s_class,"1","2","3","4","tot") %>% 
  mutate(across(c(2:5), ~ round(.x * tot, 0)))




# PBA
#-----------------------------------------------------------------------------------------
source("R/79.2-DistalMHFunctions.R")

#shape stats
c_alpha_c1 <- valdat_c[1,2]; c_tot_c1 <- valdat_c[1,6]
c_alpha_c2 <- valdat_c[2,3]; c_tot_c2 <- valdat_c[2,6]
c_alpha_c3 <- valdat_c[3,4]; c_tot_c3 <- valdat_c[3,6]
c_alpha_c4 <- valdat_c[4,5]; c_tot_c4 <- valdat_c[4,6]
c_rclv_c1 <- c(valdat_c[1,3],valdat_c[1,4], valdat_c[1,5])
c_rclv_c2 <- c(valdat_c[2,2],valdat_c[2,4], valdat_c[2,5])
c_rclv_c3 <- c(valdat_c[3,2],valdat_c[3,3], valdat_c[3,5])
c_rclv_c4 <- c(valdat_c[4,2],valdat_c[4,3], valdat_c[4,4])

nc_alpha_c1 <- valdat_c[1,2]; nc_tot_c1 <- valdat_c[1,6]
nc_alpha_c2 <- valdat_c[2,3]; nc_tot_c2 <- valdat_c[2,6]
nc_alpha_c3 <- valdat_c[3,4]; nc_tot_c3 <- valdat_c[3,6]
nc_alpha_c4 <- valdat_c[4,5]; nc_tot_c4 <- valdat_c[4,6]
nc_rclv_c1 <- c(valdat_c[1,3],valdat_c[1,4], valdat_c[1,5])
nc_rclv_c2 <- c(valdat_c[2,2],valdat_c[2,4], valdat_c[2,5])
nc_rclv_c3 <- c(valdat_c[3,2],valdat_c[3,3], valdat_c[3,5])
nc_rclv_c4 <- c(valdat_c[4,2],valdat_c[4,3], valdat_c[4,4])


#iterations and empty vectors/dfs
M <- 100

c_ppv <- nc_ppv <-as.data.frame(matrix(NA, M, 4))
c_rclp_c1 <- c_rclp_c2 <- c_rclp_c3  <- c_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
nc_rclp_c1 <- nc_rclp_c2 <- nc_rclp_c3  <- nc_rclp_c4 <- as.data.frame(matrix(NA, M, 3))
c_clchg_prp <- nc_clchg_prp <- rep(NA, M)


latvardat <- readRDS(paste0(mplus_dir, "/newdata.rds")) %>%
  mutate(stigma = ifelse(N==1,2, ifelse(N==2,3, ifelse(N==3,1, N))))
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
  
  #outcome regression with reclassified df
  smiadj[i,] <- getComRefPRs(dat_reclass2, "smi")$est.adj 
}


#save products
#reg output
saveRDS(smiadj, paste0(save_dir, "/smiadj.rds"))

#proportion of observations reclassified per iteration
prp_obs_chg <- as.data.frame(cbind(c_clchgprp, nc_clchg_prp)) 
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

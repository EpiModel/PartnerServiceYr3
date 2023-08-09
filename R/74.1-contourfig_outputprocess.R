##
## 42. Processing counterfactual scenarios
##


# Setup ----------------------------------------------------------------------------------
context<-"hpc"

sims_dir <- paste0("data/intermediate/",context,"/figdata")
save_dir <- paste0("data/intermediate/",context,"/processed")


#get batch info
batches_infos <- EpiModelHPC::get_scenarios_batches_infos(
  paste0("data/intermediate/",context,"/figdata")
)




#A. Process intervdata -------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyr")
  library("dplyr")
  library("ggplot2")
})


#get sim files
sim_files <- list.files(
  sims_dir,
  pattern = "^sim__.*rds$",
  full.names = TRUE
)


#Process each batch in parallel 
process_sim <- function(file_name, ts) {
  
  # file_name <-sim_files[1]
  # ts <- 3901 - (5 * 52) + 1
  
  # keep only the file name without extension and split around `__`
  name_elts <- fs::path_file(file_name) %>%
    fs::path_ext_remove() %>%
    strsplit(split = "__")
  
  scenario_name <- name_elts[[1]][2]
  batch_number <- as.numeric(name_elts[[1]][3])
  
  d <- as_tibble(readRDS(file_name))
  
  d <- d %>%
    mutate(scenario_name = scenario_name, batch_number = batch_number) %>% 
    group_by(scenario_name, batch_number, sim) %>% 
    filter(time >= ts) %>% 
    mutate(time=row_number()) %>% 
    ungroup() %>% 
    mutate(sim2=ifelse(batch_number > 1,
                       sim + ((batch_number-1) * 32),
                       sim)) %>% 
    select(-sim) %>%
    rename(sim=sim2) %>%
    mutate(
      tbl                     = toupper(substr(scenario_name, 1, 1)),
      scenario.num            = as.numeric(substr(scenario_name, 2, 4)),
      scenario.new            = substr(scenario_name, 5, nchar(scenario_name))
    ) %>% 
    ungroup() %>% 
    mutate(found.indexes.all  = found.indexes.nd + found.indexes.pp) %>% 
    select(tbl, scenario.num, scenario.new, scenario_name, batch_number, sim, time, 
           incid, num, ir100, found.indexes.all) %>%
    arrange(tbl, scenario.num, scenario.new, scenario_name, batch_number, sim) %>%
    
    return(d)
}

intervds <- future.apply::future_lapply(
  sim_files,
  process_sim,
  ts = 3901 - (5 * 52) + 1   #gets data from 5 years prior to intervention start
)

#Merge batches  
fulldata <- bind_rows(intervds) %>% 
  select(tbl, scenario.num, scenario_name, sim, time, incid, found.indexes.all) %>% 
  mutate(scenario.new = stringr::str_split_i(scenario_name, "_", 1),
         psval = stringr::str_split_i(scenario_name, "_", 4))



if(fulldata$scenario_name [1] == "a001base") {
  saveRDS(fulldata, paste0(save_dir, "/fulldata_basemodel.rds"))
}



if(fulldata$scenario_name [1] != "a001base"){
  
  tblnam <- fulldata$tbl[2]
  psval <- fulldata$psval[2]
  scetop <- fulldata$scenario.new[2]

  saveRDS(fulldata, paste0(save_dir, "/fulldata_",scetop,"_",psval,".rds"))
  
  
  #B. Get pia
  #-----------------------------------------------------------------------------------------
  base_df <- readRDS(paste0(save_dir, "/fulldata_basemodel.rds", sep="")) %>% 
    filter(time > 5 * 52) %>% 
    group_by(tbl, scenario.num, scenario_name, sim) %>%
    summarise(across(c(incid, found.indexes.all), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup() %>% 
    summarise(base_incid = median(incid),
              base_foundindexes = median(found.indexes.all))
  
  base_incid <- base_df$base_incid
  
  piatbl <- fulldata %>% 
    filter(time > 5 * 52) %>% 
    group_by(tbl, scenario.num, scenario_name, sim) %>%
    summarise(across(c(incid, found.indexes.all), ~ sum(.x, na.rm = TRUE))) %>% 
    mutate(pia = (base_incid - incid) / base_incid) %>% 
    ungroup() %>% 
    mutate(across(where(is.numeric), ~round (., 6))) %>% ungroup()%>% 
    group_by(tbl, scenario.num, scenario_name) %>% 
    summarise(pia = median(pia)) %>% 
    mutate(scenario.new = stringr::str_split_i(scenario_name, "_", 1),
           x = as.numeric(stringr::str_split_i(scenario_name, "_", 2)),
           y = as.numeric(stringr::str_split_i(scenario_name, "_", 3)),
           psval = stringr::str_split_i(scenario_name, "_", 4)) %>% 
    arrange(tbl, scenario.num)
    
  saveRDS(piatbl, paste0(save_dir, "/piatbl_",scetop,"_",psval,".rds"))

   
  if(grepl(scetop,"001") == F) {
    
    #get split piatbl files
    piatbl_1 <- readRDS(paste(save_dir,"/piatbl_", tblnam, "001_", psval, ".rds", sep=""))
    piatbl_2 <- readRDS(paste(save_dir,"/piatbl_", tblnam, "114_", psval, ".rds", sep=""))
    
    piatbl.both <- rbind(piatbl_1, piatbl_2)
    
    #new data, loess model, predict
    griddat <- expand.grid(list(
      x = seq(min(piatbl$x), max(piatbl$x), length.out = 100),
      y = seq(min(piatbl$y), max(piatbl$y), length.out = 100)
    ))
    
    fit <- loess(pia ~ x * y, piatbl)
    
    griddat$pia <- as.numeric(predict(fit, newdata = griddat))
    griddat$tbl <- tbl
    griddat$psval <- psval
    
    saveRDS(griddat, paste0(save_dir, "/griddat_", tblnam,"_",psval,".rds"))
    
  }
  # #C. Get contour plot data  
  # #---------------------------------------------------------------------------------------
  # griddat <- expand.grid(list(
  #   x = seq(min(piatbl$x), max(piatbl$x), length.out = 100),
  #   y = seq(min(piatbl$y), max(piatbl$y), length.out = 100)
  #   ))
  # 
  # fit <- loess(pia ~ x * y, piatbl)
  # 
  # griddat$pia <- as.numeric(predict(fit, newdata = griddat))
  # griddat$tbl <- tblnam
  # griddat$psval <- psval
  # 
  # saveRDS(griddat, paste0(save_dir, "/griddat_", tblnam,"_",psval,".rds"))

}















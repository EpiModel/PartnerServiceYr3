##
## 100-Get intervention data for local analysis

#context<-"local"

# Setup ------------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyverse")
})
source("R/utils-scenarios_outcomes.R")
source("R/utils-netsim_inputs.R")
source("R/utils-netsize.R")


modtst_dir <- "data/intermediate/scenarios"


#get sim batches
modtst_files <- list.files(
  modtst_dir,
  pattern = "^sim__.*rds$",
  full.names = TRUE
)



#Process each batch in parallel 
process_sim <- function(file_name, ts) {
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
    ungroup()
  
  return(d)
}

intervds <- future.apply::future_lapply(
  modtst_files,
  process_sim,
  ts = interv_start - (5*52)+1   #gets data from 5 years prior to intervention start
)



# Merge all batches  
intervdata <- bind_rows(intervds)
saveRDS(intervdata, "data/intermediate/processed/allscenarios.rds")


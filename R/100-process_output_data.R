##
## 100-Get intervention data for local analysis

#ncores<-5
 
# Setup ------------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyverse")
})

#if (fs::dir_exists(cp_dir)) fs::dir_delete(cp_dir)
#future::plan(future::multicore, workers = ncores)

modtst_dir <- "data/output/modeltest"

# Process each file in parallel ------------------------------------------------
modtst_files <- list.files(
  modtst_dir,
  pattern = "^sim__.*rds$",
  full.names = TRUE
)

process_sim <- function(file_name, ts) {
  # keep only the file name without extension and split around `__`
  name_elts <- fs::path_file(file_name) %>%
    fs::path_ext_remove() %>%
    strsplit(split = "__")
  
  scenario_name <- name_elts[[1]][2]
  batch_num <- as.numeric(name_elts[[1]][3])
  
  d <- as_tibble(readRDS(file_name))
  d <- d %>%
    filter(time >= max(d$time) - ts) %>% 
    mutate(time=row_number()) %>% 
    mutate(scenario = scenario_name, 
           batch = batch_num)
 
  return(d)
}

intervds <- future.apply::future_lapply(
  modtst_files,
  process_sim,
  ts = 15*52+1   #gets the time from 5 years prior to intervention start
)

# Merge all and combine --------------------------------------------------------
intervdata <- bind_rows(intervds)
saveRDS(intervdata, paste0(modtst_dir, "/allscenarios.rds"))


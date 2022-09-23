##
## 100-Processing simulations
##

#ncores<-5
 
# Setup ------------------------------------------------------------------------
install.packages("future.apply")
suppressMessages({
  library("EpiModelHIV")
  library("future.apply")
  library("tidyverse")
})

if (fs::dir_exists(cp_dir)) fs::dir_delete(cp_dir)

future::plan(future::multicore, workers = ncores)
calib_dir <- "data/output/calib"

# Process each file in parallel ------------------------------------------------
calib_files <- list.files(
  calib_dir,
  pattern = "^sim__.*rds$",
  full.names = TRUE
)

#source("R/utils-netsize.R")
#source("R/utils-netsim_inputs.R")

process_sim <- function(file_name, ts) {
  # keep only the file name without extension and split around `__`
  name_elts <- fs::path_file(file_name) %>%
    fs::path_ext_remove() %>%
    strsplit(split = "__")
  
  scenario_name <- name_elts[[1]][2]
  batch_num <- as.numeric(name_elts[[1]][3])
  
  d0 <- as_tibble(readRDS(file_name))
  d <- d0 %>%
    filter(time >= max(d$time) - ts) %>% 
    mutate(time=row_number()) %>% 
    mutate(scenario_name = scenario_name, 
           batch = batch_num)
 
  return(d)
}

intervds <- future.apply::future_lapply(
  calib_files,
  process_sim,
  ts = 10*52
)

# Merge all and combine --------------------------------------------------------
intervdata <- bind_rows(intervds)
saveRDS(intervdata, paste0(calib_dir, "/intervdata_allsce.rds"))

# assessments <- assessments %>%
#   select(- c(sim, batch)) %>%
#   group_by(scenario_name) %>%
#   summarise(across(
#     everything(),
#     list(
#       q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
#       q2 = ~ quantile(.x, 0.50, na.rm = TRUE),
#       q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
#     ),
#     .names = "{.col}__{.fn}"
#   ))
  # this last bloc calculate the q1, median and q3 for all of the variables

# Save the result --------------------------------------------------------------
# small rds to be downloaded and assessed locally
# saveRDS(assessments, paste0(calib_dir, "/assessments.rds"))

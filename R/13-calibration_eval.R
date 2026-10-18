##
## 13. Epidemic Model Parameter Calibration, Local evaluation
##
#

# Setup ------------------------------------------------------------------------
suppressMessages({
  library(EpiModel)
  library(dplyr)
  library(tidyr)
})

d <- readRDS("data/output/calib/assessments.rds")

glimpse(d)

d %>%
  pivot_longer(-scenario_name) %>%
  separate(name, into = c('name', 'quant'), sep = "__") %>%
  filter(quant == "q2") %>%
  pivot_wider(names_from = scenario_name, values_from = value)

d %>%
  filter(scenario_name == "1") %>%
  pivot_longer(-scenario_name) %>%
  separate(name, into = c('name', 'quant'), sep = "__") %>%
  pivot_wider(names_from = quant, values_from = value)


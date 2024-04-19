#### Preamble ####
# Purpose: Cleans the raw agriculture data
# Author: Cheng Yang
# Date: 28 March 2024
# Contact: yvonneyang.cheng@mail.utoronto.ca
# License: MIT

#### WORKPLACE SETUP ####
library(dplyr)
library(tidyverse)
library(arrow)
library(janitor)
library(readxl)
library(rstanarm)
library(broom.mixed)
library(modelsummary)
library(ggplot2)
library(tidybayes)


dataq <- read_parquet(here::here("data/analysis_data/agriculture.parquet"))


model <- stan_glm(
         climate_impacts_per_decade_percent ~ local_delta_t + annual_precipitation_change_each_study_mm + co2_ppm, 
         data = dataq, 
         family = gaussian,
         prior = normal(location = 0, scale = 3, autoscale = TRUE),
         
         prior_intercept = normal(location = 0, scale = 3, autoscale = TRUE),
         seed = 853)

saveRDS(
  model,
  file = "models/gaussian.rds"
)

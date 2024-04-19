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


#### CLEAN DATA ####
agriculture <- read_excel("data/raw_data/Projected_impacts_datasheet_11.24.2021.xlsx")

agriculture_clean <- agriculture|>
  filter(Region == "Asia", Crop == "Rice", `Publication year` %in% c(2011, 2012, 2013,2014,2015,2016,2017,2018,2019, 2020)) |>
  select(Crop,
         Country,
         Region,
         `Current Average Temperature (dC)_area_weighted`,
         `Current Annual Precipitation (mm) _area_weighted`,
         `Local delta T`,
         `Annual Precipitation change each study  (mm)` ,
         `Projected yield (t/ha)`,
         `Climate impacts per decade (%)`,
         `CO2 ppm`,
         `Publication year`
         ) |>
  clean_names()|>
  na.omit()

agriculture_clean

write_parquet(agriculture_clean,
              "data/analysis_data/agriculture.parquet")

agriculture_raw <- read_parquet(here::here("data/analysis_data/agriculture.parquet"))

names(agriculture_raw)


testres <- agriculture_raw

countries <- unique(agriculture_raw$country)

testres <- testres|>
  group_by(country) |>
  mutate(
    "climate_impact_mean" = mean(climate_impacts_per_decade_percent),
    "climate_impact_median" = median(climate_impacts_per_decade_percent),
    "climate_impact_sd" = sd(climate_impacts_per_decade_percent)
  ) |>
  ungroup()
  
production <- testres |>
  select(country, climate_impact_mean, climate_impact_median, climate_impact_sd) |>
  unique()

write_parquet(production,
              "data/analysis_data/production.parquet")

  
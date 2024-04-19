#### Preamble ####
# Purpose: Simulates data
# Author: Cheng Yang
# Date: 28 March 2024
# Contact: yvonneyang.cheng@mail.utoronto.ca
# License: MIT

#### Workspace setup ####

library(tidyverse)
library(dplyr)

#### Simulate ####

# In this simulation, we are generating synthetic agricultural data for random Asian countries.
# The data includes temperature change, precipitation change, projected yield, and CO2 concentrations.
# We assume normal distribution for continuous variables to simulate a realistic variation in the data.
# Temperature change and CO2 concentrations are particularly crucial in studying climate impact on agriculture.

set.seed(1008)

# List of countries to sample from
countries <- c("Japan", "China", "India", "South Korea", "Indonesia", 
               "Thailand", "Vietnam", "Malaysia", "Philippines", "Singapore")

# Initialize a tibble with simulated data
simulated_agri_data <- tibble(
  "Country" = sample(countries, size = 50, replace = TRUE),
  "TemperatureChange" = rnorm(50, mean = 1.5, sd = 0.5),
  "PrecipitationChange" = rnorm(50, mean = 100, sd = 30),
  "ProjectedYield" = rnorm(50, mean = 6.5, sd = 1),
  "CO2Concentrations" = rnorm(50, mean = 432, sd = 3)
)

# Output the simulated data
print(simulated_agri_data)

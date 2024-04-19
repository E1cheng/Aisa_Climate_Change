#### Preamble ####
# Purpose: Data Integrity Tests
# Author: Cheng Yang
# Date: 28 March 2024
# Contact: yvonneyang.cheng@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(here)
library(arrow)

#### Test data ####

# Load the dataset
agriculture_production <- read_parquet(here::here("data/analysis_data/agriculture.parquet"))

# Test 1: Check for missing values in specific columns
test_that("Critical columns contain no missing values", {
  critical_columns <- c("country", "climate_impact_mean", "climate_impact_median", "climate_impact_sd")
  for (col in critical_columns) {
    expect_true(all(!is.na(agriculture_production[[col]])), info = paste("Missing values found in", col))
  }
})

# Test 2: Validate that climate impact measures are within expected bounds
test_that("Climate impact measures are within expected bounds", {
  expect_true(all(agriculture_production$climate_impact_mean > -100 & agriculture_production$climate_impact_mean < 100))
  expect_true(all(agriculture_production$climate_impact_median > -100 & agriculture_production$climate_impact_median < 100))
})

# Test 3: Verify that the standard deviation of climate impacts is within a plausible range
test_that("Standard deviation of climate impacts is within a plausible range", {
  expect_true(all(agriculture_production$climate_impact_sd >= 0), info = "Climate impact standard deviation should not be negative.")
  expect_true(all(agriculture_production$climate_impact_sd < 10), info = "Climate impact standard deviation should be less than 10 for all countries.")
})


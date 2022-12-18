## Load packages
library(tidyverse)
library(lme4)

## Load helper functions
source("estimation_functions.R")

## Load data
ctsib <- read_csv("ctsib.csv")

## Define response (stable or not)
ctsib <- ctsib %>%
  mutate(stable = 1 * (CTSIB == 1))

## Fit model to ctsib data. Fixed effects in the model include:
##  (Intercept) -- the intercept
##  Surface -- a categorical variable with two levels (foam and norm)
##  Vision -- a categorical variable with three levels (closed, dome, open)

ctsib_fit <- run_model(ctsib, "ctsib")

## Components

# 1) Estimated beta parameters
ctsib_fit$beta

# 2) Test statistics for each of the beta parameters
ctsib_fit$test_stat

# 3) Estimated variance parameter
ctsib_fit$sigmasq

# 4) Estimated random effects
ctsib_fit$re


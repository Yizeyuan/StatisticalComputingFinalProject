## Load packages
library(tidyverse)
library(lme4)

## Load helper functions
source("estimation_functions.R")

## Load data
tortoise <- read_csv("gopher_tortoise.csv")

## Fit model to tortoise data. Fixed effects in the model are:
##   (Intercept) -- the intercept
##   prev -- the seroprevalence
##   year -- year effect (as a categorical variable)

tortoise_fit <- run_model(tortoise, "tortoise")

## You can ignore the note about boundary (singular)

## Components

# 1) Estimated beta parameters
tortoise_fit$beta

# 2) Test statistics for each of the beta parameters
tortoise_fit$test_stat

# 3) Estimated variance parameter
tortoise_fit$sigmasq

# 4) Estimated random effects
tortoise_fit$re


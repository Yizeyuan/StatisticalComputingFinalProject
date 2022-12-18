## Load packages
library(tidyverse)
library(lme4)
library(broom)
library(broom.mixed)

## Load helper functions
source("estimation_functions.R")

## Load data
ctsib <- read_csv("ctsib.csv")
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

##### Binomial Model #####
## Load data
original_data <- ctsib
data_test <- original_data %>%
  mutate(surface_norm = ifelse(Surface == 'norm',1, 0),
         vision_open = ifelse(Vision == 'open',1,0),
         vision_dome = ifelse(Vision == 'dome', 1, 0))

# Resample Random Effect
# Note: this generates one set of random effects that is used for each bootstrap
# data set. 
Z_i <- rnorm(40,mean=0, sd=sqrt(ctsib_fit$sigmasq))
Z_i <- rep(Z_i, each=12)

# mu_hat
mu_hat <- exp(ctsib_fit$beta[1]+
                ctsib_fit$beta[2]*data_test$surface_norm+
                ctsib_fit$beta[3]*data_test$vision_dome+
                ctsib_fit$beta[4]*data_test$vision_open+Z_i)/(1+exp(ctsib_fit$beta[1]+
                                                                 ctsib_fit$beta[2]*data_test$surface_norm+
                                                                 ctsib_fit$beta[3]*data_test$vision_dome+
                                                                 ctsib_fit$beta[4]*data_test$vision_open+Z_i))


## Resampling B set of Y_hat from binomial  ##
B=10

data_test_new <- data_test %>%
  mutate(mu_hat = mu_hat) %>%
  crossing(B=1:B) %>%
  mutate(Y_hat = rbernoulli(n(), mu_hat))%>%
  mutate(Yhat = ifelse(Y_hat == "TRUE", 1,0))

bootstrap_test <- data_test_new %>%
  nest_by(B)%>%
  summarize(tidy(glmer(Y_hat ~ Surface + Vision + (1|Subject),
                       family = binomial,
                       data = data)), .groups = "drop")


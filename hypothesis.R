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
Z_i <- rnorm(40,mean=0, sd=sqrt(ctsib_fit$sigmasq))
Z_i <- rep(Z_i, each=12)

# mu_hat
mu_hat <- exp(ctsib_fit$beta[1]+Z_i)/(1+exp(ctsib_fit$beta[1]+Z_i))


## Resampling B set of Y_hat from binomial  ##
B=50
data_test_new <- data_test %>%
  mutate(mu_hat = mu_hat) %>%
  crossing(B=1:B) %>%
  mutate(Y_hat = rbernoulli(n(), mu_hat))%>%
  mutate(Yhat = ifelse(Y_hat == "TRUE", 1,0))

bootstrap_test <- data_test_new %>%
  group_by(B)%>%
  summarize(tidy(glmer(Y_hat ~ Surface + Vision + (1|Subject),
                       family = binomial,
                       data = data)), .groups = "drop")



# F-test
# Null-Hypothesis
ctsib_fit <- lm(log(Diff) ~ Magnitude, data = earthquakes)
(lm_summ <- summary(lm_fit))
slope <- lm_fit$coefficients[2] # Extract estimated slope
t_value <- lm_summ$coefficients["Magnitude",3] # Extract test statistic


## Option 2: Resampling predictors and responses

## Generate bootstrap samples under null hypothesis (no effect of magnitude)
B <- 1000

bootstrap <- tibble(B = 1:B) %>%
  crossing(earthquakes) %>%
  group_by(B) %>%
  summarize(Index1 = sample(n, n, replace = TRUE),
            Index2 = sample(n, n, replace = TRUE),
            Diff = Diff[Index1],
            Magnitude = Magnitude[Index2],
            .groups = "drop") %>%
  nest_by(B) %>%
  summarize(tidy(lm(log(Diff) ~ Magnitude, data = data)), .groups = "drop")

## Sampling distribution of slope given null hypothesis
bootstrap %>%
  filter(term == "Magnitude") %>%
  ggplot(aes(x = statistic)) +
  geom_density() + 
  geom_vline(xintercept = t_value, col = "red") +
  ylab("Bootstrap Sampling Distribution (Null)") +
  xlab("Slope")

## Compute p-value
bootstrap %>% 
  filter(term == "Magnitude") %>%
  summarize(p = mean(abs(statistic) > abs(t_value)))




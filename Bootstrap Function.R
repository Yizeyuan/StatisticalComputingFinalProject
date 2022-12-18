## Bootstrap
## Load packages
library(tidyverse)
library(tidymodels)
library(GGally)
library(ggsci)
library(cvms)
library(rpart)
library(rpart.plot)
library(kernlab)
library(dplyr)
source("estimation_functions.R")
## Load packages
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

##### Binomial Model #####
## Load data
original_data <- read.csv("ctsib.csv")
result <- function(original_data,B){
  ctsib_fit <- run_model(original_data, "ctsib")
  n=nrow(original_data)
  data <- original_data %>% mutate(ifelse(Surfece == 'foam',0,1)
  )

# Resample Random Effect
  Z_i <- rnorm(40,mean=0, ctsib_fit$sigmasq)
  Z_i <- rep(Z_i, each=12)
  
# mu_hat  
  mu_hat <- exp(ctsib_fit$beta[1]+ctsib_fit$stabbeta[2]+beta[3])
  
  
}
  








# since the goal of this analysis is to determine how the surface and vision treatments affect a person’s stability,
# only the relevant data would be used.
original_relevant_data <- select(original_data, Surface, Vision, CTSIB)
head(original_data)
table(original_relevant_data$Surface)


### Data pre-processing


# foam -> 1, norm -> 2
original_relevant_data$Surface[original_relevant_data$Surface == "foam"] <- 1
original_relevant_data$Surface[original_relevant_data$Surface == "norm"] <- 2
table(original_relevant_data$Surface)
table(original_relevant_data$Vision)
original_relevant_data$Vision[original_relevant_data$Vision == "closed"] = 1
original_relevant_data$Vision[original_relevant_data$Vision == "dome"] = 2
original_relevant_data$Vision[original_relevant_data$Vision == "open"] = 3
table(original_relevant_data$Vision)


# fill the na value with 0
original_relevant_data[is.na(original_relevant_data)] <- 0;

# replace the column value of the CTSIB to 2 when its value is greater than 1
original_relevant_data$CTSIB[original_relevant_data$CTSIB <= 1] <- 0
original_relevant_data$CTSIB[original_relevant_data$CTSIB > 1] <- 1
original_relevant_data$CTSIB


plot(original_relevant_data$CTSIB)

### Get the formula
x1 <- original_relevant_data$Surface
x2 <- original_relevant_data$Vision
y <-  original_relevant_data$CTSIB
formula_str <- y ~ x1 + x2
family <- "binomial"



### Fit the binomial model

## When fitting the binomial model, the estimated probability of success for the logistic regression model:
  binomial_model <- glm(data = original_relevant_data, formula = formula_str, family = family)
binomial_model 


summary(binomial_model)

# plot the model to get some result of Residuals vs Fitted, Normal Q-Q, Scale-Location, and Residuals vs Leverage.
plot(binomial_model)
plot(density(residuals(binomial_model)))































## Compute time between earthquakes in days
earthquakes <- earthquakes %>%
  arrange(Time) %>%
  mutate(Diff = as.numeric(Time - lag(Time))/(60 * 60 * 24),
         Magnitude = lag(Magnitude)) %>%
  filter(!is.na(Diff)) %>%
  rowid_to_column("ID") %>%
  select(ID,Diff,Magnitude)

## Plot density of time differences
dens_plot <- earthquakes %>%
  ggplot(aes(x = Diff)) +
  geom_density()

dens_plot

## Compute MLE of mean and rate
n <- nrow(earthquakes)
mu_hat <- mean(earthquakes$Diff)
lambda_hat <- 1/mu_hat

## Compute se and 95% CI for lambda based on delta method
se_lambda_hat <- lambda_hat/sqrt(n)
ci_lambda_hat <- lambda_hat + c(-1,1) * se_lambda_hat

## Compare exponential with smoothed density
exp_df <- tibble(x = seq(0,3,length = 100)) %>%
  mutate(f = dexp(x, rate = lambda_hat))

dens_plot + 
  geom_line(data = exp_df,aes(x = x, y = f))

## QQ-plot
earthquakes <- earthquakes %>%
  arrange(Diff) %>%
  mutate(p = (1:n() - .5)/n(),
         q = qexp(p, lambda_hat))

earthquakes %>%
  ggplot(aes(x = q, y = Diff)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Theoretical Quantile") + 
  ylab("Sample Quantile")

##### Parametric Bootstrap #####

## Generate samples
B <- 1000 # Number of bootstrap samples

bootstrap1 <- tibble(B = 1:1000) %>%
  group_by(B) %>%
  summarize(d = rexp(n, lambda_hat), .groups = "keep") %>%
  summarize(mu_hat = mean(d),
            lambda_hat = 1/mean(d))

## Bootstrap standard deviation
sd(bootstrap1$lambda_hat)

## Bootstrap confidence interval
quantile(bootstrap1$lambda_hat,c(.025,.975))

## Bootstrap bias
mean(bootstrap1$lambda_hat) - lambda_hat

## Bootstrap hypothesis test

##### Non-parametric Bootstrap #####

## Generate samples
B <- 1000 # Number of bootstrap samples

bootstrap1 <- tibble(B = 1:1000) %>%
  group_by(B) %>%
  summarize(d = sample(earthquakes$Diff,n,replace = TRUE), .groups = "keep") %>%
  summarize(mu_hat = mean(d),
            lambda_hat = 1/mean(d))

## Compute bootstrap standard deviation
sd(bootstrap1$lambda_hat)

## Compute bootstrap confidence interval
quantile(bootstrap1$lambda_hat,c(.025,.975))

## Bootstrap bias 
mean(bootstrap1$lambda_hat) - lambda_hat























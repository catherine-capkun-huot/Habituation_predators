# Description -------------------------------------------------------------

#### ### ### ## #### ### ### ## #### ### ### ## #### ### ### ## #### ### ## 

# Nonlinear Mixed Models & FID 
# Created by Catherine Čapkun-Huot & Andrew MacDonald 
# Date: 20/10/2022

#### ### ### ## #### ### ### ## #### ### ### ## #### ### ### ## #### ### ## 


# Libraries ---------------------------------------------------------------

# Load packages
library(tidyverse)
library(tidybayes)
library(brms)


# Select priors -----------------------------------------------------------

# Select priors before loading data

# Formula of the model 
nonlin_form <- bf(FID ~ inv_logit(logitM) * 1000 * (1 - inv_logit(logitp)*num_obs/(exp(logd) + num_obs)),
                  logitM ~ 1 + (1 |t| tamia_id),
                  logitp ~ 1 + (1 |t| tamia_id),
                  logd ~ 1 + (1 |t| tamia_id),
                  nl = TRUE,
                  family = Gamma(link = "identity"))

# Create fake data to get priors
fake_id <- expand.grid(tamia_id = paste0("t", 1:40),
                       num_obs = (0:15), 
                       FID = 200)

# Get priors - we use this to know what priors we need to assign 
get_prior(nonlin_form, data = fake_id)

# Assign priors 
nonlin_priors <- c(prior(lkj(2), class = "cor"),
                   prior(exponential(2), class = "sd", nlpar = "logd"),
                   prior(exponential(4), class = "sd", nlpar = "logitM"),
                   prior(exponential(2), class = "sd", nlpar = "logitp"),
                   prior(normal(1.5, .5), class = "b", nlpar = "logd"),
                   prior(normal(.5,.5), class = "b", nlpar = "logitM"),
                   prior(normal(-1, .2), class = "b", nlpar = "logitp"),
                   prior(gamma(6.25, .25), class = "shape"))

# Run the model with the assigned priors by sampling ONLY from the priors (not from the data or fake data)
# The predictions from this model will tell us that the priors are OK if they roughly encompass the range of biologically possible values
nonlin_fit <- brm(nonlin_form,
                  data = fake_id,
                  prior = nonlin_priors,
                  backend = "cmdstanr",
                  file = "prior_nonlin", 
                  sample_prior = "only",
                  file_refit = "on_change")

# Get output
summary(nonlin_fit)

# Plot it to see better
# Plot 12 draws of 40 chipmunks
fake_tamia_id_epred <- fake_id |> 
  add_epred_draws(nonlin_fit, ndraws =12)
fake_tamia_id_epred |> 
  ggplot(aes(x = num_obs, y = .epred, colour = tamia_id)) + 
  geom_line() + 
  facet_wrap(~.draw) + 
  guides(colour = "none")

# It looks fine



# Data --------------------------------------------------------------------

# Load FID data - data_FID_analyses.csv -, each row is a FID trial
data_FID <- read.csv("data_FID_analyses.csv", 
                     header = TRUE, 
                     sep = ",")

# Data structure
data_FID <- tibble(ID      = as.factor(data_FID$ID),
                   num_obs = as.numeric(data_FID$obs_number),
                   FID     = as.numeric(data_FID$FID + 0.01), # gamma distribution needs values > 0 
                   risk    = as.factor(data_FID$Approach_speed))

# Change risk category names
levels(data_FID$risk) <- list(Low  = "20", Medium = "40", High = "60")

# Load individual characteristics data - ind_charact.csv -, each row is an individual 
data_ind <- read.csv("ind_charact.csv", 
                     header = TRUE, 
                     sep = ";")

# Data structure
data_ind <- tibble(ID    = as.factor(data_ind$ID),
                   sex   = as.factor(data_ind$Sex),
                   explo = as.numeric(data_ind$Exploration),
                   docil = as.numeric(data_ind$Docility), 
                   capt  = as.numeric(data_ind$Number_captures))

# Join data on FID with data on behaviour in one combined data set 
data_all <- dplyr::left_join(data_FID, data_ind, by = "ID")


# FID Models ---------------------------------------------------------------

## First model - ID as random effect ----

# Formula
form_fit1 <- bf(FID    ~ inv_logit(logitM) * 1000 * (1 - inv_logit(logitp)*num_obs/(exp(logd) + num_obs)),
                logitM ~ 1 + risk + (1 |t| ID),
                logitp ~ 1 + risk + (1 |t| ID),
                logd   ~ 1 + risk + (1 |t| ID),
                nl     = TRUE,
                family = Gamma(link = "identity"))

# Assign priors
priors <- c(prior(lkj(2), class = "cor"),
            prior(exponential(2),   class = "sd", nlpar = "logd"),
            prior(exponential(2),   class = "sd", nlpar = "logitM"),
            prior(exponential(2),   class = "sd", nlpar = "logitp"),
            prior(normal(1.5, .5),  class = "b",  nlpar = "logd"),
            prior(normal(.5,.5),    class = "b",  nlpar = "logitM"),
            prior(normal(1, .2),   class = "b",  nlpar = "logitp"),
            prior(gamma(6.25, .25), class = "shape"))

# Run model 1
fit1 <- brm(form_fit1,
            data        = data_FID,
            prior       = priors,
            seed        = 1234,
            adapt_delta = 0.95,
            core        = 3,
            iter        = 5000,
            backend     = "cmdstanr",
            file        = "fit1", 
            file_refit  = "on_change")

# Output
summary(fit1)

# Inspect model
#shinystan::launch_shinystan(fit1)


## Second model - ID as random effect nested in risk treatments ----

form_fit2 <- bf(FID    ~ inv_logit(logitM) * 1000 * (1 - inv_logit(logitp)*num_obs/(exp(logd) + num_obs)),
                logitM ~ 1 + risk + (1 | gr(ID, by = risk, id = "t")),
                logitp ~ 1 + risk + (1 | gr(ID, by = risk, id = "t")),
                logd   ~ 1 + risk + (1 | gr(ID, by = risk, id = "t")),
                nl     = TRUE,
                family = Gamma(link = "identity"))

# Run model 2
fit2 <- brm(form_fit2,
            data        = data_FID,
            prior       = priors,
            seed        = 1234,
            adapt_delta = 0.95,
            core        = 3,
            iter        = 5000,
            backend     = "cmdstanr",
            file        = "fit2", 
            file_refit  = "on_change")

# Output
summary(fit2)

# Compare first and second models
loo_compare(loo(fit1), loo(fit2))


## Third model - no random effects ----

# Formula
form_fit3 <- bf(FID    ~ inv_logit(logitM) * 1000 * (1 - inv_logit(logitp)*num_obs/(exp(logd) + num_obs)),
                logitM ~ 1 + risk,
                logitp ~ 1 + risk,
                logd   ~ 1 + risk,
                nl     = TRUE,
                family = Gamma(link = "identity"))

# Same priors but removed the ones needed for group-level effects
priors_fit3 <- c(prior(normal(1.5, .5),  class = "b", nlpar = "logd"),
                 prior(normal(.5,.5),    class = "b", nlpar = "logitM"),
                 prior(normal(-1, .2),   class = "b", nlpar = "logitp"),
                 prior(gamma(6.25, .25), class = "shape"))

# Model 3 
fit3 <- brm(form_fit3,
            data        = data_FID,
            prior       = priors_fit3,
            seed        = 1234,
            adapt_delta = 0.95,
            core        = 3,
            iter        = 5000,
            backend     = "cmdstanr",
            file        = "fit3", 
            file_refit  = "on_change")

# Compare first and third models
loo_compare(loo(fit1), loo(fit3))


## Fourth model - add fixed effects ----

# Formula
form_fit4 <- bf(FID    ~ inv_logit(logitM) * 1000 * (1 - inv_logit(logitp)*num_obs/(exp(logd) + num_obs)),
                logitM ~ 1 + risk + scale(capt) + (1 |t| ID),
                logitp ~ 1 + risk + sex + scale(docil) + scale(explo) + (1 |t| ID),
                logd   ~ 1 + risk + (1 |t| ID),
                nl     = TRUE,
                family = Gamma(link = "identity"))

# Model 4
fit4 <- brm(form_fit4,
            data        = data_all,
            prior       = priors,
            seed        = 1234,
            adapt_delta = 0.95,
            cores       = 3,
            iter        = 5000,
            backend     = "cmdstanr",
            file        = "fit4", 
            file_refit  = "on_change")

# Output
summary(fit4)



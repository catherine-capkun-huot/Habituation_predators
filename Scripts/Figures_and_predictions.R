# Description -------------------------------------------------------------

#### ### ### ## #### ### ### ## #### ### ### ## #### ### ### ## #### ### ## 

# Nonlinear Mixed Models & FID - Figures & predictions
# This code follows the Nonlinear Mixed Models & FID script and uses objects created from it
# Created by Catherine Čapkun-Huot
# Date: 20/10/2022

#### ### ### ## #### ### ### ## #### ### ### ## #### ### ### ## #### ### ## 

# Libraries ---------------------------------------------------------------

# Load packages
library(tidyverse)
library(tidybayes)
library(brms) # models
library(sjPlot) # tables
library(modelr) # needed for data_grid (Figure 2)
library(ggpubr) # needed for ggarrange - combine plots
library(cowplot) # other package to combine plots
library(ggplot2) # figures

# Data --------------------------------------------------------------------

# Upload individual characteristic data again to get risk levels for some figures
data_ind2 <- read.csv("ind_charact.csv", 
                      header = TRUE, 
                      sep = ";")

data_ind2 <- tibble(ID        = as.factor(data_ind2$ID),
                    explo     = as.numeric(data_ind2$Exploration),
                    docil     = as.numeric(data_ind2$Docility), 
                    risk      = as.factor(data_ind2$Risk))

# Change risk category names
levels(data_ind2$risk) <- list(Low  = "20", Medium = "40", High = "60")



# Figures -----------------------------------------------------------------

## Figure 1 ----
# Model illustration - not made in R

## Figure 2 ----

# Habituation responses  of the 40 chipmunks predicted from the posterior for each risk treatment 
# Lines are extrapolated to 15 tests

# Predictions from the posterior
obs_pred <- add_epred_draws(data_FID, 
                            fit1, 
                            seed = 1234)  

# Predictions extrapolated to 15 tests
obs_pred_extra <- data_FID |>
                  data_grid(nesting(risk, ID), num_obs = 0:15) |> 
                  add_epred_draws(fit1)


# Plot it 
p2 <- ggplot() +
      geom_point(data = data_all, aes(x = num_obs, y = FID), shape = 1, size = 1) + 
      stat_lineribbon(data = obs_pred_extra, aes(x = num_obs, y = .epred, group = ID), size = 0.5, colour = "#0066CC", .width = .95, alpha = 0.15) +
      scale_fill_brewer(palette = "Blues") + 
      stat_lineribbon(data = obs_pred_extra, aes(x = num_obs, y = .epred, group = ID), size = 0.5, colour = "#004C99", .width = 0) + #lines without alpha value
      guides(colour = "none") + 
      coord_cartesian(ylim = c(0,1000)) + 
      facet_wrap(~ risk) +
      theme_bw() + 
      theme(legend.position = "none") + 
      xlab("Trial order") + 
      ylab("FID (cm)")

# Change line colour for line section that is not extrapolated
p2 <- p2 + stat_lineribbon(data = obs_pred, aes(x = num_obs, y = .epred, group = ID), size = 0.5, colour = "#001933", .width = 0)  

# Save it
ggsave(filename = "Figure2.jpeg",
       plot     = p2, 
       device   = "jpeg",
       width    = 129, 
       height   = 100, 
       units    = "mm", 
       dpi      = 300)


## Figure 3 ----

### Figure 3abc ----
### Sex differences

# Calculate difference between males and females at low risk and with all other variables set to the mean
#inv_logit_scaled(logitp_Intercept) - inv_logit_scaled(logitp_Intercept + logitp_sexM)
inv_logit_scaled(-0.89) - inv_logit_scaled(-0.89 + -0.98)

# Create mean male and female individuals for each risk level
mean_tamia_sex <- expand_grid(risk    = c("Low", "Medium", "High"),
                              sex     = c("M", "F"),
                              ID      = "chipmunk",
                              explo   = 48.9, # mean
                              docil   = 11.32, # mean
                              capt    = 24.38, #mean 
                              num_obs = 0:15) |>
                  mutate(ID = paste0(ID, "_", risk, "_", sex))  |>
                  add_epred_rvars(fit4, 
                                  re_formula = NA, 
                                  seed = 1234) 

# Plot it
p3abc <- ggplot() +
         geom_point(data = data_all, aes(x = num_obs, y = FID, color = sex, shape = sex), alpha = 0.5, size = 1.5) +
         facet_wrap(~fct_relevel(risk,'Low','Medium','High')) +
         stat_lineribbon(data = mean_tamia_sex, aes(x = num_obs, ydist = .epred, color = sex, fill = sex), size = 1, .width = c(.95)) +
         stat_lineribbon(data = mean_tamia_sex, aes(x = num_obs, ydist = .epred, color = sex, fill = sex), size = 1, .width = 0) +
         theme_bw() +
         coord_cartesian(ylim = c(100,500)) + 
         scale_color_manual(name = "Sex", values = c("#031A6B", "#087CA7"),labels = c('Female','Male')) +  
         scale_fill_manual(name = "Sex", values = c("#031A6B26", "#087CA726"),labels = c('Female','Male')) +
         scale_shape_manual(name = "Sex", values = c(16, 17), labels = c('Female','Male')) + 
         xlab("Trial order") + 
         ylab("FID (cm)")

# Contrast figure (not in paper)

# Low risk
new_tamia_sex <- expand_grid(risk    = "Low",
                             sex     = c("M", "F"),
                             ID      = "chipmunk",
                             explo   = 48.9, # mean
                             docil   = 11.32, # mean
                             capt    = 24.38, # mean 
                             num_obs = 0:15)  |>
                 add_epred_rvars(fit4, 
                                 re_formula = NA, 
                                 seed = 1234) 

pred_contrast_sex <- new_tamia_sex |>
                     pivot_wider(names_from = sex, values_from = .epred) |>
                     mutate(contrast_sex = M - F)

# Plot it 
pred_contrast_sex |>
ggplot(aes(x = num_obs, dist = contrast_sex)) + 
stat_lineribbon(color = "#08519C", .width = 0.95) +
scale_fill_brewer() + 
geom_hline(yintercept = 0) + 
guides(colour = "none") + 
theme_bw() + 
xlab("") + 
ylab("FID (cm)") + 
coord_cartesian(ylim = c(0, 80)) +
theme(legend.position = "none") + 
theme(plot.margin = unit(c(5.5, 1.5, 3, 10), "pt"))


# Calculate probability that FID is greater for male
pred_contrast_sex |> mutate(bayesian_p = sum(contrast_sex < 0)/5000)

# Medium risk
new_tamia_sex_40 <- expand_grid(risk    = "Medium",
                                sex     = c("M", "F"),
                                ID      = "chipmunk",
                                explo   = 48.9, # mean
                                docil   = 11.32, # mean
                                capt    = 24.38, #mean 
                                num_obs = 0:15)  |>
                    add_epred_rvars(fit4, 
                                    re_formula = NA, 
                                    seed = 1234) 

pred_contrast_sex_40 <- new_tamia_sex_40 |>
                        pivot_wider(names_from = sex, values_from = .epred) |>
                        mutate(contrast_sex = M - F)

pred_contrast_sex_40 |>
ggplot(aes(x = num_obs, dist = contrast_sex)) + 
stat_lineribbon(color = "#08519C", .width = 0.95) +
scale_fill_brewer() + 
geom_hline(yintercept = 0) + 
guides(colour = "none") + 
theme_bw() + 
xlab("Trial order") + 
coord_cartesian(ylim = c(0, 80)) +
theme(axis.title.y = element_blank(), 
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank()) +
theme(legend.position = "none") + 
theme(plot.margin = unit(c(5.5, 1.5, 3, 1.5), "pt"))


# High risk
new_tamia_sex_60 <- expand_grid(risk    = "High",
                                sex     = c("M", "F"),
                                ID      = "chipmunk",
                                explo   = 48.9, # mean
                                docil   = 11.32, # mean
                                capt    = 24.38, #mean 
                                num_obs = 0:15)  |>
                    add_epred_rvars(fit4, 
                                    re_formula = NA, 
                                    seed = 1234) 

pred_contrast_sex_60 <- new_tamia_sex_60 |>
                        pivot_wider(names_from = sex, values_from = .epred) |>
                        mutate(contrast_sex = M - F)

pred_contrast_sex_60 |>
ggplot(aes(x = num_obs, dist = contrast_sex)) + 
stat_lineribbon(color = "#08519C", .width = 0.95) +
scale_fill_brewer() + 
geom_hline(yintercept = 0) + 
guides(colour = "none") + 
theme_bw() + 
xlab("") + 
theme(axis.title.y = element_blank(), 
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank()) +
coord_cartesian(ylim = c(0, 80)) +
theme(legend.position = "none") + 
theme(plot.margin = unit(c(5.5, 5.5, 3, 1), "pt"))


### Figure 3def ----
# Exploration differences

# Create mean individuals for the min, mean and max values of exploration for each risk level
mean_tamia_explo <- expand_grid(risk    = c("Low", "Medium", "High"),
                                ID      = "chipmunk",
                                sex     = "F",
                                explo   = c(0, 48.9, 125), # minimum, mean and maximum values
                                docil   = 11.32, # mean
                                capt    = 24.38, # mean
                                num_obs = 0:15) |>
                    mutate(ID = paste0(ID, "_", risk, "_", explo))  |>
                    add_epred_rvars(fit4, 
                                    re_formula = NA,
                                    seed = 1234)

mean_tamia_explo$explo <- as.factor(mean_tamia_explo$explo) # categories

# Plot it
p3def <- ggplot() +
         geom_point(data = data_all, aes(x = num_obs, y = FID), alpha = 0.3, size = 1.5) +
         facet_wrap(~fct_relevel(risk,'Low','Medium','High')) +
         stat_lineribbon(data = mean_tamia_explo, aes(x = num_obs, ydist = .epred, color = explo, fill = explo), size = 1, .width = c(.95)) +
         stat_lineribbon(data = mean_tamia_explo, aes(x = num_obs, ydist = .epred, color = explo, fill = explo), size = 1, .width = 0) +
         theme_bw() +
         coord_cartesian(ylim = c(100,500)) + 
         scale_fill_manual(name   = "Exploration", 
                           values = c( "#00330026","#6D8E2B26", "#A1C02726"), 
                           labels = c('Minimum','Mean', 'Maximum')) + 
         scale_color_manual(name   = "Exploration", 
                            values = c( "#003300E6","#6D8E2BE6","#A1C027E6"), 
                            labels = c('Minimum','Mean', 'Maximum')) +
         xlab("Trial order") + 
         ylab("FID (cm)")


# Calculate contrasts
#Low risk
explo_low <- expand_grid(risk    = "Low",
                         ID      = "chipmunk",
                         sex     = "F",
                         explo   = c(0, 48.9, 125), # min, mean. max 
                         docil   = 11.32, # mean
                         capt    = 24.38, # mean
                         num_obs = 0:15) |>
             add_epred_rvars(fit4, re_formula = NA, seed = 1234)

explo_low$explo <- as.factor(explo_low$explo) # categories 

pred_contrast_explo_low <- explo_low |>
                           pivot_wider(names_from = explo, values_from = .epred) |>
                           mutate(contrast_explo = `125` - `0`)

#Medium risk
explo_med <- expand_grid(risk    = "Medium",
                         ID      = "chipmunk",
                         sex     = "F",
                         explo   = c(0, 48.9, 125), # min, mean. max 
                         docil   = 11.32, # mean
                         capt    = 24.38, # mean
                         num_obs = 0:15) |>
             add_epred_rvars(fit4, re_formula = NA, seed = 1234)

explo_med$explo <- as.factor(explo_med$explo) # categories 

pred_contrast_explo_med <- explo_med |>
                           pivot_wider(names_from = explo, values_from = .epred) |>
                           mutate(contrast_explo = `125` - `0`)

#High risk
explo_high <- expand_grid(risk    = "High",
                          ID      = "chipmunk",
                          sex     = "F",
                          explo   = c(0, 48.9, 125), # min, mean. max 
                          docil   = 11.32, # mean
                          capt    = 24.38, # mean
                          num_obs = 0:15) |>
              add_epred_rvars(fit4, re_formula = NA, seed = 1234)

explo_high$explo <- as.factor(explo_high$explo) # categories 

pred_contrast_explo_high <- explo_high |>
                            pivot_wider(names_from = explo, values_from = .epred) |>
                            mutate(contrast_explo = `125` - `0`)


### Figure 3ghi ----
# Docility differences

# Create mean individuals for the min, mean and max values of docility for each risk level
mean_tamia_docil <- expand_grid(risk    = c("Low", "Medium", "High"),
                                ID      = "chipmunk",
                                sex     = "F",
                                explo   = 48.9, # mean 
                                docil   = c(0, 11.32, 60), # min, mean. max
                                capt    = 24.38, # mean
                                num_obs = 0:15) |>
                    mutate(ID = paste0(ID, "_", risk, "_", docil))  |>
                    add_epred_rvars(fit4, re_formula = NA, seed = 1234)

mean_tamia_docil$docil <- as.factor(mean_tamia_docil$docil) # categories 


# Plot it
p3ghi <- ggplot() +
         geom_point(data = data_all, aes(x = num_obs, y = FID), alpha = 0.3, size = 1.5) +
         facet_wrap(~fct_relevel(risk,'Low','Medium','High')) +
         stat_lineribbon(data = mean_tamia_docil, aes(x = num_obs, ydist = .epred, color = docil, fill = docil), size = 1, .width = c(.95)) +
         stat_lineribbon(data = mean_tamia_docil, aes(x = num_obs, ydist = .epred, color = docil, fill = docil), size = 1, .width = 0) +
         theme_bw() +
         coord_cartesian(ylim = c(100,500)) + 
         scale_fill_manual(name   = "Docility", 
                           values = c( "#00330026","#6D8E2B26", "#A1C02726"), 
                           labels = c('Minimum','Mean', 'Maximum')) + 
         scale_color_manual(name   = "Docility", 
                            values = c( "#003300E6","#6D8E2BE6","#A1C027E6"), 
                            labels = c('Minimum','Mean', 'Maximum')) +
         xlab("Trial order") + 
         ylab("FID (cm)")


# Calculate contrasts
#Low risk
docil_low <- expand_grid(risk    = "Low",
                         ID      = "chipmunk",
                         sex     = "F",
                         explo   = 48.9, # mean 
                         docil   = c(0, 11.32, 60), # min, mean. max
                         capt    = 24.38, # mean
                         num_obs = 0:15) |>
             add_epred_rvars(fit4, re_formula = NA, seed = 1234)

docil_low$docil <- as.factor(docil_low$docil) # categories 

pred_contrast_docil_low <- docil_low |>
                           pivot_wider(names_from = docil, values_from = .epred) |>
                           mutate(contrast_docil = `60` - `0`)

#Medium risk
docil_med <- expand_grid(risk    = "Medium",
                         ID      = "chipmunk",
                         sex     = "F",
                         explo   = 48.9, # mean 
                         docil   = c(0, 11.32, 60), # min, mean. max
                         capt    = 24.38, # mean
                         num_obs = 0:15) |>
             add_epred_rvars(fit4, re_formula = NA, seed = 1234)

docil_med$docil <- as.factor(docil_med$docil) # categories 

pred_contrast_docil_med <- docil_med |>
                           pivot_wider(names_from = docil, values_from = .epred) |>
                           mutate(contrast_docil = `60` - `0`)

#High risk
docil_high <- expand_grid(risk    = "High",
                          ID      = "chipmunk",
                          sex     = "F",
                          explo   = 48.9, # mean 
                          docil   = c(0, 11.32, 60), # min, mean. max
                          capt    = 24.38, # mean
                          num_obs = 0:15) |>
              add_epred_rvars(fit4, re_formula = NA, seed = 1234)

docil_high$docil <- as.factor(docil_high$docil) # categories 

pred_contrast_docil_high <- docil_high |>
                            pivot_wider(names_from = docil, values_from = .epred) |>
                            mutate(contrast_docil = `60` - `0`)


# Put three figures together
p3 <- plot_grid(p3abc, p3def, p3ghi, ncol = 1, align = "v")


# Save it
ggsave(filename = "Figure3.jpeg",
       plot     = p3, 
       device   = "jpeg",
       width    = 177, 
       height   = 177, 
       units    = "mm", 
       dpi      = 300)


## Figure 4 ----
# Hyperparameters (standard deviation)

fit2 |> get_variables()

# Change labels
variable_names <- c(`sd_ID__logd_Intercept`   = "log(d)",
                    `sd_ID__logitM_Intercept` = "logit(m)",
                    `sd_ID__logitp_Intercept` = "logit(p)")

# Get sd distributions 
sd <- fit2 |> gather_rvars(`sd_ID__logitM_Intercept:riskLow`,
                           `sd_ID__logitM_Intercept:riskMedium`,
                           `sd_ID__logitM_Intercept:riskHigh`,
                           `sd_ID__logitp_Intercept:riskLow`,
                           `sd_ID__logitp_Intercept:riskMedium`, 
                           `sd_ID__logitp_Intercept:riskHigh`, 
                           `sd_ID__logd_Intercept:riskLow`, 
                           `sd_ID__logd_Intercept:riskMedium`, 
                           `sd_ID__logd_Intercept:riskHigh`)

sd <- sd |> separate(.variable, into =c("variable", "risk"), sep =":")

# Reorder group factor levels
sd$variable <- factor(sd$variable,
                      levels = c("sd_ID__logitM_Intercept",
                                 "sd_ID__logitp_Intercept",
                                 "sd_ID__logd_Intercept"))

# Reorder risk levels
sd <- sd |> 
  mutate(risk_order = paste0(risk) |>
           forcats::fct_inorder())

# Plot
p4 <- sd |>
      ggplot(aes(ydist = .value, x = risk_order, fill = risk_order)) +
      stat_dist_halfeye()+
      facet_wrap(~variable, labeller = labeller(variable = variable_names)) + 
      theme_bw() + 
      theme(axis.line=element_line()) +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(0,3.5)) + 
      scale_x_discrete(labels = c('Low','Medium','High')) +
      xlab("Risk") + 
      ylab("Standard deviation") + 
      scale_fill_manual(values= c("#32936F", "#FAA613", "#F44708"))

# Save it
ggsave(filename = "Figure4",
       plot     = p4, 
       device   = "jpeg",
       width    = 129, 
       height   = 100, 
       units    = "mm", 
       dpi      = 300)

# Test that the standard deviation of logitp is greater at low risk
hypothesis(fit2, "logitp_Intercept:riskLow > logitp_Intercept:riskMedium", class = "sd", group = "ID", seed = 1234)
hypothesis(fit2, "logitp_Intercept:riskLow > logitp_Intercept:riskHigh", class = "sd", group = "ID", seed = 1234)

# Supplementary figures  --------------------------------------------------

## Figure S1 ----
# Posterior predictive checks
pS1a <- pp_check(fit4, type = "dens_overlay", ndraws = 20) + 
        theme(plot.margin = margin(7, 3, 3, 20, "pt")) + 
        theme(legend.position = c(.8, .8))

pS1b <- pp_check(fit4, type = "stat_grouped", stat   = "median", group = "risk")+ 
        theme(plot.margin = margin(3, 3, 3, 40, "pt")) + 
        guides(shape = guide_legend(override.aes = list(size = 0.5))) + 
        guides(color = guide_legend(override.aes = list(size = 0.5))) + 
        theme(legend.title = element_text(size = 7), 
              legend.text  = element_text(size = 7)) +
        theme(axis.text.x = element_text(size = 5)) 

pS1c <- pp_check(fit4, type = "ecdf_overlay", ndraws = 20)+ 
        theme(plot.margin = margin(7, 3, 20, 4, "pt")) + 
        theme(legend.position = c(.8, .3))

pS1d <- pp_check(fit4, type = "scatter_avg",  ndraws = 20) +
        theme(plot.margin = margin(7, 70, 3, 3, "pt"))

pS1 <- ggarrange(pS1a, pS1b, pS1c, pS1d, widths = c(0.4, 0.6, 0.4, 0.6)) 

# Save it
ggsave(filename = "FigureS1",
       plot     = pS1, 
       device   = "jpeg",
       width    = 177, 
       height   = 100, 
       units    = "mm", 
       dpi      = 300)

## Figure S2 ----
# Habituation curve of each chipmunk (epred function)

true_chipmunks_epred <- data_FID |>
                        add_epred_draws(fit1, 
                                        seed = 1234)

pS2 <- true_chipmunks_epred |> 
       ggplot(aes(x = num_obs, y = .epred, group = ID)) +
       stat_lineribbon(.width = 0.95, color = "#08519C") +
       scale_fill_brewer() + 
       facet_wrap(risk ~ ID) +
       guides(colour = "none") +
       theme_classic() + 
       theme(legend.position = "none") +
       xlab("Trial order") + 
       ylab("FID (cm)") + 
       geom_point(aes(x = num_obs, y = FID), alpha = 0.1)

# Save it
ggsave(filename = "FigureS2",
       plot     = pS2,  
       device   = "png",
       width    = 129, 
       height   = 238, 
       units    = "mm", 
       dpi      = 300)

## Figure S3 ----
# Parameter plots 

### Figure S3 - d ----
# Parameter d

chipmunk_d <- fit1 |> 
              spread_rvars(r_ID__logd[ID,Intercept], 
                           b_logd_Intercept,
                           b_logd_riskMedium,
                           b_logd_riskHigh)

chipmunk_d <- chipmunk_d |> 
              mutate(d_low  = (r_ID__logd + b_logd_Intercept)) |> # get d distributions
              mutate(d_med  = (d_low + b_logd_riskMedium)) |>
              mutate(d_high = (d_low + b_logd_riskHigh)) |>
              select(ID, starts_with("d")) |>
              pivot_longer(cols = -ID, names_to = "risk", values_to = "d")

new_names <- c("d_low" = "Low", "d_med" = "Medium", "d_high" = "High")

chipmunk_d <- mutate(chipmunk_d, risk = new_names[risk])

# Each chipmunk now has a predicted value of d for the three risk levels.
# However, a chipmunk has been exposed to one risk treatment only. 

# We join data sets to keep one d value: the one associated to the risk treatment it was exposed to
chipmunk_d <- dplyr::left_join(data_ind2, 
                               chipmunk_d,
                               by = c("ID", "risk")) 

chipmunk_d$risk <- factor(chipmunk_d$risk, levels = c("Low", "Medium", "High")) # Reorder risk levels

# Plot it
pS3a <- chipmunk_d |> 
        mutate(ID = forcats::fct_reorder(ID, d, median)) |> # order
        ggplot(aes(y = ID, dist = exp(d), color = risk)) + 
        stat_dist_halfeye() + 
        coord_cartesian(xlim = c(0,15)) +
        facet_wrap( ~ risk, scales = "free_y") +
        theme_bw() +
        xlab("d (trial number)") + 
        ylab("Chipmunk ID")+
        scale_color_manual(values= c("#32936F", "#FAA613", "#F44708")) +
        theme(legend.position = "none") + 
        theme(axis.text.y = element_text(size = 10), 
              axis.text.x = element_text(size = 10)) + 
        scale_x_continuous(breaks = c(0, 5, 10, 15))

# Calculate mean min and max for each risk level (refer to chipmunk_d dataset)

# Low risk 
# Min (P056)
exp(0.20)
# Max (P036)
exp(1.13)

# Medium risk
# Min (N004)
exp(2.00)
# Max (N070)
exp(2.35)

# High risk
# Min (P057)
exp(2.04)
# Max (P006)
exp(2.37)

### Figure S3 - m ----
# Parameter m

chipmunk_m <- fit1 |> 
              spread_rvars(r_ID__logitM[ID,Intercept], 
                           b_logitM_Intercept, 
                           b_logitM_riskMedium,
                           b_logitM_riskHigh)

chipmunk_m <- chipmunk_m |> 
              mutate(M_low  = (r_ID__logitM + b_logitM_Intercept)) |> # get M
              mutate(M_med  = (M_low + b_logitM_riskMedium)) |>
              mutate(M_high = (M_low + b_logitM_riskHigh)) |>
              select(ID, starts_with("M")) |>
              pivot_longer(cols = -ID, names_to = "risk", values_to = "M")

new_names <- c("M_low" = "Low", "M_med" = "Medium", "M_high" = "High")

chipmunk_m <- mutate(chipmunk_m, risk = new_names[risk])

chipmunk_m <- dplyr::left_join(data_ind2, 
                               chipmunk_m, 
                               by = c("ID", "risk")) #join data sets to get risk levels

chipmunk_m$risk <- factor(chipmunk_m$risk, levels = c("Low", "Medium", "High"))

# Plot it
pS3b <- chipmunk_m |> 
        mutate(ID = forcats::fct_reorder(ID, M, median)) |> # order
        ggplot(aes(y = ID, dist =(inv_logit_scaled(M)*1000), color = risk)) + 
        stat_dist_halfeye() + 
        coord_cartesian(xlim = c(200,600)) +
        facet_wrap( ~ risk, scales = "free_y") +
        theme_bw() + 
        xlab("m (cm)") + 
        ylab("Chipmunk ID")+
        scale_color_manual(values= c("#32936F", "#FAA613", "#F44708")) +
        theme(legend.position = "none")+ 
        theme(axis.text.y = element_text(size = 10), 
              axis.text.x = element_text(size = 10)) +
        scale_x_continuous(breaks = c(200, 400, 600)) + 
        theme(plot.margin = unit(c(5.5, 8, 5.5, 5.5), "pt"))

# Calculate mean min and max for each risk level (refer to chipmunk_m dataset)

# Low risk 
# Min (P014)
inv_logit_scaled(-0.46) * 1000
# Max (P036)
inv_logit_scaled(-0.32) * 1000

# Medium risk
# Min (R014)
inv_logit_scaled(-0.49) * 1000
# Max (Q001)
inv_logit_scaled(-0.24) * 1000

# High risk
# Min (Q028)
inv_logit_scaled(-0.78) * 1000
# Max (Q061)
inv_logit_scaled(-0.49) * 1000


### Figure S3 - p ----
# Parameter p

chipmunk_p <- fit1 |> 
              spread_rvars(r_ID__logitp[ID,Intercept], 
                           b_logitp_Intercept, 
                           b_logitp_riskMedium,
                           b_logitp_riskHigh)

chipmunk_p <- chipmunk_p |> 
              mutate(p_low = (r_ID__logitp + b_logitp_Intercept))|> # get p
              mutate(p_med = (p_low + b_logitp_riskMedium))|>
              mutate(p_high = (p_low + b_logitp_riskHigh))|>
              select(ID, starts_with("p")) |>
              pivot_longer(cols = -ID, names_to = "risk", values_to = "p")

new_names <- c("p_low" = "Low", "p_med" = "Medium", "p_high" = "High")

chipmunk_p <- mutate(chipmunk_p, risk = new_names[risk])

chipmunk_p <- dplyr::left_join(data_ind2, 
                               chipmunk_p, 
                               by = c("ID", "risk")) # get risk levels

chipmunk_p$risk <- factor(chipmunk_p$risk, levels = c("Low", "Medium", "High"))


# Plot it
pS3c <- chipmunk_p |> 
        mutate(ID = forcats::fct_reorder(ID, p, median)) |> # order
        ggplot(aes(y = ID, dist = (inv_logit_scaled(p)), color = risk)) + 
        stat_dist_halfeye() + 
        coord_cartesian(xlim = c(0,1)) +
        theme_bw() + 
        facet_wrap( ~ risk, scales = "free_y") + 
        xlab("p") + 
        ylab("Chipmunk ID") +
        scale_color_manual(values= c("#32936F", "#FAA613", "#F44708")) +
        theme(legend.position = "none") + 
        theme(axis.text.y = element_text(size = 10), 
              axis.text.x = element_text(size = 10)) + 
        scale_x_continuous(breaks = c(0, 0.5, 1))

# Calculate mean min and max for each risk level (refer to chipmunk_p dataset)

# Low risk 
# Min (Q088)
inv_logit_scaled(-2.233)
# Max (P056)
inv_logit_scaled(2.945)

# Medium risk
# Min (N070)
inv_logit_scaled(-2.406)
# Max (N004)
inv_logit_scaled(0.035)

# High risk
# Min (P006)
inv_logit_scaled(-2.333)
# Max (Q090)
inv_logit_scaled(0.079)


### Figure S3 - a ----
# Asymptote

chipmunk_a <- dplyr::left_join(chipmunk_m, 
                               chipmunk_p, 
                               by = c("ID", "risk")) # get risk levels

chipmunk_a <- chipmunk_a |> 
              select(ID, risk, M, p) |>
              mutate(p = inv_logit_scaled(p)) |>
              mutate(M = inv_logit_scaled(M)) |>
              mutate(a = (1000 * M * (1 - p))) # calculate a

chipmunk_a$risk <- factor(chipmunk_a$risk, levels = c("Low", "Medium", "High"))


# Plot it
pS5d <- chipmunk_a |> 
        mutate(ID = forcats::fct_reorder(ID, a, median)) |> # order
        ggplot(aes(y = ID, dist = a, color = risk)) + 
        stat_dist_halfeye() + 
        coord_cartesian(xlim = c(0,500)) +
        theme_bw() + 
        facet_wrap( ~ risk, scales = "free") + 
        xlab("Asymptote (cm)") + 
        ylab("Chipmunk ID")+
        scale_color_manual(values= c("#32936F", "#FAA613", "#F44708")) + 
        theme(legend.position = "none") + 
        theme(axis.text.y = element_text(size = 10), 
              axis.text.x = element_text(size = 10)) +
        scale_x_continuous(breaks = c(0, 250, 500)) + 
        theme(plot.margin = unit(c(5.5, 8, 5.5, 5.5), "pt"))


# Plot figure S5 a-d together
pS3 <- ggarrange(pS3a, pS3b, pS3c, pS3d)

# Save it
ggsave(filename = "FigureS3",
       plot     = pS3, 
       device   = "jpeg",
       width    = 177, 
       height   = 177, 
       units    = "mm", 
       dpi      = 300)

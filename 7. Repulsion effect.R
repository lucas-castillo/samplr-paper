# Repulsion effect
library(samplr)
library(ggplot2)
library(dplyr)
source("src/theme.R")
set.seed(2024)

trial_stim <- rep(25, 100)

# The distance between the mean value and the anchor is 2, producing repulsion effect
start_point <- trial_stim + 2
abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
                          s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)
abs_model$simulate(stopping_rule = 'fixed', n_sample = 10, trial_stim = trial_stim, start_point = start_point)

samples1 <- abs_model$sim_results %>%
  pull(samples) %>%
  lapply(function(x) x[-1]) %>%  # Remove the first element
  unlist()
samples_df1 <- data.frame(samples = samples1)


# The distance between the mean value and the anchor is 10, , producing anchor effect
start_point <- trial_stim + 10
abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
                          s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)
abs_model$simulate(stopping_rule = 'fixed', n_sample = 10, trial_stim = trial_stim, start_point = start_point)

samples2 <- abs_model$sim_results %>%
  pull(samples) %>%
  lapply(function(x) x[-1]) %>%  # Remove the first element
  unlist()

samples_df2 <- data.frame(samples = samples2)

fig_repul <- ggplot()+
  geom_histogram(data=samples_df1, mapping=aes(x = samples, y=after_stat(density)), bins=50, fill='#64CCC5',  alpha = 0.7)+
  geom_vline(aes(xintercept=27), color='#176B87', linewidth=1)+
  geom_histogram(data=samples_df2, mapping=aes(x = samples, y=after_stat(density)), bins=50, fill='#AD2959',  alpha = 0.7)+
  geom_vline(aes(xintercept=35), color='#62013C', linewidth=1)+
  stat_function(fun = dnorm, args = list(mean = 25, sd = 1), color = "#393E46", linewidth = 1, alpha = 0.5)+
  labs(x = 'Estimates', y='Density')
  
fig_repul

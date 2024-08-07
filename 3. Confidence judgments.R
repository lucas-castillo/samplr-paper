# resolution of confidence
library(samplr)
library(ggplot2)
library(dplyr)
set.seed(2024)

trial_stim <- factor(sample(c('left', 'right'), 500, TRUE))

abs_model1 <- Zhu23ABS$new(width=2, n_chains=6, nd_time=0.4,
                           s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)
abs_model1$simulate(stopping_rule = 'relative', delta = 10, dec_bdry = 0, 
                    discrim = 1, trial_stim = trial_stim)

df <- abs_model1$sim_results %>%
  group_by(accuracy) %>%
  summarise(conf = mean(confidence),
            rt = mean(rt))
View(df)
# resolution of confidence
library(samplr)
library(ggplot2)
library(dplyr)
set.seed(2024)

trial_stim <- factor(sample(c('left', 'right'), 500, TRUE))

# using the same parameters from https://osf.io/8wp5a
abs_model1 <- Zhu23ABS$new(width=0.01, n_chains=6, nd_time=0,
                           s_nd_time=0, lambda = 10, distr_name = 'norm', distr_params = 1)
abs_model1$simulate(stopping_rule = 'relative', delta = 10, dec_bdry = 0, 
                    discrim = 1, trial_stim = trial_stim)

df <- abs_model1$sim_results %>%
  group_by(accuracy) %>%
  summarise(conf = mean(confidence))
df$accuracy <- factor(df$accuracy, labels = c('Error', 'Correct'))

ggplot(df, aes(accuracy, conf)) +
  geom_line(aes(group = 1)) +
  geom_point(size = 2)

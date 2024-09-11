# over-precision
library(samplr)
library(ggplot2)
library(dplyr)
set.seed(2024)

trial_stim <- round(runif(1, 10, 50))
abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
                          s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)
abs_model$simulate(stopping_rule = 'fixed', n_sample = 5, trial_stim = trial_stim)

sub_prob <- seq(0.5, 1, by=0.1)
proportion <- matrix(0, 1, 6)

proportion <- sapply(sub_prob, function(sp){
  conf_level <- sp
  conf_interval <- t(sapply(abs_model$sim_results$samples, function(samples) quantile(samples, probs = c((1-conf_level)/2, (1+conf_level)/2))))
  lb <- conf_interval[1]
  ub <- conf_interval[2]
  return(pnorm(ub, mean=trial_stim, sd=1) - pnorm(lb, mean=trial_stim, sd=1))
})


fig <- data.frame(sub_prob=sub_prob, proportion=proportion) %>%
  ggplot(aes(x=sub_prob, y=proportion)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = 'Subjective probability', y = 'Proportion')
fig

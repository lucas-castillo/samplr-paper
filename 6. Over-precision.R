# over-precision
library(samplr)
library(ggplot2)
set.seed(2024)

trial_stim <- round(runif(1, 10, 20))
abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
                          s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 5)
abs_model$simulate(stopping_rule = 'fixed', n_sample = 5, trial_stim = trial_stim)

sub_prob <- matrix(0, 1, 3)
obj_prob <- c(0.5, 0.9, 1)

for (i in 1:3) {
  conf_level <- obj_prob[i]
  abs_model$confidence_interval(conf_level)
  lb <- abs_model$sim_results$conf_interval_l
  ub <- abs_model$sim_results$conf_interval_u
  sub_prob[i] <- pnorm(ub, mean=trial_stim, sd=1) - pnorm(lb, mean=trial_stim, sd=1)
}


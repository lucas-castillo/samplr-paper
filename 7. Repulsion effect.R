# Repulsion effect
library(samplr)
library(ggplot2)
library(dplyr)
source("src/theme.R")
set.seed(2024)

h_star <- seq(-0.5, 0.5, 0.2)
N <- 1000 # For each level of h_star, we run 500 independent trials
abs_model <- Zhu23ABS$new(width=0.1, n_chains=8, nd_time=0.4,
                          s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 0.25)


abs_estimate <- function(h_star){
  start_point <- rep(h_star, N)
  
  abs_model$reset_sim_results()
  abs_model$simulate(stopping_rule = 'relative', delta = 8, dec_bdry = h_star, 
                     discrim = 0, trial_stim = factor(rep('l', N)), start_point=start_point) # `discrim = 0` means that there is not distinction between 'left' and 'right' stimulus. Thus the trial_stim is just a place holder
  samples_repul <- abs_model$sim_results %>%
    pull(samples) %>%
    lapply(function(x) x[length(x)]) %>%  # Take the last sample as the estimate
    unlist()
  return(samples_repul - h_star) # Return the last sample as the estimate. We need to shift the last sample as we adjust the location of the boundary rather than the posterior of hypo, which was conducted in Zhu et al. (2024)
}


samples_repul <- sapply(h_star, abs_estimate)
repulsion_df <- data.frame(samples = as.vector(samples_repul))

h_star2 <- h_star - 2
samples_anchor <- sapply(h_star2, abs_estimate)
anchor_df <- data.frame(samples = as.vector(samples_anchor))


# # The distance between the mean value and the anchor is 2, producing repulsion effect
# h_star1 <- 0
# start_point <- rep(h_star1, N)
# abs_model$reset_sim_results()
# abs_model$simulate(stopping_rule = 'relative', delta = 5, dec_bdry = h_star1,
#                    discrim = 0, trial_stim = factor(rep('l', N)), start_point=start_point) # `discrim = 0` means that there is not distinction between 'left' and 'right' stimulus. `trial_stim` is just a place holder here.
# 
# samples_repul <- abs_model$sim_results %>%
#   pull(samples) %>%
#   lapply(function(x) x[length(x)]) %>%  # Take the last sample as the estimate
#   unlist()
# repulsion_df <- data.frame(samples = samples_repul)


# # The distance between the mean value and the anchor is 10, , producing anchor effect
# h_star2 <- 5
# start_point <- rep(h_star2, N)
# abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
#                           s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 0.5)
# abs_model$simulate(stopping_rule = 'relative', delta = 5, dec_bdry = h_star2, 
#                    discrim = 0, trial_stim = trial_stim, start_point=start_point)
# 
# samples_anchor <- abs_model$sim_results %>%
#   pull(samples) %>%
#   lapply(function(x) x[length(x)]) %>%  # Take the last sample as the estimate
#   unlist()
# 
# anchor_df <- data.frame(samples = samples_anchor + runif(N, -1.5, 2.5))

fig_repul <- ggplot()+
  geom_histogram(data=repulsion_df, mapping=aes(x = samples, y=after_stat(density)), bins=200, fill='#64CCC5',  alpha = 0.7)+
  geom_vline(aes(xintercept=0), color='#64CCC5', linewidth=0.7, alpha = 1)+
  geom_histogram(data=anchor_df, mapping=aes(x = samples, y=after_stat(density)), bins=200, fill='#AD2959',  alpha = 0.7)+
  geom_vline(aes(xintercept=2), color='#AD2959', linewidth=0.7, alpha = 1)+
  lapply(h_star, function(mu) {
    stat_function(fun = function(x) 1/4 * dnorm(x, mean = mu, sd = 0.25),
                  n=200, color = "#393E46", linewidth = 0.4, alpha = 0.7)
  })+
  labs(x = 'Estimates', y='Density')

ggsave("plots/repulsion_effect.png", plot = fig_repul, width = w, height = w/2, dpi = 300)

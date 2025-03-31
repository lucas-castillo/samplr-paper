# Repulsion effect
library(samplr)
library(ggplot2)
library(dplyr)
library(MASS)
source("src/theme.R")
set.seed(2024)


mu <- seq(-0.5, 0.5, 0.25)
N <- 1000 * length(mu)
trial_stim <- factor(rep('l', N)) # this is just a placeholder


custom_dens <- function(x, mu) {
  pdf <- dnorm(x, mu, sd=0.25)
  return(pdf)
}

mu_val <- rep(mu, each = 1000)

custom_dens_list <- lapply(mu_val, function(mu_val) {
  function(x) custom_dens(x, mu_val)
})


abs_model <- Zhu23ABS$new(width=0.05, n_chains=8, nd_time=0.4,
                          s_nd_time=0.3, lambda = 100,
                          custom_distr = custom_dens_list, custom_start = 0)

# Repulsion effect
hstar1 <- 0
start_point1 <- rep(hstar1, N) + rnorm(N, 0, 0.01) # add a noise 
abs_model$simulate(stopping_rule = 'relative', delta = 4, dec_bdry = hstar1, 
                   discrim = 0, trial_stim = trial_stim, start_point=start_point1, prior_depend=FALSE)
samples_repul <- unlist(abs_model$sim_results$point_est)
repulsion_df <- data.frame(samples = as.vector(samples_repul))


# Anchor effect
hstar2 <- hstar1 + 2
start_point2 <- rep(hstar2, N) + rnorm(N, 0, 0.01)
abs_model$reset_sim_results()
abs_model$simulate(stopping_rule = 'relative', delta = 4, dec_bdry = hstar2, 
                   discrim = 0, trial_stim = trial_stim, start_point=start_point2, prior_depend=FALSE)

samples_anchor <- unlist(abs_model$sim_results$point_est)
anchor_df <- data.frame(samples = as.vector(samples_anchor))

fig_repul <- ggplot()+
  geom_histogram(data=repulsion_df, mapping=aes(x = samples, y=after_stat(density)), bins=200, fill='#64CCC5',  alpha = 0.7)+
  geom_vline(aes(xintercept=0), color='#64CCC5', linewidth=0.7, alpha = 1)+
  geom_histogram(data=anchor_df, mapping=aes(x = samples, y=after_stat(density)), bins=200, fill='#AD2959',  alpha = 0.7)+
  geom_vline(aes(xintercept=2), color='#AD2959', linewidth=0.7, alpha = 1)+
  lapply(mu, function(mu) {
    stat_function(fun = function(x) 1/4 * dnorm(x, mean = mu, sd = 0.25),
                  n=200, color = "#393E46", linewidth = 0.4, alpha = 0.7)
  })+
  labs(x = 'Estimates', y='Density')
fig_repul

ggsave("plots/repulsion_effect.png", plot = fig_repul, width = w, height = w/2, dpi = 300)

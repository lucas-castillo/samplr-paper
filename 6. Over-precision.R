# over-precision
library(samplr)
library(ggplot2)
library(dplyr)
source("src/theme.R")

set.seed(2024)

df_conf <- tibble()
for (i in 1:100){
  trial_stim <- round(runif(1, 10, 50)) # inside or outside?
  abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
                            s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)
  abs_model$simulate(stopping_rule = 'fixed', n_sample = 10, trial_stim = trial_stim)
  sub_prob <- seq(0.5, 1, by=0.1)
  proportion <- matrix(0, 1, 6)
  
  proportion <- sapply(sub_prob, function(sp){
    conf_level <- sp
    conf_interval <- t(sapply(abs_model$sim_results$samples, function(samples) quantile(samples, probs = c((1-conf_level)/2, (1+conf_level)/2))))
    lb <- conf_interval[1]
    ub <- conf_interval[2]
    return(pnorm(ub, mean=trial_stim, sd=1) - pnorm(lb, mean=trial_stim, sd=1))
  })
  df_conf <- rbind(df_conf, tibble(sub_prob, proportion, i))
}

df_conf_summary <- df_conf %>%
  group_by(sub_prob) %>%
  summarise(mean_prop = mean(proportion),
            se = sd(proportion)/sqrt(n()))


fig <- df_conf %>%
  ggplot(aes(x=sub_prob, y=proportion, group = sub_prob)) +
  geom_boxplot() +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_jitter(alpha = 0.5, shape=16, position=position_jitter(0.01))+
  scale_x_continuous(limits = c(0, 1.1),
                     breaks = seq(0, 1, by=0.2),
                     labels = seq(0, 1, by=0.2)) +
  scale_y_continuous(limits = c(0, 1.1)) +
  labs(x = 'Subjective probability', y = 'Proportion')
fig
ggsave("plots/overprecision.pdf", width = w, height = w/2, dpi=300)

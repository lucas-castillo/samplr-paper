# resolution of confidence
library(samplr)
library(ggplot2)
library(dplyr)
source("src/theme.R")

set.seed(2024)

trial_stim <- factor(sample(c('left', 'right'), 500, TRUE))

# using the parameters estimated from the data of Murphy et al. (2014)
abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
                           s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)
abs_model$simulate(stopping_rule = 'relative', delta = 10, dec_bdry = -0.5, 
                    discrim = 1, trial_stim = trial_stim)

# df_sim <- subset(abs_model$sim_results, (rt > 0.1 & rt < 1.5))
df_sim <- abs_model$sim_results

df_sim <- df_sim %>%
  mutate(accuracy = ifelse(accuracy == 1, "Correct", "Error")) %>% 
  mutate(accuracy = factor(accuracy, levels=c("Error", "Correct")))
df <- df_sim %>% 
  group_by(accuracy) %>%
  summarise(confidence = mean(confidence))
fig <- ggplot(df, aes(accuracy, confidence)) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  geom_jitter(data=df_sim, aes(accuracy, confidence), alpha=.5, width = .1, height = .01) + 
  labs(
    x = "Choice Outcome", 
    y = "Confidence", 
    # title = "The resolution-of-confidence effect"
  )
fig
ggsave("plots/resolution_confidence.png", width = w, height = w/2, dpi = 300)

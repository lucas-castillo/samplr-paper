# speed-accuracy trade-off
library(samplr)
library(ggplot2)
source("src/theme.R")
set.seed(2024)

## Information-controlled processing ------
# generate the stimuli
trial_stim <- factor(sample(c('left', 'right'), 500, TRUE))

accuracy <- rep(NA, 2)
meanRT <- rep(NA, 2)
seRT <- rep(NA, 2)

# Initialise the model
# using the parameters estimated from the data of Murphy et al. (2014) (except `delta`)
abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
                          s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)
# Vary the value of `delta`
for (d in 1:2) {
  delta <- switch (d, 3, 10)
  abs_model$reset_sim_results()
  abs_model$simulate(stopping_rule = 'relative', delta = delta, dec_bdry = -0.5, 
                     discrim = 1, trial_stim = trial_stim)
  accuracy[d] <- mean(abs_model$sim_results$accuracy)
  meanRT[d] <- mean(abs_model$sim_results$rt)
  seRT[d] <- sd(abs_model$sim_results$rt)/sqrt(length(abs_model$sim_results$rt))
}

df <- data.frame(accuracy=accuracy, mean=meanRT, se=seRT)

# plot the accuracy-RT plot
df %>% 
  ggplot(aes(x = accuracy, y = mean)) + 
  geom_bar(stat="identity", width=0.05) +
  geom_errorbar(
    aes(ymin=mean-se, ymax=mean+se), 
    width=0.01,
    position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0.5, 0.9))+
  scale_x_continuous(breaks = accuracy, labels = accuracy * 100) +
  labs(x = "Accuracy (%)", y = "Response Time (s)", title = "Speed-accuracy trade-off")

ggsave("plots/speed_accuracy_tradeoff.pdf", width = 11, height = 6, dpi = 300)

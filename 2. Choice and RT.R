# speed-accuracy trade-off
library(samplr)
library(ggplot2)
library(dplyr)
source("src/theme.R")
set.seed(2024)

## Information-controlled processing ------
# generate the stimuli
trial_stim <- factor(sample(c('left', 'right'), 200, TRUE))

# Initialise the model
# using the parameters estimated from the data of Murphy et al. (2014) (except `delta`)
abs_model <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
                          s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)

abs_simulation <- function(delta, i){
  abs_model$reset_sim_results()
  abs_model$simulate(stopping_rule = 'relative', delta = delta, dec_bdry = -0.5, 
                     discrim = 1, trial_stim = trial_stim)
  accuracy <- mean(abs_model$sim_results$accuracy) * 100
  meanRT <- mean(abs_model$sim_results$rt)
  return(c(accuracy, meanRT))
}


fast_df <- sapply(1:100, abs_simulation, delta = 3) %>%
  t() %>%
  data.frame() %>%
  setNames(c('accuracy', 'RT')) %>%
  mutate(speed = 'fast')


slow_df <- sapply(1:100, abs_simulation, delta = 10) %>%
  t() %>%
  data.frame() %>%
  setNames(c('accuracy', 'RT')) %>%
  mutate(speed = 'low')

df <- bind_rows(fast_df, slow_df)
summary_df <- df %>%
  group_by(speed) %>%
  summarise(meanRT = mean(RT),
            meanAcc = mean(accuracy),
            sdRT = sd(RT)/sqrt(n()),
            sdAcc = sd(accuracy)/sqrt(n()))

# plot the accuracy-RT plot
ggplot(summary_df, mapping = aes(x = meanAcc, y = meanRT)) + 
  geom_point(data = df, mapping = aes(x = accuracy, y = RT, color = speed), size = 0.7, alpha = 0.5) +
  geom_point(size = 2, color = 'black') +
  geom_errorbar(aes(ymin = meanRT-1.96*sdRT, ymax = meanRT + 1.96*sdRT), width=0) +
  geom_errorbarh(aes(xmin = meanAcc-1.96*sdAcc, xmax = meanAcc + 1.96*sdAcc), height=0) +
  coord_cartesian(ylim=c(0.5, 0.9))+
  labs(
    x = "Accuracy (%)", 
    y = "Response Time (s)", 
    # title = "Speed-accuracy trade-off"
  )

ggsave("plots/speed_accuracy_tradeoff.png", width = w, height = w/2, dpi = 300)

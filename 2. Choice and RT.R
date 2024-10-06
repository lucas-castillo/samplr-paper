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
fig1 <- ggplot(df, aes(x = accuracy, y = mean)) + 
  geom_bar(stat="identity", width=0.05) +
  geom_errorbar( aes(ymin=mean-se, ymax=mean+se), width=0,
                 position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0.5, 0.9))+
  scale_x_continuous(breaks = accuracy, labels = accuracy) +
  labs(x = "Accuracy", y = "Response Time", title = "Speed-accuracy trade-off")
fig1

# ## Time-controlled processing ------
# # generate the stimuli
# trial_stim <- factor(sample(c('left', 'right'), 500, TRUE))
# 
# abs_model2 <- Zhu23ABS$new(width=2, n_chains=8, nd_time=0.4,
#                            s_nd_time=0.3, lambda = 100, distr_name = 'norm', distr_params = 1)
# abs_model2$simulate(stopping_rule = 'relative', delta = 10, dec_bdry = -0.5, 
#                    discrim = 1, trial_stim = trial_stim)
# df_sim <- subset(abs_model2$sim_results, (rt > 0.1 & rt < 1.5))
# 
# # bin the RT and calculate the accuracy
# df_sim$rt_bin <- cut(df_sim$rt, breaks = 10)
# binned_ac <- aggregate(accuracy ~ rt_bin, data = df_sim, FUN = mean)
# 
# fig2 <- ggplot(binned_ac, aes(x = rt_bin, y = accuracy)) +
#   geom_bar(stat = "identity") +
#   labs(x = "RT Bins", y = "Mean Accuracy", title = "Mean Accuracy by RT Bins")

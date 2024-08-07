# speed-accuracy trade-off
library(samplr)
library(ggplot2)
set.seed(2024)

## Information-controlled processing ------

# generate the stimuli
trial_stim <- factor(sample(c('left', 'right'), 100, TRUE))

accuracy <- rep(NA, 10)
meanRT <- rep(NA, 10)
sdRT <- rep(NA, 10)

# Initialise the model
abs_model <- Zhu23ABS$new(width=1, n_chains=5, nd_time=0.1,
                          s_nd_time=0.3, lambda = 50, distr_name = 'norm', distr_params = 1)
# Vary the relative stopping rule
for (d in 2:11) {
  abs_model$reset_sim_results()
  abs_model$simulate(stopping_rule = 'relative', delta = d, dec_bdry = 0, 
                     discrim = 1, trial_stim = trial_stim)
  df_temp <- subset(abs_model$sim_results, (rt > 0.1 & rt < 1.5)) # filter the RT
  accuracy[d-1] <- mean(df_temp$accuracy)
  meanRT[d-1] <- mean(df_temp$sim_results$rt)
  sdRT[d-1] <- sd(df_temp$sim_results$rt)
}

df <- data.frame(accuracy=accuracy, mean=meanRT, sd=sdRT)

# plot the accuracy-RT plot
fig <- ggplot(df, aes(accuracy, mean)) + 
  geom_errorbar( aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point(size=2)


## Time-controlled processing ------
# generate the stimuli
trial_stim <- factor(sample(c('left', 'right'), 500, TRUE))

abs_model2 <- Zhu23ABS$new(width=1, n_chains=4, nd_time=0.1,
                          s_nd_time=0.3, lambda = 50, distr_name = 'norm', distr_params = 1)
abs_model2$simulate(stopping_rule = 'relative', delta = 3, dec_bdry = 0, 
                   discrim = 1, trial_stim = trial_stim)
df_sim <- subset(abs_model2$sim_results, (rt > 0.1 & rt < 1.5))

# bin the RT and calculate the accuracy
df_sim$rt_bin <- cut(df_sim$rt, breaks = 10)
binned_ac <- aggregate(accuracy ~ rt_bin, data = df_sim, FUN = mean)

ggplot(binned_ac, aes(x = rt_bin, y = accuracy)) +
  geom_bar(stat = "identity") +
  labs(x = "RT Bins", y = "Mean Accuracy", title = "Mean Accuracy by RT Bins")

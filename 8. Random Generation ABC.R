library(glue)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(samplr)
library(samplrData)
library(abcrf)
library(patchwork)

set.seed(2024)
source("src/rg_functions.R")
source("src/rforest_posterior.R")
source("src/theme.R")
registerDoParallel(parallel::detectCores() - 1)
models <- c("MH", "MC3", "HMC", "REC", "MCHMC", "MCREC")

# Simulate sequences (or read from cache) ---------------------------------
if ("simulations.RData" %in% list.files("cache")){
  load("cache/simulations.RData")
} else {
  simulations <- tibble()
  for (model in models){
    temp <- foreach(
      i = 1:10000, 
      .combine = "rbind", .packages = c("magrittr", "dplyr")
    ) %dopar% {
      prior() %>% 
        simulate(model, params=.) %>% 
        get_measures() %>% 
        mutate(model, i)
    }
    simulations <- rbind(simulations, temp)
  }
  save(simulations, file = "cache/simulations.RData")  
}

# Remove NA values
simulations <- simulations %>% 
  drop_na() %>% 
  # and remove simulations so prior is even
  group_by(model) %>% 
  mutate(N = n()) %>% 
  ungroup %>% 
  filter(i <= min(N)) %>% 
  select(-N)

simulations <- simulations %>% 
  mutate(model = factor(model, levels=models))
observed <- samplrData::castillo2024.rgmomentum.e1 %>% 
  filter(target_dist == "N") %>% 
  group_by(id) %>% 
  summarise(across(c(R, A, TP_full, D, S), \(x){mean(x, na.rm=T)})) %>% 
  rename(TP = "TP_full") %>% 
  ungroup


# Rejection ABC -----------------------------------------------------------
tolerance <- .1
rejection_posterior <- tibble()

# get mean, sd of measures from simulations
standardizing_values <- simulations %>% 
  pivot_longer(R:S, names_to = "measure") %>% 
  group_by(measure) %>% 
  summarise(across(
    value, 
    c("M"=\(x){mean(x, na.rm=T)}, "S"=\(x){sd(x, na.rm=T)}), 
    .names = "{.fn}")
  )

# use to standardize simulations and observations
z_simulations <- simulations %>% 
  pivot_longer(R:S) %>% 
  nest_by(name) %>% 
  mutate(
    M = standardizing_values$M[standardizing_values$measure == name],
    S = standardizing_values$S[standardizing_values$measure == name]
  ) %>% 
  unnest(data) %>% 
  mutate(z = (value - M) / S) %>% 
  ungroup %>% 
  pivot_wider(names_from = name, values_from = z, id_cols = c(model, i))

z_observations <- observed %>% 
  pivot_longer(R:S) %>% 
  nest_by(name) %>% 
  mutate(
    M = standardizing_values$M[standardizing_values$measure == name],
    S = standardizing_values$S[standardizing_values$measure == name]
  ) %>% 
  unnest(data) %>% 
  mutate(z = (value - M) / S) %>% 
  ungroup %>% 
  pivot_wider(names_from = name, values_from = z, id_cols = id)

# carry out rejection ABC
for (r in 1:nrow(observed)){
  z_simulations2 <- z_simulations
  # get participant data
  data <- unlist(as.vector(z_observations[r, c("R", "A", "TP", "D", "S")]))
  
  # and calculate euclidean distance to each simulation
  euclidean_distance <- (apply(z_simulations, 1, \(r){
    sum((data - as.numeric(r[c("R", "A", "TP", "D", "S")])) ** 2)
  }))
  
  # remove simulations until we only have the closest tolerance%
  model_p <- z_simulations2 %>% 
    mutate(distance = euclidean_distance) %>% 
    mutate(threshold = quantile(distance, tolerance, na.rm=T)) %>% 
    filter(distance <= threshold) %>% 
    group_by(model) %>% 
    tally %>% 
    mutate(p = n / sum(n))
  
  # probability = proportion of models in this circle
  model_p <- bind_rows(
    model_p,
    expand_grid(
      model = models, n=0, p=0
    )
  ) %>% 
    group_by(model) %>% 
    summarise(across(n:p, sum))

  # add to tibble
  return_df <- model_p %>% 
    select(-n) %>% 
    pivot_wider(names_from = model, values_from = p) %>% 
    mutate(id = z_observations[[r,"id"]])
  
  rejection_posterior <- bind_rows(rejection_posterior, return_df)
}

rejection_posterior %>% 
  mutate(across(HMC:REC, \(x){log(x + 1e-8)})) %>% 
  summarise(across(HMC:REC, sum)) %>% 
  pivot_longer(everything()) %>% 
  arrange(desc(value)) %>% 
  mutate(BF = exp(value - nth(value, 2)))

# BF of inclusion
# Models composed of qualitative features for which support can be evaluated (see Castillo et al 2024)
rejection_posterior <- rejection_posterior %>% 
  pivot_longer(HMC:REC) %>% 
  nest_by(name) %>% 
  mutate(replicas = name %in% c("MC3", "MCHMC", "MCREC")) %>% 
  mutate(gradients = name %in% c("HMC", "MCHMC", "MCREC")) %>% 
  mutate(autoc_proposals = # matched models
           ifelse(name %in% c("HMC", "MCHMC"), F,
           ifelse(name %in% c("REC", "MCREC"), T, 
           NA))) %>% 
  unnest(data)

bf_inclusion <- function(df, column){
  df %>% 
    filter(!is.na({{column}})) %>% 
    group_by(id, {{column}}) %>%
    summarise(p = sum(value)) %>% 
    group_by({{column}}) %>% 
    summarise(L = sum(log(p))) %>% 
    mutate(BF = exp(L - min(L)))
}

rejection_posterior %>% 
  bf_inclusion(replicas)

rejection_posterior %>% 
  bf_inclusion(gradients)

rejection_posterior %>% 
  bf_inclusion(autoc_proposals)


# Random Forests ----------------------------------------------------------
# thin simulations for speed


set.seed(2024)
model <- abcrf(model ~ R + A + TP + D + S, data=simulations)
model$model.rf$prediction.error # OOB error
prediction <- predict(model, obs = observed, training = simulations)

# get posterior -----------------------------------------------------------

# get training record (or load from cache)
if ("training_record.RData" %in% list.files("cache")){
  load("cache/training_record.RData")
} else {
  training_record <- get_forest_memory(model, simulations)
  save(training_record, file = "cache/training_record.RData")  
}

# use training record to compute posterior (or load from cache!)
if ("posterior.RData" %in% list.files("cache")){
  load("cache/posterior.RData")
} else {
  posterior <- get_posterior(observed, training_record, model)
  save(posterior, file = "cache/posterior.RData")  
}
posterior <- posterior %>% 
  magrittr::set_colnames(models) %>% 
  as_tibble() %>% 
  mutate(id = factor(observed$id)) %>% 
  pivot_longer(c(everything(), -id), names_to = "model", values_to = "p") %>% 
  mutate(model = factor(model, levels = models)) %>% 
  mutate(chains = model %in% c("MC3", "MCHMC", "MCREC")) %>%
  mutate(gradient = model %in% c("REC", "MCHMC", "MCREC")) %>%
  mutate(momentum = ifelse(model %in% c("REC", "MCREC"), T, ifelse(model %in% c("HMC", "MCHMC"), F, NA)))

posterior %>% 
  pivot_wider(names_from = model, values_from = p, id_cols = id) %>% 
  summarise(across(MH:MCREC, \(x){sum(log(x + 1e-8))})) %>% 
  pivot_longer(everything()) %>% 
  arrange(desc(value)) %>% 
  mutate(BF = exp(value - nth(value, 2)))

(A <- posterior %>% 
  group_by(id) %>% 
  mutate(pmcrec = sum(ifelse(model == "MCREC", p, 0))) %>% 
  ggplot(aes(forcats::fct_reorder(id, pmcrec), p, fill=model)) + 
  geom_col(position = "stack") + 
  xlab("Participants") + 
  ylab("Posterior") + 
  theme(
    axis.text.x = element_blank(), 
    axis.ticks = element_blank()
  ) +
  labs(title="Model Allocation") +
  scale_fill_brewer(name="Model")
  + guides(fill = guide_legend(nrow = 1)) + 
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1), expand=c(0,0))
)


(B <- posterior %>% 
  group_by(id, chains) %>% 
  summarise(p = sum(p)) %>% 
  filter(chains) %>% 
  ggplot(aes(forcats::fct_reorder(id, p), p, fill=1-p)) + 
  geom_col(position = "stack") + 
  theme(
    axis.text.x = element_blank(), 
    axis.ticks = element_blank()
  ) +
  scale_fill_distiller(palette = "Greens") +
  xlab("Participants") + 
  ylab("Posterior") + 
  labs(title="Multiple Chains") +
  theme(legend.position = "none") + 
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1), expand=c(0,0)) + 
    geom_hline(yintercept = .5, linetype = "dashed")
)
(
C <- posterior %>% 
  group_by(id, gradient) %>% 
  summarise(p = sum(p)) %>% 
  filter(gradient) %>% 
  mutate(pc = sum(ifelse(gradient, p, 0))) %>% 
  ggplot(aes(forcats::fct_reorder(id, pc), p, fill=1-p)) + 
  geom_col(position = "stack") + 
  xlab("Participants") + 
  ylab("Posterior") + 
  labs(title="Gradient") +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks = element_blank()
  ) +
  scale_fill_distiller(palette = "Oranges") +
  theme(legend.position = "none") + 
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1), expand=c(0,0)) + 
    geom_hline(yintercept = .5, linetype = "dashed")
)

(D <- posterior %>% 
  group_by(id, momentum) %>% 
  filter(!is.na(momentum)) %>% 
  summarise(p = sum(p)) %>% 
  mutate(p = p / sum(p)) %>% 
  filter(momentum) %>% 
  mutate(pc = sum(ifelse(momentum, p, 0))) %>% 
  ggplot(aes(forcats::fct_reorder(id, pc), p, fill=1-p)) + 
  geom_col(position = "stack") + 
  xlab("Participants") + 
  ylab("Posterior") +
  labs(title="Momentum") +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks = element_blank()
  ) +
  scale_fill_distiller(palette = "Purples") +
  theme(legend.position = "none") + 
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1), expand=c(0,0)) + 
    geom_hline(yintercept = .5, linetype = "dashed")
)

layout <- '
AAA
BCD
'
A + B + C + D + plot_layout(design = layout)

ggsave(plot = A, "plots/RG_ABC.png", width=w, height=w/1.44, dpi=300)


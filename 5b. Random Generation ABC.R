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
    for (i in 1:1000){
      simulations <- rbind(
        simulations,
        prior() %>% 
          simulate(model, params=.) %>% 
          get_measures() %>% 
          mutate(model, i)
      )
    }
  }
  save(simulations, file = "cache/simulations.RData")  
}

simulations <- simulations %>% 
  mutate(model = factor(model, levels=models))
observed <- samplrData::castillo2024.rgmomentum.e1 %>% 
  filter(target_dist == "N") %>% 
  group_by(id) %>% 
  summarise(across(c(R, A, TP_full, D, S), \(x){mean(x, na.rm=T)})) %>% 
  rename(TP = "TP_full") %>% 
  ungroup

model <- abcrf(model ~ R + A + TP + D + S, data=simulations)
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

ggsave("plots/RG_ABC.png", width=w, height=w/1.44, dpi=300)


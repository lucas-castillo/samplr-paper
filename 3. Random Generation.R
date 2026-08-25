library(glue)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(samplr)
library(samplrData)
set.seed(2024)
source("src/rg_functions.R")
source("src/theme.R")
models <- c("MH", "MC3", "HMC", "MCREC")


empirical <- samplrData::castillo2024.rgmomentum.e1 %>% 
  mutate(value = ifelse(value < 100, NA, value)) %>% 
  mutate(value = ifelse(value > 210, NA, value))

simulations <- tibble()
for (i in 1:100){
  for (model in models){
    simulations <- rbind(
      simulations,
      get_measures(simulate(model, prior(), "norm")) %>% mutate(model=model, target="N"),
      get_measures(simulate(model, prior(), "unif")) %>% mutate(model=model, target="U")
      )
  }
}

sum_simulations <- simulations %>% 
  pivot_longer(R:S) %>% 
  mutate(name = factor(name, levels=c("R", "A", "TP", "D", "S"))) %>%
  group_by(model, target, name) %>% 
  summarise(M=mean(value), S=sd(value)) %>% 
  rename(target_dist = "target") %>% 
  mutate(target_dist = factor(target_dist)) %>% 
  mutate(target_distN = as.numeric(target_dist)) %>% 
  ungroup %>% 
  mutate(model = factor(model, level=models))

measure_names <- c(
  "R"="Repetitions",
  "A"="Adjacencies",
  "TP"="Turning Points",
  "D"="Distances",
  "S"="Shape"
)


empirical %>% 
  filter(!is.na(value)) %>% 
  group_by(id, target_dist) %>% 
  rename(TP="TP_full") %>% 
  mutate(S = sum(S,na.rm = T)) %>% 
  summarise(across(c(R, A, TP, D, S), \(x){mean(x, na.rm = T)})) %>%
  pivot_longer(R:S) %>% 
  mutate(name = factor(name, levels=c("R", "A", "TP", "D", "S"))) %>% 
  mutate(target_dist = ifelse(target_dist == "U", "Uniform", "Gaussian")) %>% 
  ungroup %>% 
  nest_by(name) %>% 
  mutate(name = factor(measure_names[name], levels=measure_names)) %>% 
  ungroup %>% 
  unnest(data) %>% 
  ggplot() + 
  geom_boxplot(aes(target_dist, value)) + 
  facet_wrap(vars(name), scales = "free", ncol = 1) +
  geom_rect(
    mapping = aes(xmin = target_distN-.45, xmax = target_distN+.45, ymin = M - .5*S, ymax=M+.5*S, fill=model),
    data = sum_simulations %>% 
      rowwise() %>% 
      mutate(name = factor(measure_names[name], levels=measure_names)),# %>% filter(name == "R"),
    linewidth=.5, color="transparent", alpha=.75, position = "dodge"
  ) + 
  coord_flip() + 
  xlab("Condition") + 
  theme(
    axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    # axis.text = element_blank(),
  ) + 
  guides(fill=guide_legend(title="Model"))
ggsave("plots/random_generation.png", width = w, height = w/1.3, dpi = 300)

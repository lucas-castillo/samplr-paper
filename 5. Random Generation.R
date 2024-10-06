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

empirical <- samplrData::castillo2024.rgmomentum.e1 %>% 
  mutate(value = ifelse(value < 100, NA, value)) %>% 
  mutate(value = ifelse(value > 210, NA, value))

simulations <- tibble()
for (i in 1:100){
  simulations <- rbind(
    simulations,
    get_measures(simulate("MCREC", prior(), "norm")) %>% mutate(target="N"),
    get_measures(simulate("MCREC", prior(), "unif")) %>% mutate(target="U")
    )
}
sum_simulations <- simulations %>% 
  pivot_longer(R:S) %>% 
  mutate(name = factor(name, levels=c("R", "A", "TP", "D", "S"))) %>%
  group_by(target, name) %>% 
  summarise(M=mean(value), S=sd(value)) %>% 
  rename(target_dist = "target") %>% 
  mutate(target_dist = factor(target_dist)) %>% 
  mutate(target_distN = as.numeric(target_dist))


measure_names <- list(
  "R"="Repetitions",
  "A"="Adjacencies",
  "TP"="Turning Points",
  "D"="Distances",
  "S"="Shape"
)
measure_labeller <- function(variable,value){
  return(measure_names[value])
}


empirical %>% 
  filter(!is.na(value)) %>% 
  group_by(id, target_dist) %>% 
  rename(TP="TP_full") %>% 
  mutate(S = sum(S,na.rm = T)) %>% 
  summarise(across(c(R, A, TP, D, S), \(x){mean(x, na.rm = T)})) %>%
  pivot_longer(R:S) %>% 
  mutate(name = factor(name, levels=c("R", "A", "TP", "D", "S"))) %>% 
  mutate(target_dist = ifelse(target_dist == "U", "Uniform", "Gaussian")) %>% 
  ggplot() + 
  geom_boxplot(aes(target_dist, value)) + 
  facet_wrap(vars(name), scales = "free", labeller=measure_labeller, ncol = 1) +
  geom_rect(
    mapping = aes(xmin = target_distN-.25, xmax = target_distN+.25, ymin = M - .5*S, ymax=M+.5*S),
    data=sum_simulations,# %>% filter(name == "R"),
    fill="grey", linewidth=.5, alpha=.5, color="black"
  ) + 
  coord_flip() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
  )

ggsave("plots/random_generation.png", width = w, height = w/1.44, dpi = 300)

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

get_simulated_random_measures <- function(start, dist, params){
  # prior
  epsilon <- .1
  L <- 30
  delta_T <- 2
  nChains <- 6
  swap_all <- T
  alpha <- .2
  
  v <- sampler_mcrec(
    start = start,
    distr_name = dist,
    distr_params = params,
    epsilon = epsilon, 
    L = L, 
    alpha = alpha, 
    nChains = nChains, 
    delta_T = delta_T, 
    swap_all = swap_all, 
    iterations = 100
  )$Samples[,,1]
  return(get_measures(v))
}

empirical <- samplrData::castillo2024.rgmomentum.e1 %>% 
  mutate(value = ifelse(value < 100, NA, value)) %>% 
  mutate(value = ifelse(value > 210, NA, value))
  
measures <- empirical %>% 
  group_by(target_dist, target_gender) %>% 
  filter(!is.na(value)) %>% 
  summarise(
    M = mean(value),
    S = sd(value),
    lb = min(value),
    ub = max(value)
  )

mm <- measures$M [measures$target_gender == "M" & measures$target_dist == "N"]
ms <- measures$S [measures$target_gender == "M" & measures$target_dist == "N"]
ml <- measures$lb[measures$target_gender == "M" & measures$target_dist == "U"]
mu <- measures$ub[measures$target_gender == "M" & measures$target_dist == "U"]
fm <- measures$M [measures$target_gender == "F" & measures$target_dist == "N"]
fs <- measures$S [measures$target_gender == "F" & measures$target_dist == "N"]
fl <- measures$lb[measures$target_gender == "F" & measures$target_dist == "U"]
fu <- measures$ub[measures$target_gender == "F" & measures$target_dist == "U"]



simulations <- tibble()
for (i in 1:100){
  simulations <- rbind(
    simulations,
    get_simulated_random_measures(
      rnorm(1, mm, ms),
      "norm",
      c(mm, ms)
    ) %>% mutate(target="N", target_gender = "M"),
    get_simulated_random_measures(
      rnorm(1, fm, fs),
      "norm",
      c(fm,fs)
    ) %>% mutate(target="N", target_gender = "F"),
    get_simulated_random_measures(
      runif(1, ml, mu),
      "unif",
      c(ml, mu)
    ) %>% mutate(target="U", target_gender = "M"),
    get_simulated_random_measures(
      runif(1, fl, fu),
      "unif",
      c(fl,fu)
    ) %>% mutate(target="U", target_gender = "F")
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

ggsave("plots/random_generation.pdf", width = 6, height = 6, dpi = 300)

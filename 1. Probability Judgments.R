library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(samplr)
set.seed(123)

### code here
a <- seq(.01, .99, .01)
b <- seq(.01, .99, .01)
not_a <- 1 - a
not_b <- 1 - b
a_and_b = a * b
b_and_not_a = b * not_a
a_and_not_b = a * not_b
not_a_and_not_b = 1 - a_and_b - b_and_not_a - a_and_not_b
get_v <- function(p, N, beta){
  (N * p * (1-p)) / ((N + 2 * beta)**2)
}

theoretical <- tibble()
simulated <- tibble()

for (beta in c(.5, 1, 2)){
  for (N in c(1, 5, 20)){
    mean_estimates <- Bayesian_Sampler(
      a_and_b = a * b,
      b_and_not_a = b * not_a,
      a_and_not_b = a * not_b,
      not_a_and_not_b = not_a_and_not_b,
      beta=beta, 
      N=N
    )$a
    sd_estimates <- sqrt(get_v(a, N, beta))
    theoretical <- rbind(
      theoretical, 
      tibble(M=mean_estimates, SD=sd_estimates, beta=beta, N=N)
    )
    simulated <- rbind(
      simulated,
      sapply(a, \(x){(rbinom(n = 4e2, size = N, prob = x) + beta) / (N + 2 * beta)}) %>% 
        as_tibble() %>% 
        summarise(across(everything(), c("M"=mean, "SD"=sd))) %>% 
        pivot_longer(everything()) %>% 
        separate(name, into = c("id", "measure"), sep = "_") %>% 
        pivot_wider(names_from = measure, values_from = value) %>% 
        mutate(beta = beta, N = N)
    )
  }
}

theoretical <- theoretical %>% mutate(across(c(beta, N), factor))
simulated <- simulated %>% mutate(across(c(beta, N), factor))

simulated %>% 
  ggplot(aes(M, SD, color=N, fill=N)) + 
  geom_point() + 
  geom_line(data=theoretical) +
  facet_grid(vars(beta))

library(glue)
library(latex2exp)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(samplr)
source("src/theme.R")
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

theoretical <- tibble()
simulated <- tibble()

for (beta in (1 / c(5, 2))){
  for (N in c(1, 5, 20)){
    mean_estimates <- Bayesian_Sampler(
      a_and_b = a * b,
      b_and_not_a = b * not_a,
      a_and_not_b = a * not_b,
      not_a_and_not_b = not_a_and_not_b,
      beta=beta, 
      N=N
    )$a
    sd_estimates <- Bayesian_Sampler(
      a_and_b = a * b,
      b_and_not_a = b * not_a,
      a_and_not_b = a * not_b,
      not_a_and_not_b = not_a_and_not_b,
      beta=beta, 
      N=N, 
      return = "variance"
    )$a %>% sqrt
    simulations <- Bayesian_Sampler(
      a_and_b = a * b,
      b_and_not_a = b * not_a,
      a_and_not_b = a * not_b,
      not_a_and_not_b = not_a_and_not_b,
      beta=beta, 
      N=N, 
      return = "simulation"
    )$a
    
    
    theoretical <- rbind(
      theoretical, 
      tibble(M=mean_estimates, SD=sd_estimates, beta=beta, N=N)
    )
    simulated <- rbind(
      simulated,
      tibble(
        M=simulations %>% 
          apply(2, mean),
        SD=simulations %>% 
          apply(2, sd),
        beta=beta,
        N=N
      )  
    )    
  }
}

theoretical <- theoretical %>% 
  mutate(across(c(beta, N), factor)) %>% 
  nest(.by=beta) %>% 
  rowwise() %>% 
  mutate(beta_label = TeX(glue("$\\beta={beta}$"), output = "character")) %>% 
  unnest(data)
simulated <- simulated %>% 
  mutate(across(c(beta, N), factor)) %>% 
  nest(.by=beta) %>% 
  rowwise() %>% 
  mutate(beta_label = TeX(glue("$\\beta={beta}$"), output = "character")) %>% 
  unnest(data)


simulated %>% 
  ggplot(aes(M, SD,color=N, fill=N,shape=N, linetype = N)) + 
  geom_point(size=1.1, color="black", alpha=.8) + 
  geom_line(data=theoretical) +
  facet_grid(cols = vars(beta_label), labeller = label_parsed) + 
  xlab("Mean") +
  ylab("Standard Deviation") +
  scale_shape_manual(values=21:23) +
  scale_linetype_manual(values=c(1,2,5)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_x_continuous(labels = \(x){x})

ggsave("plots/probability_judgments.png", width = w, height = w/2, dpi = 300)

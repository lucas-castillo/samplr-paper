library(glue)
library(latex2exp)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(samplr)
library(patchwork)
source("src/theme.R")
set.seed(123)

### parameters --
a <- seq(.01, .99, .01)
b <- seq(.01, .99, .01)
not_a <- 1 - a
not_b <- 1 - b
a_and_b = a * b
b_and_not_a = b * not_a
a_and_not_b = a * not_b
not_a_and_not_b = 1 - a_and_b - b_and_not_a - a_and_not_b
beta <- .25
Ns <- c(1,3,7, 20)
## data points -- 
theoretical <- tibble()
simulated <- tibble()

for (N in Ns){
  mean_estimates <- Bayesian_Sampler(
    a_and_b = a * b,
    b_and_not_a = b * not_a,
    a_and_not_b = a * not_b,
    not_a_and_not_b = not_a_and_not_b,
    beta=beta, 
    N=N
  )$a
  var_estimates <- Bayesian_Sampler(
    a_and_b = a * b,
    b_and_not_a = b * not_a,
    a_and_not_b = a * not_b,
    not_a_and_not_b = not_a_and_not_b,
    beta=beta, 
    N=N, 
    return = "variance"
  )$a
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
    tibble(true=a, M=mean_estimates, V=var_estimates, beta=beta, N=N)
  )
  simulated <- rbind(
    simulated,
    tibble(
      true=a,
      M=simulations %>% 
        apply(2, mean),
      V=simulations %>% 
        apply(2, var),
      beta=beta,
      N=N
    )  
  )    
}
# make PT + N
ptn <- expand_grid(true = a, N=Ns) %>% 
  mutate(d = ifelse(N==1, .17, ifelse(N==3, .07, ifelse(N==7, .03, .01)))) %>% 
  mutate(exp = (1 - 2*d) * true + d) %>% 
  mutate(var = (1/N) * (exp * (1 - exp))) %>% 
  # mutate(across(c(d, N), factor)) %>% 
  mutate(label = glue("N = {N}; d = {d}")) %>% 
  mutate(label = forcats::fct_reorder(label, N))
# finalize BS dfs
theoretical <- theoretical %>% 
  nest(.by=c(beta, N)) %>% 
  rowwise() %>%
  mutate(label = glue("N = {N}; $\\beta={beta}$")) %>% 
  mutate(label = forcats::fct_reorder(label, N)) %>% 
  unnest(data)
simulated <- simulated %>% 
  nest(.by=c(beta, N)) %>% 
  rowwise()  %>%
  mutate(label = glue("N = {N}; $\\beta={beta}$")) %>% 
  mutate(label = forcats::fct_reorder(label, N)) %>% 
  unnest(data)


## figure -- 

p1 <- ptn %>% 
  ggplot(aes(true, exp, color=label)) + 
  geom_line() + 
  xlab("P(A)") + 
  ylab("Model Prediction") + 
  theme(legend.position = c(.2, .8))

p2 <- theoretical %>% 
  ggplot(aes(true, M, color=label)) +
  geom_line() + 
  xlab("P(A)") + 
  ylab("Model Prediction") + 
  theme(legend.position = c(.2, .8))

p3 <- ptn %>% 
  ggplot(aes(exp, var, color=label, linetype=label, group=label)) +
  geom_line() +
  xlab("Mean") +
  ylab("Variance") +
  scale_shape_manual(values=21:24) +
  scale_linetype_manual(values=c(1,2,5, 6)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_color_brewer(palette="Set1") + 
  scale_x_continuous(labels = \(x){x}) + 
  theme(legend.position = c(.2, .8))


p4 <- simulated %>% 
  ggplot(aes(M, V, color= label, fill=label,shape=label, linetype = label, group=label)) + 
  geom_point(size=1.1, alpha=.8) + 
  geom_line(data=theoretical) +
  xlab("Mean") +
  ylab("Variance") +
  scale_shape_manual(values=21:24) +
  scale_linetype_manual(values=c(1,2,5, 6)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_color_brewer(palette="Set1") + 
  scale_x_continuous(labels = \(x){x}) + 
  theme(legend.position = c(.2, .8))

(p1 + p2) / (p3 + p4) + plot_layout(guides="collect")

ggsave("plots/probability_judgments.png", width = w, height = w/2, dpi = 300)

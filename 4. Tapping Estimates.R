library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(samplr)
set.seed(123)
v <- sampler_mc3(
  start = rnorm(1, .5), 
  distr_name = "norm", 
  distr_params = c(3, 5), 
  sigma_prop = 1
)$Samples[,,1]
psd <- samplr::calc_PSD(v, plot = F)
tibble(
  lf=psd$log_freq,
  lp=psd$log_psd
) %>% 
  ggplot(aes(lf, lp)) + 
  geom_line() + 
  geom_abline(intercept = psd$polyfit[2], slope = psd$polyfit[1], color="blue") + 
  geom_segment(x=-2.5, y=-1, xend=-1.5, yend=-1 - 1) + 
  geom_segment(x=-2.5, y=-1, xend=-1.5, yend=-1 - .5) + 
  geom_segment(x=-2.5, y=-1, xend=-1.5, yend=-1 - 1.5) + 
  geom_segment(x=-2.5, y=-1, xend=-1.5, yend=-1 - .75) + 
  geom_segment(x=-2.5, y=-1, xend=-1.5, yend=-1 - 1.25) + 
  annotate("text", x = -2, y = -1, label="1/f", size=8)


library(glue)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(samplr)
library(latex2exp)
source("src/theme.R")
set.seed(123)
e <- .05

v <- sampler_mc3(
  start = rnorm(1, .5), 
  distr_name = "norm", 
  distr_params = c(3, 5), 
  sigma_prop = 1, 
  iterations = 500
)$Samples[,,1]
psd <- samplr::calc_PSD(v, plot = F)
tibble(
  lf=psd$log_freq,
  lp=psd$log_psd
) %>% 
  ggplot(aes(lf, lp)) + 
  geom_line() + 
  geom_abline(intercept = psd$polyfit[2], slope = psd$polyfit[1], color="black", linetype="longdash") + 
  # 1/f inset
  geom_segment(x=-2.5, y=0, xend=-1.5, yend=- 1) + 
  geom_segment(x=-2.5, y=0, xend=-1.5, yend=- .5) + 
  geom_segment(x=-2.5, y=0, xend=-1.5, yend=- 1.5) + 
  geom_segment(x=-2.5, y=0, xend=-1.5, yend=- .75) + 
  geom_segment(x=-2.5, y=0, xend=-1.5, yend=- 1.25) + 
  annotate("rect", xmin=-2.5-e, ymin=0+e, xmax=-1.5+e, ymax=-1.5-e, fill=NA, colour="black") + 
  annotate("text", x = -2, y = -1, label="1/f", size=8) + 
  annotate("text", x = -1, y = 2.5, 
           label=glue("alpha == {round(- psd$polyfit[1], 2)}"), 
           size=6, color="black", parse=T) +
  xlab(TeX("$\\log_{10} (Frequency)$")) +
  ylab(TeX("$\\log_{10} (PSD)$"))

ggsave("plots/tapping.pdf", width = 6, height = 6, dpi = 300)

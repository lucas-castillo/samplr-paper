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

all <- tibble()
for (i in 1:50){
  v <- sampler_mc3(
    start = rnorm(1, 3, .5), 
    distr_name = "norm", 
    distr_params = c(3, 5), 
    sigma_prop = 1, 
    iterations = 1000, 
    nChains = 6
  )$Samples[,,1]
  psd <- samplr::calc_PSD(v, plot = F)
  all <- rbind(all, tibble(
    lf=psd$log_freq,
    lp=psd$log_psd,
    intercept = psd$polyfit[2], 
    slope = psd$polyfit[1],
    i
  ))
}
label <- paste0(
  "$\\bar{\\alpha} = ",
  round(- mean(all$slope), 2),
  "$"
)
label

all %>% 
  ggplot(aes(lf, lp)) + 
  geom_line(aes(group = i), alpha=.1) + 
  geom_abline(
    intercept = mean(all$intercept), 
    slope = mean(all$slope), 
    linetype="longdash", 
    linewidth=1.4,
    color=RColorBrewer::brewer.pal(3, name = "Blues")[3]
  ) + 
  annotate("text", x = -1, y = 2.5, 
           label=TeX(label, output = "character"), 
           size=6, color="black", parse=T) +
  # inset --------
  annotate("segment", x=-2.5, y=0 - 1, xend=-1.5, yend=- 1    - 1) +
  annotate("segment", x=-2.5, y=0 - 1, xend=-1.5, yend=- .5   - 1) +
  annotate("segment", x=-2.5, y=0 - 1, xend=-1.5, yend=- 1.5  - 1) +
  annotate("segment", x=-2.5, y=0 - 1, xend=-1.5, yend=- .75  - 1) +
  annotate("segment", x=-2.5, y=0 - 1, xend=-1.5, yend=- 1.25 - 1) +
  annotate("rect", xmin=-2.5-e, ymin=0-1+e, xmax=-1.5+e, ymax=-1.5-1-e, fill=NA, colour="black") +
  annotate("text", x = -2, y = -2.1, label="1/f", size=8) + 
  # inset END --------
  xlab(TeX("Log Frequency")) +
  ylab(TeX("Log Power Spectral Density")) + 
  scale_x_continuous(limits = c(min(all$lf), max(all$lf)), expand = c(0,0))


ggsave("plots/tapping.png", width = w, height = w/2, dpi = 300)

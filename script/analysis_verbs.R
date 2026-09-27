# -- head -- #

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

# -- read -- #

d = read_tsv('dat/sample_counts.tsv')

# -- viz -- #

d |> 
  ggplot(aes(p_k,suffix)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  geom_jitter(width = 0, height = .1)

glm1 = glmmTMB::glmmTMB(cbind(k,m) ~ suffix + (1|lemma), family = binomial, data = d)

# sjPlot::plot_model(glm1, 'pred')

# nice labels for the suffix classes, csak tő (stem) first
suffix_labels = c(
  stem = 'csak tő',
  zik = '-zik',
  Vdik = '-odik/edik/ödik',
  szik = '-szik',
  lik = '-lik'
)

emmeans::emmeans(glm1, 'suffix') |>
  as.data.frame() |>
  # rev() because ggplot puts the first factor level at the bottom of a discrete y axis
  mutate(suffix = factor(suffix, levels = rev(names(suffix_labels)), labels = rev(suffix_labels))) |>
  ggplot(aes(emmean, suffix)) +
  geom_pointrange(aes(xmin = asymp.LCL, xmax = asymp.UCL)) +
  labs(x = 'log odds (-k / -m)', y = NULL) +
  theme_bw()

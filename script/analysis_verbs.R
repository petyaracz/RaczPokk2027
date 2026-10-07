# -- head -- #

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

# -- read -- #

d = read_tsv('dat/sample_counts.tsv')
ref = read_tsv('dat/large_ik.tsv')
all = read_tsv('dat/verbs.tsv')

# -- combine -- #

ref = ref |> 
  mutate(
    log_1sg = log(freq/lemma_freq)
  )

d2 = ref |> 
  select(lemma, log_1sg, llfpm10, lfpm10) |> 
  inner_join(d)

# -- viz -- #

ref |> 
  ggplot(aes(log_1sg, suffix)) +
  geom_violin() +
  geom_boxplot(width = .1)

ref |> 
  ggplot(aes(llfpm10, suffix)) +
  geom_violin() +
  geom_boxplot(width = .1)

ref |> 
  ggplot(aes(lfpm10, suffix)) +
  geom_violin() +
  geom_boxplot(width = .1)

d |> 
  ggplot(aes(p_k,suffix)) +
  geom_violin() +
  geom_boxplot(width = .1)

glm1 = glmmTMB::glmmTMB(cbind(k,m) ~ suffix + (1|lemma), family = binomial, data = d)
glm2 = glmmTMB::glmmTMB(cbind(k,m) ~ suffix + log_1sg + lfpm10 + (1|lemma), family = binomial, data = d2)
glm3 = glmmTMB::glmmTMB(cbind(k,m) ~ log_1sg + lfpm10 + (1|lemma), family = binomial, data = d2)

summary(glm2)
summary(glm3)
performance::check_collinearity(glm2)
performance::check_collinearity(glm3)


sjPlot::plot_model(glm1, 'pred')
sjPlot::plot_model(glm2, 'pred')

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

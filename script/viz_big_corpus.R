# -- head -- #

setwd('~/Github/RaczPokk2027/')

library(tidyverse)
library(patchwork)

# -- read -- #

all = read_tsv('dat/verbs.tsv')

# -- plot -- #

# suffix x -0/-ik arany x tipus / token x 1sg / 3sg

p1 = all |> 
  filter(
    !is.na(suffix),
    xpostag == '[/V][Prs.NDef.3Sg]'
  ) |> 
  mutate(
    suff = paste0('-',str_remove(suffix, 'ik')),
    type = ifelse(ik_verb, '-ik','-0')
  ) |> 
  ggplot(aes(suff,fill = type)) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  coord_flip() +
  scale_fill_viridis_d() +
  ggtitle('3sg forms')

p2 = all |> 
  filter(
    !is.na(suffix),
    xpostag == '[/V][Prs.NDef.1Sg]'
  ) |> 
  mutate(
    suff = paste0('-',str_remove(suffix, 'ik')),
    type = ifelse(ik_verb, '-ik','-0')
  ) |> 
  ggplot(aes(suff,fill = type)) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  coord_flip() +
  scale_fill_viridis_d() +
  ggtitle('1sg forms')

p3 = all |> 
  filter(
    !is.na(suffix),
    xpostag == '[/V][Prs.NDef.3Sg]'
         ) |> 
  mutate(
    suff = paste0('-',str_remove(suffix, 'ik')),
    type = ifelse(ik_verb, '-ik','-0')
  ) |> 
  ggplot(aes(lfpm10, suff, colour = type)) +
  geom_boxplot() +
  theme_bw() +
  scale_colour_viridis_d()# +
  # ggtitle('3sg forms')

p4 = all |> 
  filter(
    !is.na(suffix),
    xpostag == '[/V][Prs.NDef.1Sg]'
  ) |> 
  mutate(
    suff = paste0('-',str_remove(suffix, 'ik')),
    type = ifelse(ik_verb, '-ik','-0')
  ) |> 
  ggplot(aes(lfpm10, suff, colour = type)) +
  geom_boxplot() +
  theme_bw() +
  scale_colour_viridis_d()# +
  # ggtitle('1sg forms')

p1 + p2 + p3 + p4 + plot_layout(guides = 'collect')

ggsave('viz/big_corpus.png', dpi = 'print', width = 6, height = 6)

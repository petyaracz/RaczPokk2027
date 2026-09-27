# -- head -- #

set.seed(1337)

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

# -- read -- #

d = read_csv('dat/concordance_mnsz2-ud_20260927144104.csv', skip = 4)
ref = read_tsv('dat/ik_sample.tsv')

# -- count verbs -- #

d = d |> 
  mutate(
    lemma = KWIC |> 
      str_to_lower() |> 
      str_replace_all('[oeö][km]$', 'ik')
  ) 

# inner_join(ref,d) |> 
#   count(lemma,suffix) |> 
#   count(suffix)

keep_verbs = inner_join(ref,d) |> 
  distinct(lemma,suffix) |> 
  group_by(suffix) |> 
  sample_n(25) |> 
  pull(lemma)

d2 = d |> 
  filter(lemma %in% keep_verbs)

# -- write -- #

write_tsv(d2, 'dat/concordance_mnsz2_tidy.tsv')

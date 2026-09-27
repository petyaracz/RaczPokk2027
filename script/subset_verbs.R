# -- head -- #

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

set.seed(1337)

# -- read -- #

# webcorpus2 frequency list; hunspell flags real dictionary words, screening out typos and junk
d = arrow::read_parquet('~/Github/Webcorpus2FrequencyList/frequencies.parquet') |>
  filter(hunspell)

# -- def -- #

# plural nouns, so we can drop verb/noun homographs below (e.g. horgászok is both "I fish" and "anglers")
plur = d |>
  filter(xpostag == '[/N][Pl][Nom]') |>
  pull(form)

# candidate -ik verbs: 1sg indefinite -k forms. This tag is reliable, unlike the -m side of the
# alternation, which the parser routinely confuses with 1sg definite (see readme)
sg = d |>
  filter(
    xpostag == '[/V][Prs.NDef.1Sg]',
    str_detect(lemma, 'ik$'),
    str_detect(form, 'k$')
         ) |>
  group_by(lemma) |>
  arrange(-llfpm10) |>
  slice(1) |>  # one form per lemma: the most frequent attested -k form
  ungroup()

# drop anything that's also attested as a plural noun form
sg = sg |>
  filter(!form %in% plur)

sg = sg |>
  mutate(
    freqQ = ntile(lfpm10, 10),  # frequency decile, used below to avoid the low-frequency tail. using 1sg so verbs that very rarely show up in 1sg don't get sampled in ("itten áramlok")
    suffix = case_when(
      str_detect(lemma, 'szik$') ~ 'szik',
      str_detect(lemma, '[^s]zik$') ~ 'zik',
      str_detect(lemma, '[^l]lik$') ~ 'lik',
      str_detect(lemma, '[oeö]dik$') ~ 'Vdik',
      T ~ 'stem'
    )
  )

# -- run -- #

# stratified sample: ~30 lemmas per suffix class, restricted to the top 4 frequency deciles.
# low-frequency items don't generate enough tokens for the -m form to ever get tagged correctly,
# even by chance, which is a separate problem from the one this whole workflow is solving
ik_sample = sg |>
  filter(freqQ > 6) |>
  # count(suffix)
  group_by(suffix) |>
  sample_n(29) # okay but need to grab 1sg

ik_sample |>
  pull(form)

# build one regex from the sampled -k forms, swapping the final k for a [km] class so a literal
# string search (run externally, against MNSZ2-UD) picks up both variants at once
ik_query = ik_sample |>
  pull(form) |>
  paste(collapse = '|') |>
  str_replace_all('[km]\\|', '[km]|') |>  # ...k| -> ...[km]| for every item but the last
  str_replace('[km]$', '[km]')            # ...k  -> ...[km] for the last item, which has no trailing |

ik_query = glue::glue('({ik_query})')

# -- write -- #

write_tsv(ik_sample, 'dat/ik_sample.tsv')
write_lines(ik_query, 'dat/ik_query.txt')
write_tsv(sg, 'dat/large_ik.tsv')

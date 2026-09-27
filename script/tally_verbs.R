# -- head -- #

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

# -- read -- #

# per-row definite/indefinite calls for the ambiguous -m rows (script/classify_definiteness.R)
m = read_tsv('dat/objcap_rows_classified.tsv')
# every -k/-m hit for the 125 sampled verbs, MNSZ2-UD concordance (tidied and subset to
# 25 verbs per suffix class in script/subset_results.R)
all = read_tsv('dat/concordance_mnsz2_tidy.tsv')
# sample metadata (lemma, frequency, suffix class) for the 100 sampled verbs
ref = read_tsv('dat/ik_sample.tsv')

# -- def -- #

# rows the heuristic called genuinely definite: strip these out rather than tally them as -ik
# indefinite use. Rows for lemmas that never went through the heuristic (no accusative object
# possible at all, see readme) simply don't appear here and are kept as-is by the anti_join below
drop_rows = m |>
  filter(heuristic_call == 'definite')

d = all |>
  anti_join(drop_rows)

# -- run -- #

counts = d |>
  mutate(
    form = str_to_lower(KWIC),
    ending = case_when(
      str_detect(form, 'k$') ~ 'k',
      str_detect(form, 'm$') ~ 'm'
    ),
    lemma = str_replace(form, '.[km]$', 'ik')
  ) |>
  count(lemma, ending) |>
  pivot_wider(names_from = ending, values_from = n, values_fill = 0) |>
  mutate(
    p_k = k / (k + m),
    lo_k_m = log((k + 1) / (m + 1))  # log odds of -k over -m, Laplace-smoothed
  )

metadata = ref |>
  select(lemma, lemma_freq, corpus_size, llfpm10, lemma_syl_count, suffix)

metadata |> 
  distinct(lemma) |> 
  pull()

counts |> 
  distinct(lemma)

counts2 = left_join(metadata, counts) # the NA verbs are never 1sg

counts2 = counts2 |> 
  filter(!is.na(k))

# -- write -- #

write_tsv(counts2, 'dat/sample_counts.tsv')

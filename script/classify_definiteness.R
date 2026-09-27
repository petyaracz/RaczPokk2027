# -- head -- #

# for every -m hit of a verb that can genuinely take a direct object, decide from context
# (Left/Right of the KWIC concordance) whether the use is definite or indefinite conjugation.
# lemmas that can never take an accusative object (govern dative/oblique/infinitival complements,
# or are inherently passive/anticausative) are not in object_capable below and don't need this:
# no definite reading is grammatically available, so every -m token is indefinite by construction.
# the object_capable list and the government facts behind it are hand-reviewed, not automatic
# (see dat/lemma_transitivity_decisions.tsv and readme).

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

# -- read -- #

samp = read_tsv('dat/ik_sample.tsv')
conc = read_tsv('dat/concordance_mnsz2_tidy.tsv')

# -- def -- #

# hand-reviewed: verbs among the 125 sampled that can genuinely license a direct accusative
# object (dat/lemma_transitivity_decisions.tsv). everything else has no possible definite
# reading regardless of context
object_capable = c('tojik', 'baszik', 'cselekszik', 'dudorászik', 'pakolászik', 'toszik',
                    'hazudozik', 'étkezik', 'úszik', 'bogarászik', 'kotorászik',
                    'adakozik', 'alszik', 'furikázik', 'hugyozik', 'igyekszik', 'kotlik',
                    'mozizik', 'törekszik')

pairs = samp |>
  filter(lemma %in% object_capable) |>
  transmute(lemma, k_form = form, m_form = str_replace(form, 'k$', 'm'))

conc = conc |> mutate(KWIC_lc = str_to_lower(KWIC))

# conc already carries a lemma column (built in subset_results.R from both -k/-m forms), so no
# join needed here: just isolate the -m hits of the object-capable lemmas
rows = conc |>
  filter(KWIC_lc %in% pairs$m_form)

# -- fun -- #

# same-sentence context only: text after the last sentence boundary marker, so an object from
# the previous sentence doesn't get attributed to this verb
cleanLeft = function(x) {
  x = replace_na(x, '')
  parts = str_split(x, '</s><s>|<s>|</s>')
  map_chr(parts, ~ tail(.x, 1))
}

# mirror of cleanLeft for the Right side: same-sentence text up to the next boundary marker
cleanRight = function(x) {
  x = replace_na(x, '')
  parts = str_split(x, '</s><s>|<s>|</s>')
  map_chr(parts, ~ head(.x, 1))
}

# -- run -- #

rows = rows |>
  mutate(
    left_clean = cleanLeft(Left),
    right_clean = cleanRight(Right),
    last_toks = str_extract(str_trim(left_clean), '(\\S+\\s+){0,3}\\S+$'),   # last 4 tokens before the verb
    first_toks = str_extract(str_trim(right_clean), '^\\S+(\\s+\\S+){0,3}')  # first 4 tokens after the verb
  ) |>
  mutate(
    # negation and a handful of other particles routinely intervene between object and verb
    # ("az újat nem iszom") without breaking the object-verb relationship
    particle_gap_end = '(\\s+(nem|is|már|még|csak|mindig))*\\s*$',
    particle_gap_start = '^(\\s*(nem|is|már|még|csak|mindig)\\s+)*',
    # an adjective can sit between the article and the noun ("a zöld azúrt")
    adj_gap = '(\\S+\\s+){0,2}',

    # accusative pronouns/demonstratives, checked on both sides of the verb
    flag_pronoun_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b(azt|ezt|őt|mit|kit|ugyanazt|mindezt|mindazt)\\b', particle_gap_end)), FALSE),
    flag_pronoun_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b(azt|ezt|őt|mit|kit|ugyanazt|mindezt|mindazt)\\b')), FALSE),

    # definite article + accusative-marked noun, checked on both sides of the verb
    flag_art_acc_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b(a|az)\\s+', adj_gap, '\\S*[aeiouáéíóöőúüű]t\\b', particle_gap_end)), FALSE),
    flag_art_acc_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b(a|az)\\s+', adj_gap, '\\S*[aeiouáéíóöőúüű]t\\b')), FALSE),

    # -ért ("for the sake of") ends in t and would otherwise be caught by flag_art_acc above,
    # but it's a causal-final complement, not an accusative object. postpositions like "alatt"
    # ("under") end in a double -tt, so the vowel+t requirement above already excludes them
    # (confirmed with a substitution test: swap the -m verb for its 3sg definite form and check
    # the result reads as plausible Hungarian. "a hid alatt alussza" needs an object that "alatt"
    # can't provide, so it's implausible, and it doesn't match the pattern either, good)
    flag_ert_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b(a|az)\\s+', adj_gap, '\\S*ért\\b', particle_gap_end)), FALSE),
    flag_ert_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b(a|az)\\s+', adj_gap, '\\S*ért\\b')), FALSE),

    # alszik's most common transitive idiom ("alussza az almat" / "almat alszik", sleeps one's
    # sleep/dream) is a possessive-marked object with no article at all, so the flag_art_acc
    # pattern above can never catch it. narrow, lemma-specific patch rather than a general fix
    # for unarticled possessive objects, which would need real morphological analysis
    flag_almot_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b\u00e1lm\\w*[a\u00e1]t\\b', particle_gap_end)), FALSE),
    flag_almot_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b\u00e1lm\\w*[a\u00e1]t\\b')), FALSE),

    flag_definite = (flag_pronoun_L | flag_art_acc_L | flag_pronoun_R | flag_art_acc_R | flag_almot_L | flag_almot_R),
    flag_false_positive = (flag_ert_L | flag_ert_R),
    heuristic_call = if_else(flag_definite & !flag_false_positive, 'definite', 'indefinite')
  )

# known remaining gaps, left unfixed as low-prevalence (see readme): possessive-marked definites
# with no article at all ("napját álmodom"), and cross-clause pronoun misattribution in parallel
# constructions ("amit iszom, azt eszem is")

# -- write -- #

write_tsv(rows |> select(lemma, Reference, Left, KWIC, Right, heuristic_call), 'dat/objcap_rows_classified.tsv')

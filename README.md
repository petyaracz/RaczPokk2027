# Derivational suffix and -ik verb morphology

## Problem

Morphologically disambiguated corpus data has a systematic problem for this pattern: low-frequency 1sg indefinite forms in `-m` get mis-tagged as definite. `-m` is homophonous between the two readings (it's the regular 1sg definite ending for every verb, and the special `-ik`-conjugation indefinite ending for this class), and the automatic tagger resolves the ambiguity wrong often enough, especially for rare forms, that it's visible even for verbs that can never grammatically take a definite reading at all.

Concretely: Webcorpus2 has exactly two matches for `nyílom` (the 1sg indefinite of `nyílik`, "opens", a verb with no possible transitive/definite use), and both are tagged `[/V][Prs.Def.1Sg]`. The real, levelled indefinite use is there in the corpus; the tagger just calls it definite.

What do? Use context to disambiguate instead of trusting the tag. Reading a KWIC line and judging definite vs indefinite from the actual sentence is a good task to hand to AI help: cheap to scale, and checkable against real examples rather than a black-box tag.

## Workflow

1. **`script/subset_verbs.R`** — reads Webcorpus2, keeps `-ik` verbs with a reliably-tagged 1sg indefinite `-k` form (the `-k` side is never ambiguous), screens out verb/noun homographs (e.g. `horgászok`, both "I fish" and "anglers"), and classifies each into a derivational suffix class (`lik`, `szik`, `zik`, `Vdik` = `-odik/-edik/-ödik` and `-kodik/-kedik/-ködik`, `stem` = everything else). Writes the full qualifying candidate pool to `dat/large_ik.tsv` (a population-level baseline to check the sample against), then draws a stratified sample of 29 lemmas per suffix class, restricted to the top 4 frequency deciles *of the 1sg form itself* (not lemma frequency: sorting by lemma frequency pulls in verbs that are common overall but almost never actually inflect for 1sg, heavily 3sg-biased state verbs and the like). Builds a `[km]`-alternation regex from the sampled `-k` forms. Writes `dat/ik_sample.tsv` and `dat/ik_query.txt`.
2. **MNSZ2-UD concordance search** (external, not scripted here) — the regex is run as a literal string search against MNSZ2-UD, a different, independently tagged corpus, returning real sentence context (Left/KWIC/Right) for every hit rather than a parser tag.
3. **`script/subset_results.R`** — joins the concordance hits back to the sample, derives a `lemma` column from the KWIC form directly, and subsets down to 25 lemmas per suffix class (125 total) since not every sampled lemma is guaranteed a hit. Writes `dat/concordance_mnsz2_tidy.tsv`.
4. **`dat/lemma_transitivity_decisions.tsv`** — hand review of all 125 sampled lemmas: does the verb ever grammatically license a direct accusative object at all? Most govern datives, obliques, or infinitival complements and never do, so their `-m` hits are indefinite by construction regardless of context, no further checking needed. Only the lemmas that can genuinely take an object (19 of the 125) create real ambiguity that needs step 5.
5. **`script/classify_definiteness.R`** — for those 19 object-capable lemmas, classifies each `-m` hit as definite or indefinite from its concordance context: accusative pronouns (`azt`, `ezt`, `őt`, `mit`, `kit`, `ugyanazt`, `mindezt`, `mindazt`) or a definite article followed by an accusative-marked noun, checked on both sides of the verb (Hungarian word order allows the object before or after it), tolerating an intervening adjective or a small set of particles (`nem`, `is`, `már`, `még`, `csak`, `mindig`). Excludes `-ért` ("for the sake of", ends in `t` but isn't an accusative object). Includes one lemma-specific rule for `alszik`'s `álmát`/`álmomat` idiom (a possessive object with no article, which the general pattern can't catch). Writes `dat/objcap_rows_classified.tsv`.
6. **`script/tally_verbs.R`** — strips the rows called definite, tallies `-k` vs `-m` per lemma, joins frequency and suffix-class metadata, computes `p_k` and the `-k`/`-m` log-odds. Writes `dat/sample_counts.tsv`, the final 125-verb result.

## Results

`script/analysis_verbs.R` fits `glm1`, a mixed-effects logistic regression on `dat/sample_counts.tsv`: `glmmTMB(cbind(k,m) ~ suffix + (1|lemma), family = binomial)`. The outcome is each verb's `-k` vs `-m` count; suffix class is the fixed effect of interest, and a per-lemma random intercept absorbs the fact that individual verbs vary in their own right, so the suffix comparison isn't driven by a handful of extreme items. `emmeans::emmeans(glm1, 'suffix')` then gives the estimated marginal log odds of `-k` for each suffix class, holding that by-lemma variation constant, plotted with 95% Wald confidence intervals.

`-lik` sits well apart from the other four classes (log odds ≈ 2.0, 95% CI [1.2, 2.7]), the only class whose interval sits clearly on the `-k` side of zero: `-lik` verbs level to `-k` far more than any other class. The remaining four (`csak tő`, `-zik`, `-odik/edik/ödik`, `-szik`) all sit around zero to mildly negative with overlapping intervals, so the data support one real contrast, `-lik` vs everything else, not five distinguishable classes.

## Directories

- `script/` — the R scripts above.
- `dat/` — sampled verbs, concordance data, the population-level baseline, intermediate and final tallies.
- `old/` — superseded work, not part of the current method.

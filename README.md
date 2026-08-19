# Peer Dating Discourse and Romantic Self-Perception

Analysis code for an MSc Behavioural Science dissertation (LSE, 2026).

**Research question:** How does peer-to-peer dating discourse among same-sex friends shape
individuals' romantic self-perception?

---

## Study design

A between-subjects online vignette experiment. Participants (women aged 18–35, recruited by
snowball sampling) completed baseline measures, read one of three vignettes depicting a
conversation between close female friends, and then completed post-exposure measures.

| Condition | Vignette content |
|---|---|
| Competitive | Friends discuss dating in evaluative, standards-elevating terms |
| Supportive | Friends discuss dating in affirming, non-competitive terms |
| Neutral | Friends discuss a non-dating topic, matched for length and warmth |

Final analysed sample: **N = 146** (competitive 51, neutral 60, supportive 35).

Vignette language was derived from naturally occurring dating discourse on public forums.

## Hypotheses

| | Prediction | Outcome |
|---|---|---|
| H1 | Competitive discourse raises state dating anxiety | Supported |
| H2 | Trait intrasexual competitiveness moderates the effect of discourse on romantic self-perception | Not supported |
| H3 | Discourse condition shifts romantic self-perception, with a directional asymmetry favouring competitive framing | Not supported; asymmetry test inconclusive |
| H4 | Perceived peer selectivity mediates the effect on self-perception (exploratory) | Not supported |

---

## Repository structure

```
├── scripts/
│   ├── 01_import_and_verify.R   Import, exclusions, scale construction
│   ├── 02_analysis.R            Models, hypothesis tests, sensitivity analyses
│   └── 03_figures_apa.R         Figures
├── data/
│   ├── raw/                     NOT TRACKED — see Data availability
│   └── derived/                 NOT TRACKED — regenerate by running the scripts
└── output/
    ├── tables/                  Analysis output as CSV
    └── figures/                 Figures as PNG
```

Run the scripts in numerical order. `02_analysis.R` expects `data/derived/dat_scales.rds`,
which `01_import_and_verify.R` produces.

## Analytic approach

Hypotheses were tested using **linear regression models** — one per outcome — with condition
dummy-coded (neutral as reference), continuous covariates mean-centred, and a condition × ICS
interaction term. Type III tests were obtained via estimated marginal means.

This was deliberate rather than incidental: an ANCOVA assumes homogeneity of regression slopes,
which is precisely what H2 tests. Estimating the interaction rather than assuming it away is
what makes both hypotheses testable within one model.

Other analytic decisions:

- Bonferroni correction across three pairwise contrasts
- Bias-corrected and accelerated (BCa) bootstrap intervals, 5,000 resamples, seed `20260814`
- Mediation estimated in `lavaan` (the pre-specification named PROCESS Model 4; the estimand
  is identical and the substitution is recorded as an amendment)
- Intention-to-treat as primary: manipulation-check failures, speeders and straight-liners were
  retained in the main analysis and examined in three pre-committed sensitivity subsamples

## A note on `02_analysis.R`

The modelling for this study was originally run interactively on 14 August 2026, and the derived
model variables were not saved with that session. `02_analysis.R` reconstructs it.

The reconstruction is verified rather than assumed: the script contains assertion gates that halt
execution unless the collapsed relationship-status counts, the two primary omnibus *F* values, and
the manipulation-check pass count all reproduce the values recorded at the time. Expected values
for every downstream quantity are given as inline comments.

## Data availability

Raw and derived data are **not** included in this repository. The dataset consists of individual
survey responses collected under ethical approval that does not cover public release.

Aggregate output — the tables and figures reported in the dissertation — is in `output/`.

## Requirements

R (≥ 4.0) with: `dplyr`, `emmeans`, `effectsize`, `car`, `boot`, `lavaan`, `psych`.

```r
install.packages(c("dplyr", "emmeans", "effectsize", "car", "boot", "lavaan", "psych"))
```

## Author

Nour Lejmi — MSc Behavioural Science, London School of Economics and Political Science, 2026.

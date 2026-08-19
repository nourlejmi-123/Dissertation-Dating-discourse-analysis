# =============================================================================
# 02_analysis.R
#
# Peer Dating Discourse and Romantic Self-Perception
# MSc Behavioural Science dissertation — Nour Lejmi, LSE
#
# Reconstructs the analysis originally run interactively on 14 August 2026.
# Derived model variables (mean-centred covariates, collapsed relationship
# status) were not saved with the original session; this script rebuilds them
# and verifies the resulting models against the recorded values in the results
# log before any downstream analysis is run.
#
# Input:  data/derived/dat_scales.rds   (composites, built by 01_import_and_verify.R)
# Output: data/derived/dat_model.rds    (adds derived model variables)
#         output/tables/*.csv           (as listed at the foot of this file)
#
# Random seed: 20260814 (all bootstrap procedures)
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------

library(dplyr)
library(emmeans)
library(effectsize)
library(car)
library(boot)
library(lavaan)
library(psych)

SEED <- 20260814
set.seed(SEED)

dat <- readRDS("data/derived/dat_scales.rds")

stopifnot(nrow(dat) == 146)


# ---- 1. Derived model variables ---------------------------------------------
# VERIFIED: reproduces the reported omnibus tests exactly (see section 2).

# Relationship status. Q16 holds relationship status in the values export:
#   1 = single, not dating        2 = single, casually dating
#   3 = committed relationship    4 = engaged or married
#   5 = other
# Engaged/married (n = 5) and other (n = 6) are too sparse to estimate as
# separate levels. Collapsed to three: 1-2 = single, 3-4 = partnered,
# 5 = other. Single is the reference category. Nuisance covariate.
#
# NOTE: Q17 holds ethnicity, not relationship status. The two are adjacent in
# the export and easily transposed.

dat$relstat3 <- factor(
  case_when(
    dat$Q16 %in% c(1, 2) ~ "single",
    dat$Q16 %in% c(3, 4) ~ "partnered",
    dat$Q16 == 5         ~ "other"
  ),
  levels = c("single", "partnered", "other")
)

# Condition, neutral as reference
dat$condition <- relevel(factor(dat$condition), ref = "neutral")

# Mean-centred continuous covariates and moderator
dat$ics_c     <- dat$ics     - mean(dat$ics,     na.rm = TRUE)
dat$anx_pre_c <- dat$anx_pre - mean(dat$anx_pre, na.rm = TRUE)
dat$mvs_pre_c <- dat$mvs_pre - mean(dat$mvs_pre, na.rm = TRUE)

# Integrity checks
stopifnot(
  identical(as.vector(table(dat$relstat3)), c(87L, 53L, 6L)),
  levels(dat$condition)[1] == "neutral",
  all(abs(sapply(dat[c("ics_c", "anx_pre_c", "mvs_pre_c")], mean, na.rm = TRUE)) < 1e-10)
)

saveRDS(dat, "data/derived/dat_model.rds")


# ---- 2. Primary models ------------------------------------------------------
# Two linear regression models, one per outcome. Condition dummy-coded with
# neutral as reference; continuous covariates mean-centred; condition x ICS
# interaction estimated rather than assumed absent.
#
# Rationale for regression over ANCOVA: an ANCOVA assumes homogeneity of
# regression slopes, which is precisely what H2 tests. Testing H2 and
# reporting a valid ANCOVA would be mutually incoherent.
#
# H1 outcome: post-exposure state dating anxiety
# H3 outcome: romantic self-perception (Scale F, post-exposure)
# H2:         the condition x ICS term in m3

m1 <- lm(anx_post  ~ condition * ics_c + relstat3 + anx_pre_c, data = dat)
m3 <- lm(self_perc ~ condition * ics_c + relstat3 + mvs_pre_c, data = dat)

# Type III tests via estimated marginal means (invariant to contrast coding).
# car::Anova(type = 3) with default treatment contrasts produces an incorrect
# effect size for the ICS term when an interaction is present — it estimates
# the effect at the neutral condition rather than averaged across conditions.
# joint_tests is the defensible version and is what is reported.

jt_m1 <- joint_tests(m1)
jt_m3 <- joint_tests(m3)

print(jt_m1)
print(jt_m3)

# --- VERIFICATION GATE -------------------------------------------------------
# These are the recorded values from the 14 August 2026 session. If either
# fails, the reconstruction differs from the reported analysis and nothing
# downstream should be trusted.

stopifnot(
  abs(jt_m1$F.ratio[jt_m1$`model term` == "condition"] - 7.81) < 0.01,
  abs(jt_m3$F.ratio[jt_m3$`model term` == "condition"] - 1.52) < 0.01,
  jt_m1$df2[1] == 137,
  jt_m3$df2[1] == 137
)
message("Verification gate passed: primary models reproduce recorded values.")
# -----------------------------------------------------------------------------

# Partial eta-squared with two-sided CIs, computed from the joint_tests F values
eta_m1 <- F_to_eta2(jt_m1$F.ratio, jt_m1$df1, jt_m1$df2, alternative = "two.sided")
eta_m3 <- F_to_eta2(jt_m3$F.ratio, jt_m3$df1, jt_m3$df2, alternative = "two.sided")

# Expected (m1): condition .102 [.022, .200]; ICS .034; relstat .043;
#                anx_pre .420 [.300, .522]; condition x ICS .053
# Expected (m3): condition .022 [.000, .083]; ICS .064 [.008, .157];
#                relstat .207 [.094, .317]; mvs_pre .232 [.120, .346];
#                condition x ICS .006 [.000, .044]


# ---- 3. Adjusted means and pairwise contrasts -------------------------------
# Because both models contain an interaction, these are estimates at mean ICS,
# averaged over relationship status. State this rather than presenting them as
# unconditional means.

emm_m1 <- emmeans(m1, ~ condition)
emm_m3 <- emmeans(m3, ~ condition)

pw_m1 <- pairs(emm_m1, adjust = "bonferroni")
pw_m3 <- pairs(emm_m3, adjust = "bonferroni")

print(summary(emm_m1, infer = TRUE))   # Expected: neutral 3.11, comp 3.84, supp 3.39
print(pw_m1)                           # Expected: neutral - competitive = -0.728, p < .001
print(summary(emm_m3, infer = TRUE))   # Expected: neutral 4.24, comp 3.99, supp 4.38
print(pw_m3)                           # Expected: no contrast significant

# Simple slopes of ICS by condition (H1 exploratory interaction; H2 null)
ss_m1 <- emtrends(m1, ~ condition, var = "ics_c")
ss_m3 <- emtrends(m3, ~ condition, var = "ics_c")

print(summary(ss_m1, infer = TRUE))    # Expected: supportive 0.600 [0.30, 0.90];
                                       # neutral 0.050; competitive 0.034
print(summary(ss_m3, infer = TRUE))


# ---- 4. Loss-asymmetry contrast (H3 directional prediction) -----------------
# Prospect-theory-derived prediction: competitive discourse moves self-perception
# further from the neutral reference point than supportive discourse does.
#
# Sign gate (on ADJUSTED means): competitive < neutral < supportive.
# Contrast weights: neutral +2, competitive -1, supportive -1.
# Level order is c("neutral", "competitive", "supportive") — confirm before
# applying weights.

print(levels(emm_m3@levels$condition))

asym <- contrast(emm_m3, list(asymmetry = c(2, -1, -1)))
print(summary(asym, infer = TRUE))
# Expected: estimate 0.119, SE 0.367, t(137) = 0.32, p = .747, CI [-0.61, 0.85]


# ---- 5. Bootstrap robustness check on the asymmetry -------------------------
# Direct test of |d_comp| - |d_supp|, assumption-free about the sign ordering.
# Stratified by condition (preserving 51/60/35 in each resample), 5,000
# replicates, BCa intervals.
#
# Computed from model coefficients rather than via emmeans inside the loop:
# because relationship status does not interact with condition and ICS is
# mean-centred, the coefficients ARE the adjusted mean differences at mean ICS.
# Identical result, avoids refitting emmeans 5,000 times.
#
# t0 should equal the parametric contrast estimate (0.1188) exactly — this
# confirms the coefficient-based implementation computes the intended quantity.
#
# NOTE ON BIAS: the bootstrap bias estimate (-0.069) is 58% of the point
# estimate. Expected behaviour — |d_comp| - |d_supp| involves absolute values,
# non-smooth at zero, so resampling systematically distorts the statistic when
# the true difference is near zero. BCa partially corrects for this, which is
# why BCa rather than percentile intervals were used.

asym_stat <- function(d, i) {
  fit <- lm(self_perc ~ condition * ics_c + relstat3 + mvs_pre_c, data = d[i, ])
  cf  <- coef(fit)
  d_comp <- cf["conditioncompetitive"]
  d_supp <- cf["conditionsupportive"]
  unname(abs(d_comp) - abs(d_supp))
}

set.seed(SEED)
boot_asym <- boot(
  data      = dat,
  statistic = asym_stat,
  R         = 5000,
  strata    = dat$condition
)

print(boot_asym)
print(boot.ci(boot_asym, type = "bca"))
# Expected: t0 = 0.1188, bias = -0.0692, SE = 0.2398, BCa CI [-0.32, 0.58]
#
# The bootstrap CI is NARROWER than the parametric contrast CI [-0.61, 0.85].
# These are related but NOT identical quantities: the contrast tests
# 2*M_neutral - M_comp - M_supp; the bootstrap tests the difference of absolute
# deviations. They coincide numerically here because the observed signs fell as
# predicted. Report both, note their agreement, do not present as the same test.


# ---- 6. H4 — Exploratory mediation via perceived peer selectivity -----------
# Pre-spec specified PROCESS Model 4. Implemented in lavaan instead: identical
# estimand (single-mediator model, bootstrapped indirect effects, bias-corrected
# intervals), R-native. Recorded as an amendment. Do NOT claim PROCESS was used.
#
# SCOPE CONDITION: Scale G items use within-item comparative wording ("more
# selective than I am"), so the composite captures PERCEIVED DISCREPANCY, not a
# self-other difference score. This establishes perceived discrepancy, NOT
# demonstrated misperception. Must not be written up as pluralistic ignorance
# in the strict sense.
#
# DO NOT REPORT FIT STATISTICS for this model. A PROCESS Model 4 equivalent
# should be saturated; the single df arises from lavaan not estimating one
# covariance among exogenous predictors. Fit indices are not meaningful here.

dat$comp       <- as.numeric(dat$condition == "competitive")
dat$supp       <- as.numeric(dat$condition == "supportive")
dat$relstat_p  <- as.numeric(dat$relstat3  == "partnered")
dat$relstat_o  <- as.numeric(dat$relstat3  == "other")

med_model <- '
  peer_sel  ~ a1*comp + a2*supp + ics_c + relstat_p + relstat_o
  self_perc ~ b*peer_sel + c1*comp + c2*supp + ics_c + mvs_pre_c + relstat_p + relstat_o

  ind_comp := a1*b
  ind_supp := a2*b
'

set.seed(SEED)
fit_med <- sem(
  med_model,
  data      = dat,
  se        = "bootstrap",
  bootstrap = 5000
)

print(parameterEstimates(fit_med, boot.ci.type = "bca.simple", standardized = FALSE))
# Expected a paths: a1 = 0.403 (p = .090); a2 = 0.301 (p = .242)
# Expected b path:  b  = -0.110 (p = .202)
# Expected indirect: comp -0.044 [-0.187, 0.007]; supp -0.033 [-0.182, 0.014]
# Expected: mvs_pre_c 0.479 (p < .001); relstat_p 1.112 (p < .001)


# ---- 7. Sensitivity analyses ------------------------------------------------
# Three pre-committed subsamples, each refitting both primary models.
#
# Speeders and ICS straight-liners were moved from EXCLUSION to SENSITIVITY
# (amendment 4, 14 Aug 2026): excluding on a post-randomisation variable
# carries the same risk as excluding on manipulation-check failure. All ten
# straight-line cases sat at the scale minimum on a uniformly negatively
# valenced instrument, so excluding them would systematically remove the low
# end of the H2 moderator distribution.
#
# Manipulation check: failure was NOT an exclusion criterion. Primary analyses
# are intention-to-treat on all 146.

# QID42 is the manipulation check item:
#   1 = competitive/critical  2 = supportive/encouraging  3 = neutral/unrelated
dat$mc_pass <- with(dat, (
  (condition == "competitive" & QID42 == 1) |
  (condition == "supportive"  & QID42 == 2) |
  (condition == "neutral"     & QID42 == 3)
))

stopifnot(sum(dat$mc_pass, na.rm = TRUE) == 108)   # 74.0% overall pass rate

subsamples <- list(
  primary          = dat,
  no_speeders      = dat[!dat$flag_speeder, ],        # n = 138
  no_straightliners = dat[!dat$flag_straightline, ],  # n = 136
  mc_passers       = dat[dat$mc_pass, ]               # n = 108
)

sensitivity <- lapply(names(subsamples), function(nm) {
  d <- subsamples[[nm]]

  f1 <- lm(anx_post  ~ condition * ics_c + relstat3 + anx_pre_c, data = d)
  f3 <- lm(self_perc ~ condition * ics_c + relstat3 + mvs_pre_c, data = d)

  j1 <- joint_tests(f1)
  j3 <- joint_tests(f3)

  cond_contrast <- as.data.frame(
    pairs(emmeans(f1, ~ condition), adjust = "bonferroni")
  )
  cn <- cond_contrast[cond_contrast$contrast == "neutral - competitive", ]

  data.frame(
    sample              = nm,
    n                   = nrow(d),
    h1_F                = j1$F.ratio[j1$`model term` == "condition"],
    h1_p                = j1$p.value[j1$`model term` == "condition"],
    h1_comp_neutral_est = cn$estimate,
    h1_comp_neutral_p   = cn$p.value,
    h3_F                = j3$F.ratio[j3$`model term` == "condition"],
    h3_p                = j3$p.value[j3$`model term` == "condition"],
    h2_F                = j3$F.ratio[j3$`model term` == "condition:ics_c"],
    h2_p                = j3$p.value[j3$`model term` == "condition:ics_c"],
    expl_int_F          = j1$F.ratio[j1$`model term` == "condition:ics_c"],
    expl_int_p          = j1$p.value[j1$`model term` == "condition:ics_c"]
  )
})

sensitivity <- do.call(rbind, sensitivity)
print(sensitivity)

# Expected pattern:
#   H1 condition:   ROBUST — F = 7.81 / 8.83 / 7.35 / 8.78, all p <= .001
#   H1 contrast:    -0.728 / -0.768 / -0.748 / -0.940
#                   LARGEST in the MC-pass subsample. The ITT estimate is
#                   diluted by participants who did not perceive the
#                   manipulation. Report as strengthening confidence.
#   H3 condition:   STABLY NULL — p = .222 / .259 / .340 / .493
#   H2 interaction: NULL throughout — p = .668 / .641 / .816 / .929
#   Exploratory interaction on anxiety: FRAGILE —
#                   p = .024 / .247 / .042 / .659. Holds in two specifications,
#                   vanishes in two including MC-passers. NOT a finding.
#
# Note: the competitive-supportive contrast reaches significance in the
# no-speeders sample (p = .025) but not in the primary analysis (p = .106).
# Do not upgrade the claim on this basis; the primary analysis governs.


# ---- 8. Assumption checks ---------------------------------------------------
# Homogeneity of regression slopes is NOT an assumption of these models — the
# condition x ICS interaction is estimated rather than assumed away. Do not
# list it.

shapiro.test(residuals(m1))   # Expected: W = .987, p = .178
shapiro.test(residuals(m3))   # Expected: W = .991, p = .444

leveneTest(anx_post  ~ condition, data = dat, center = median)  # F(2,143) = 0.55, p = .577
leveneTest(self_perc ~ condition, data = dat, center = median)  # F(2,143) = 0.18, p = .837

# Both met, so the contingency substitutions specified in advance (Welch's
# ANOVA, bootstrapped CIs) were not required.


# ---- 9. Scale reliability ---------------------------------------------------
# Reported in 01_import_and_verify.R where composites are built; repeated here
# for completeness of the analysis record.
#
# Expected: MVS a = .87; baseline anxiety a = .83; ICS a = .88;
#           state anxiety a = .79; Scale G a = .65
#           Scale F: two items, r = .55 (Spearman-Brown = .71) — alpha is not
#           appropriate for a two-item scale.
#
# Scale G item retention: pre-specified rule was to drop item 3 if its
# corrected item-total correlation fell below .30. Obtained r.drop = .34, so
# retained. Item 2 was the weakest performer (r.drop = .28), below the
# threshold written for item 3, but no pre-specified rule licensed its removal
# and dropping it after inspection would be post hoc. All four retained.

alpha_G <- psych::alpha(dat[, c("Scale G_1", "Scale G_2", "Scale G_3", "Scale G_4")])
print(alpha_G$item.stats)   # Expected r.drop: .56 / .28 / .34 / .57

print(cor(dat$Q38, dat$Q39, use = "complete.obs"))   # Expected .55


# ---- 10. Session info -------------------------------------------------------
# Captured for the reproducibility appendix. Package versions here are the
# citation source for the bibliography — do not cite versions from memory.

sessionInfo()

citation("emmeans")
citation("lavaan")
citation("boot")

# =============================================================================
# End of 02_analysis.R
# =============================================================================

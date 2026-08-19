t6$Pass_rate_pct <- round(100 * c(
mc["neutral","3"], mc["competitive","1"], mc["supportive","2"]
)[match(rownames(t6), c("neutral","competitive","supportive"))] / rowSums(mc), 1)
t6 <- t6[, c("Condition", "Selected competitive", "Selected supportive",
"Selected neutral", "Pass_rate_pct")]
sv(t6, "table6_manipulation_check")
# ---- Helper: omnibus table with partial eta squared ---------------------------
omni <- function(model, label) {
jt <- as.data.frame(joint_tests(model))
es <- as.data.frame(eta_squared(Anova(model, type = 3), partial = TRUE,
alternative = "two.sided"))
jt$eta2p <- es$Eta2_partial[match(jt$`model term`, es$Parameter)]
jt$CI_low <- es$CI_low[match(jt$`model term`, es$Parameter)]
jt$CI_high <- es$CI_high[match(jt$`model term`, es$Parameter)]
jt$Model <- label
jt
}
sv(omni(m1, "H1 anxiety"), "table7_H1_omnibus")
sv(omni(m3, "H3 self-perception"), "table9_H3_omnibus")
# ---- TABLE 8 / 10. Adjusted means and contrasts -------------------------------
adj <- function(model, name) {
em <- as.data.frame(emmeans(model, ~ condition))
pr <- as.data.frame(pairs(emmeans(model, ~ condition), adjust = "bonferroni"))
sv(em, paste0(name, "_adjusted_means"))
sv(pr, paste0(name, "_contrasts"))
invisible(em)
}
em1 <- adj(m1, "table8_H1")
em3 <- adj(m3, "table10_H3")
sv(as.data.frame(emtrends(m1, ~ condition, var = "ics_c")), "table8b_H1_simple_slopes")
# ---- TABLE 11. Asymmetry contrast ---------------------------------------------
asym <- as.data.frame(confint(contrast(emmeans(m3, ~ condition),
list(asymmetry = c(2, -1, -1)))))
asym_p <- as.data.frame(contrast(emmeans(m3, ~ condition),
list(asymmetry = c(2, -1, -1))))
asym$p.value <- asym_p$p.value
asym$boot_t0 <- b_asym$t0
asym$boot_bias <- mean(b_asym$t) - b_asym$t0
asym$boot_SE <- sd(b_asym$t)
bci <- boot::boot.ci(b_asym, type = "bca")
asym$boot_CI_low <- bci$bca[4]
asym$boot_CI_high <- bci$bca[5]
sv(asym, "table11_asymmetry_contrast")
# ---- TABLE 12. Mediation ------------------------------------------------------
pe <- lavaan::parameterEstimates(fit_med, boot.ci.type = "bca.simple")
t12 <- pe[pe$op %in% c("~", ":="), c("lhs","op","rhs","label","est","se","z","pvalue","ci.lower","ci.upper")]
sv(t12, "table12_mediation_paths")
# ---- TABLE 13. Sensitivity analyses -------------------------------------------
sens_row <- function(model, n, label, outcome) {
jt <- as.data.frame(joint_tests(model))
pr <- as.data.frame(pairs(emmeans(model, ~ condition), adjust = "bonferroni"))
cn <- pr[pr$contrast == "neutral - competitive", ]
data.frame(Outcome = outcome, Sample = label, N = n,
F_condition = round(jt$F.ratio[jt$`model term` == "condition"], 2),
p_condition = round(jt$p.value[jt$`model term` == "condition"], 4),
F_interaction = round(jt$F.ratio[jt$`model term` == "condition:ics_c"], 2),
p_interaction = round(jt$p.value[jt$`model term` == "condition:ics_c"], 4),
comp_vs_neut = round(cn$estimate, 3),
p_comp_vs_neut = round(cn$p.value, 4))
}
t13 <- bind_rows(
sens_row(m1,  146, "Primary",             "State anxiety"),
sens_row(s1a, 138, "Excl. speeders",      "State anxiety"),
sens_row(s1b, 136, "Excl. straight-liners","State anxiety"),
sens_row(s1c, 108, "MC passers only",     "State anxiety"),
sens_row(m3,  146, "Primary",             "Self-perception"),
sens_row(s3a, 138, "Excl. speeders",      "Self-perception"),
sens_row(s3b, 136, "Excl. straight-liners","Self-perception"),
sens_row(s3c, 108, "MC passers only",     "Self-perception")
)
sv(t13, "table13_sensitivity_analyses")
# ==============================================================================
# FIGURES
# ==============================================================================
apa <- theme_classic(base_size = 12) +
theme(axis.title = element_text(colour = "black"),
axis.text = element_text(colour = "black"),
strip.background = element_blank(),
strip.text = element_text(face = "bold", size = 12),
legend.position = "none",
plot.caption = element_text(hjust = 0, size = 9))
cond_lab <- c(neutral = "Neutral", competitive = "Competitive", supportive = "Supportive")
# ---- FIGURE 1. Condition means, both outcomes ---------------------------------
f1dat <- bind_rows(
as.data.frame(em1) |> mutate(Outcome = "State dating anxiety"),
as.data.frame(em3) |> mutate(Outcome = "Romantic self-perception")
) |>
mutate(condition = factor(cond_lab[as.character(condition)],
levels = c("Neutral", "Competitive", "Supportive")),
Outcome = factor(Outcome, levels = c("State dating anxiety",
"Romantic self-perception")))
fig1 <- ggplot(f1dat, aes(condition, emmean)) +
geom_col(fill = "grey75", colour = "black", width = .65) +
geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .12) +
geom_text(aes(label = sprintf("%.2f", emmean), y = upper.CL + .22), size = 3.6) +
facet_wrap(~ Outcome) +
scale_y_continuous(limits = c(0, 6), breaks = 1:6, expand = c(0, 0)) +
labs(x = NULL, y = "Adjusted mean (1-7 scale)",
caption = "Error bars show 95% confidence intervals. Means adjusted for baseline covariates, ICS, and relationship status.") +
apa
ggsave("output/figures/figure1_condition_means.png", fig1,
width = 8, height = 4.5, dpi = 300)
# ---- FIGURE 2. Asymmetry ------------------------------------------------------
f2 <- as.data.frame(em3) |>
mutate(condition = factor(cond_lab[as.character(condition)],
levels = c("Competitive", "Neutral", "Supportive")))
neut <- f2$emmean[f2$condition == "Neutral"]
fig2 <- ggplot(f2, aes(condition, emmean)) +
geom_hline(yintercept = neut, linetype = "dashed", colour = "grey40") +
geom_point(size = 3) +
geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .1) +
annotate("segment", x = 1.25, xend = 1.25,
y = f2$emmean[f2$condition == "Competitive"], yend = neut,
arrow = arrow(ends = "both", length = unit(.12, "cm"))) +
annotate("text", x = 1.38,
y = mean(c(f2$emmean[f2$condition == "Competitive"], neut)),
label = sprintf("d = %.2f", f2$emmean[f2$condition == "Competitive"] - neut),
hjust = 0, size = 3.4) +
annotate("segment", x = 2.75, xend = 2.75,
y = neut, yend = f2$emmean[f2$condition == "Supportive"],
arrow = arrow(ends = "both", length = unit(.12, "cm"))) +
annotate("text", x = 2.62,
y = mean(c(neut, f2$emmean[f2$condition == "Supportive"])),
label = sprintf("d = +%.2f", f2$emmean[f2$condition == "Supportive"] - neut),
hjust = 1, size = 3.4) +
labs(x = NULL, y = "Adjusted romantic self-perception",
caption = "Dashed line marks the neutral condition. Bracketed distances are the two contrast magnitudes\nunderlying the loss-asymmetry prediction. Error bars show 95% confidence intervals.") +
apa
ggsave("output/figures/figure2_loss_asymmetry.png", fig2,
width = 6.5, height = 4.8, dpi = 300)
# ---- FIGURE 3. ICS floor effect -----------------------------------------------
fig3 <- ggplot(d, aes(ics)) +
geom_histogram(binwidth = .25, fill = "grey75", colour = "black") +
geom_vline(xintercept = mean(d$ics), linetype = "dashed") +
annotate("text", x = mean(d$ics) + .15, y = Inf, vjust = 1.8, hjust = 0,
label = sprintf("M = %.2f", mean(d$ics)), size = 3.4) +
scale_x_continuous(limits = c(0.5, 7.5), breaks = 1:7) +
labs(x = "Intrasexual Competition Scale (1-7)", y = "Frequency",
caption = "Distribution of trait intrasexual competitiveness. Only 10 participants scored at or above 3.") +
apa
ggsave("output/figures/figure3_ics_distribution.png", fig3,
width = 6.5, height = 4.2, dpi = 300)
# ---- FIGURE 4. Mediation path diagram -----------------------------------------
pe2 <- lavaan::parameterEstimates(fit_med)
gv <- function(l) pe2$est[pe2$label == l]
st <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
pv <- function(l) pe2$pvalue[pe2$label == l]
boxes <- data.frame(
x = c(1, 3, 5), y = c(1, 2.4, 1),
lab = c("Competitive\ncondition", "Perceived peer\nselectivity", "Romantic\nself-perception"))
fig4 <- ggplot() +
geom_segment(aes(x = 1.5, y = 1.3, xend = 2.5, yend = 2.2),
arrow = arrow(length = unit(.2, "cm"))) +
geom_segment(aes(x = 3.5, y = 2.2, xend = 4.5, yend = 1.3),
arrow = arrow(length = unit(.2, "cm"))) +
geom_segment(aes(x = 1.6, y = 1, xend = 4.4, yend = 1),
arrow = arrow(length = unit(.2, "cm"))) +
annotate("text", x = 1.85, y = 1.9,
label = sprintf("a = %.2f%s", gv("a1"), st(pv("a1"))), size = 3.6) +
annotate("text", x = 4.15, y = 1.9,
label = sprintf("b = %.2f%s", gv("b"), st(pv("b"))), size = 3.6) +
annotate("text", x = 3, y = .85,
label = sprintf("c' = %.2f%s", gv("c1"), st(pv("c1"))), size = 3.6) +
geom_label(data = boxes, aes(x, y, label = lab), size = 3.6,
label.padding = unit(.4, "lines"), fill = "white") +
annotate("text", x = 3, y = .35, size = 3.2,
label = sprintf("Indirect effect = %.3f, 95%% BCa CI [%.3f, %.3f]",
pe2$est[pe2$label == "ind_comp"],
lavaan::parameterEstimates(fit_med, boot.ci.type = "bca.simple")$ci.lower[pe2$label == "ind_comp"],
lavaan::parameterEstimates(fit_med, boot.ci.type = "bca.simple")$ci.upper[pe2$label == "ind_comp"])) +
scale_x_continuous(limits = c(.3, 5.7)) +
scale_y_continuous(limits = c(.2, 2.9)) +
labs(caption = "Unstandardised coefficients. Competitive vs. neutral contrast shown; ICS, pre-exposure mate value\nand relationship status included as covariates. *p < .05, **p < .01, ***p < .001.") +
theme_void(base_size = 12) +
theme(plot.caption = element_text(hjust = 0, size = 9))
ggsave("output/figures/figure4_mediation_path.png", fig4,
width = 7.5, height = 4.5, dpi = 300)
# ---- FIGURE 5. Sensitivity forest plot ----------------------------------------
f5 <- bind_rows(lapply(
list(list(m1,"Primary (N = 146)"), list(s1a,"Excl. speeders (N = 138)"),
list(s1b,"Excl. straight-liners (N = 136)"), list(s1c,"MC passers only (N = 108)")),
function(z) {
ci <- as.data.frame(confint(pairs(emmeans(z[[1]], ~ condition), adjust = "bonferroni")))
ci <- ci[ci$contrast == "neutral - competitive", ]
data.frame(Sample = z[[2]], est = -ci$estimate,
lo = -ci$upper.CL, hi = -ci$lower.CL)
}))
f5$Sample <- factor(f5$Sample, levels = rev(f5$Sample))
fig5 <- ggplot(f5, aes(est, Sample)) +
geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
geom_point(size = 2.6) +
geom_errorbarh(aes(xmin = lo, xmax = hi), height = .15) +
labs(x = "Competitive - neutral difference in state dating anxiety", y = NULL,
caption = "Positive values indicate higher anxiety in the competitive condition.\nError bars show Bonferroni-adjusted 95% confidence intervals.") +
apa
ggsave("output/figures/figure5_sensitivity_forest.png", fig5,
width = 7, height = 3.8, dpi = 300)
cat("\nDone. Tables in output/tables/, figures in output/figures/\n")
list.files("output", recursive = TRUE)
table(dat2$condition, dat2$Status, useNA = "ifany")
chisq.test(c(55, 62, 43))
assigned <- table(dat2$condition[dat2$Status != 1])
analysed <- table(d$condition)
t3 <- data.frame(
Condition = c("Competitive", "Neutral", "Supportive"),
Assigned = as.integer(assigned[c("competitive","neutral","supportive")]),
Analysed = as.integer(analysed[c("competitive","neutral","supportive")])
)
t3$Attrition_pct <- round(100 * (t3$Assigned - t3$Analysed) / t3$Assigned, 1)
t3
write.csv(t3, "output/tables/table3_allocation_attrition.csv", row.names = FALSE)
t5 <- d |> group_by(condition) |>
summarise(n = n(),
anx_post_M = round(mean(anx_post),2), anx_post_SD = round(sd(anx_post),2),
self_perc_M = round(mean(self_perc),2), self_perc_SD = round(sd(self_perc),2),
peer_sel_M = round(mean(peer_sel),2), peer_sel_SD = round(sd(peer_sel),2),
ics_M = round(mean(ics),2), ics_SD = round(sd(ics),2),
mvs_pre_M = round(mean(mvs_pre),2), mvs_pre_SD = round(sd(mvs_pre),2),
anx_pre_M = round(mean(anx_pre),2), anx_pre_SD = round(sd(anx_pre),2),
age_M = round(mean(age_years),1), age_SD = round(sd(age_years),1))
write.csv(t5, "output/tables/table5_descriptives_by_condition.csv", row.names = FALSE)
t5
omni2 <- function(model, label) {
jt <- as.data.frame(emmeans::joint_tests(model))
es <- effectsize::F_to_eta2(jt$F.ratio, jt$df1, jt$df2,
ci = .95, alternative = "two.sided")
data.frame(Predictor = jt$`model term`,
df1 = jt$df1, df2 = jt$df2,
F = round(jt$F.ratio, 2),
p = round(jt$p.value, 4),
eta2p = round(es$Eta2_partial, 3),
CI_low = round(es$CI_low, 3),
CI_high = round(es$CI_high, 3),
Model = label)
}
write.csv(omni2(m1, "H1 anxiety"), "output/tables/table7_H1_omnibus.csv", row.names = FALSE)
write.csv(omni2(m3, "H3 self-perception"), "output/tables/table9_H3_omnibus.csv", row.names = FALSE)
omni2(m1, "H1")
omni2(m3, "H3")
exists("m1"); exists("s1c")
==============================================================================
# ==============================================================================
# 03_figures_apa.R
# Four APA 7-styled figures. Replaces the figure section of 02.
#
# Figure 4 (mediation path diagram) has been cut; figures renumbered.
# Titles and notes are added in Word, NOT here — APA places the figure number
# and italic title ABOVE the image, which ggplot cannot do.
#
# Requires in memory: d, m1, m3, s1a, s1b, s1c
# ==============================================================================
library(tidyverse)
library(emmeans)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
# APA 7: sans-serif, no gridlines, black text, no chartjunk.
# Change base_family to "Arial" or "Calibri" if Helvetica is unavailable.
apa <- theme_classic(base_size = 11, base_family = "Helvetica") +
theme(
axis.title    = element_text(colour = "black", size = 11),
axis.text     = element_text(colour = "black", size = 10),
axis.line     = element_line(colour = "black", linewidth = .4),
axis.ticks    = element_line(colour = "black", linewidth = .4),
strip.background = element_blank(),
strip.text    = element_text(face = "bold", size = 11, hjust = 0),
legend.position = "none",
plot.margin   = margin(8, 12, 8, 8)
)
cond_lab <- c(neutral = "Neutral", competitive = "Competitive", supportive = "Supportive")
em1 <- as.data.frame(emmeans(m1, ~ condition))
em3 <- as.data.frame(emmeans(m3, ~ condition))
# ---- FIGURE 1. Adjusted condition means, both outcomes ------------------------
f1 <- bind_rows(
em1 |> mutate(Outcome = "State dating anxiety"),
em3 |> mutate(Outcome = "Romantic self-perception")
) |>
mutate(condition = factor(cond_lab[as.character(condition)],
levels = c("Neutral", "Competitive", "Supportive")),
Outcome = factor(Outcome, levels = c("State dating anxiety",
"Romantic self-perception")))
fig1 <- ggplot(f1, aes(condition, emmean)) +
geom_col(fill = "grey80", colour = "black", width = .6, linewidth = .4) +
geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .1, linewidth = .4) +
geom_text(aes(label = sprintf("%.2f", emmean), y = upper.CL + .25),
size = 3.2, family = "Helvetica") +
facet_wrap(~ Outcome) +
scale_y_continuous(limits = c(0, 6), breaks = 1:6, expand = c(0, 0)) +
labs(x = NULL, y = "Adjusted mean (1–7 scale)") +
apa
ggsave("output/figures/figure1_condition_means.png", fig1,
width = 7, height = 3.8, dpi = 300, bg = "white")
# ---- FIGURE 2. Loss asymmetry -------------------------------------------------
f2 <- em3 |>
mutate(condition = factor(cond_lab[as.character(condition)],
levels = c("Competitive", "Neutral", "Supportive")))
neut  <- f2$emmean[f2$condition == "Neutral"]
compv <- f2$emmean[f2$condition == "Competitive"]
suppv <- f2$emmean[f2$condition == "Supportive"]
fig2 <- ggplot(f2, aes(condition, emmean)) +
geom_hline(yintercept = neut, linetype = "dashed", colour = "grey45", linewidth = .4) +
geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .08, linewidth = .4) +
geom_point(size = 2.6) +
# competitive distance
annotate("segment", x = 1.28, xend = 1.28, y = compv, yend = neut,
arrow = arrow(ends = "both", length = unit(.1, "cm")), linewidth = .35) +
annotate("text", x = 1.36, y = mean(c(compv, neut)), hjust = 0, size = 3.1,
family = "Helvetica", label = sprintf("d = %.2f", compv - neut)) +
# supportive distance
annotate("segment", x = 2.72, xend = 2.72, y = neut, yend = suppv,
arrow = arrow(ends = "both", length = unit(.1, "cm")), linewidth = .35) +
annotate("text", x = 2.64, y = mean(c(neut, suppv)), hjust = 1, size = 3.1,
family = "Helvetica", label = sprintf("d = +%.2f", suppv - neut)) +
scale_y_continuous(limits = c(3.3, 5.0), breaks = seq(3.5, 5.0, .5)) +
labs(x = NULL, y = "Adjusted romantic self-perception") +
apa
ggsave("output/figures/figure2_loss_asymmetry.png", fig2,
width = 5.5, height = 4.2, dpi = 300, bg = "white")
# ---- FIGURE 3. ICS floor effect -----------------------------------------------
fig3 <- ggplot(d, aes(ics)) +
geom_histogram(binwidth = .25, fill = "grey80", colour = "black",
linewidth = .3, boundary = 1) +
geom_vline(xintercept = mean(d$ics), linetype = "dashed",
colour = "grey30", linewidth = .4) +
annotate("text", x = mean(d$ics) + .2, y = 26, hjust = 0, size = 3.1,
family = "Helvetica", label = sprintf("M = %.2f", mean(d$ics))) +
scale_x_continuous(limits = c(.75, 7.25), breaks = 1:7, expand = c(0, 0)) +
scale_y_continuous(expand = expansion(mult = c(0, .08))) +
labs(x = "Intrasexual Competition Scale score", y = "Frequency") +
apa
ggsave("output/figures/figure3_ics_distribution.png", fig3,
width = 5.5, height = 3.6, dpi = 300, bg = "white")
# ---- FIGURE 4. Sensitivity forest (was Figure 5) -------------------------------
f4 <- bind_rows(lapply(
list(list(m1, "Primary (N = 146)"),
list(s1a, "Excluding speeders (N = 138)"),
list(s1b, "Excluding straight-liners (N = 136)"),
list(s1c, "Manipulation check passers (N = 108)")),
function(z) {
ci <- as.data.frame(confint(pairs(emmeans(z[[1]], ~ condition),
adjust = "bonferroni")))
ci <- ci[ci$contrast == "neutral - competitive", ]
data.frame(Sample = z[[2]], est = -ci$estimate,
lo = -ci$upper.CL, hi = -ci$lower.CL)
}))
f4$Sample <- factor(f4$Sample, levels = rev(f4$Sample))
fig4 <- ggplot(f4, aes(est, Sample)) +
geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = .4) +
geom_errorbarh(aes(xmin = lo, xmax = hi), height = .12, linewidth = .4) +
geom_point(size = 2.4) +
scale_x_continuous(limits = c(-0.2, 1.8), breaks = seq(0, 1.8, .3)) +
labs(x = "Competitive – neutral difference in state dating anxiety", y = NULL) +
apa
ggsave("output/figures/figure4_sensitivity_forest.png", fig4,
width = 6.5, height = 3.2, dpi = 300, bg = "white")
cat("\nFour figures written to output/figures/\n")
list.files("output/figures")
# ==============================================================================
# 04_appendix_tables.R
# Generates the four appendix tables as CSVs.
# Requires in memory: d, m1, fit_med, m3, s1a, s1b, s1c, s3a, s3b, s3c
# ==============================================================================
library(tidyverse)
library(emmeans)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
# ---- TABLE A1. Participant characteristics ------------------------------------
demo_tab <- function(var, labels, name) {
v  <- factor(as.numeric(var), levels = seq_along(labels), labels = labels)
tb <- table(v)
data.frame(Category = c(name, rep("", length(tb) - 1)),
Subcategory = names(tb),
n = as.integer(tb),
pct = round(100 * as.integer(tb) / sum(tb), 1))
}
age_n <- c(sum(d$age_years <= 24),
sum(d$age_years >= 25 & d$age_years <= 29),
sum(d$age_years >= 30))
tA1 <- bind_rows(
data.frame(Category = c("Age", "", ""),
Subcategory = c("18–24", "25–29", "30–35"),
n = age_n,
pct = round(100 * age_n / nrow(d), 1)),
demo_tab(d$Q16, c("Single, not dating", "Single, casually dating",
"Committed relationship", "Engaged or married", "Other"),
"Relationship status"),
demo_tab(d$Q17, c("White", "Black/African/Caribbean", "Asian/Asian British",
"Arab/Berber/North African", "Mixed or multiple",
"Other", "Prefer not to say"), "Ethnicity"),
demo_tab(d$Q18, c("Secondary school/GCSEs", "Sixth form/A-Levels",
"Undergraduate degree", "Postgraduate degree", "Other"),
"Highest education completed"),
demo_tab(d$Q19, c("Never", "Rarely", "Sometimes", "Often", "Very often"),
"Discusses dating with close female friends")
)
write.csv(tA1, "output/tables/tableA1_participant_characteristics.csv", row.names = FALSE)
# ---- TABLE A2. Simple slopes of ICS by condition (anxiety model) --------------
tA2 <- as.data.frame(emtrends(m1, ~ condition, var = "ics_c")) |>
transmute(Condition = str_to_title(as.character(condition)),
Slope = round(ics_c.trend, 3),
SE = round(SE, 3),
df = df,
CI_low = round(lower.CL, 3),
CI_high = round(upper.CL, 3))
write.csv(tA2, "output/tables/tableA2_simple_slopes.csv", row.names = FALSE)
# ---- TABLE A3. Mediation paths ------------------------------------------------
pe <- lavaan::parameterEstimates(fit_med, boot.ci.type = "bca.simple")
tA3 <- pe |>
filter(op %in% c("~", ":=")) |>
transmute(Outcome = lhs,
Predictor = rhs,
Label = ifelse(is.na(label) | label == "", "", label),
b = round(est, 3),
SE = round(se, 3),
z = round(z, 2),
p = round(pvalue, 3),
CI_low = round(ci.lower, 3),
CI_high = round(ci.upper, 3))
write.csv(tA3, "output/tables/tableA3_mediation_paths.csv", row.names = FALSE)
# ---- TABLE A4. Sensitivity analyses -------------------------------------------
sens_row <- function(model, n, label, outcome) {
jt <- as.data.frame(joint_tests(model))
pr <- as.data.frame(pairs(emmeans(model, ~ condition), adjust = "bonferroni"))
cn <- pr[pr$contrast == "neutral - competitive", ]
data.frame(Outcome = outcome, Sample = label, N = n,
F_condition   = round(jt$F.ratio[jt$`model term` == "condition"], 2),
p_condition   = round(jt$p.value[jt$`model term` == "condition"], 3),
F_interaction = round(jt$F.ratio[jt$`model term` == "condition:ics_c"], 2),
p_interaction = round(jt$p.value[jt$`model term` == "condition:ics_c"], 3),
Comp_vs_neut  = round(-cn$estimate, 3),
p_contrast    = round(cn$p.value, 3))
}
tA4 <- bind_rows(
sens_row(m1,  146, "Primary",                    "State dating anxiety"),
sens_row(s1a, 138, "Excluding speeders",         "State dating anxiety"),
sens_row(s1b, 136, "Excluding straight-liners",  "State dating anxiety"),
sens_row(s1c, 108, "Manipulation check passers", "State dating anxiety"),
sens_row(m3,  146, "Primary",                    "Romantic self-perception"),
sens_row(s3a, 138, "Excluding speeders",         "Romantic self-perception"),
sens_row(s3b, 136, "Excluding straight-liners",  "Romantic self-perception"),
sens_row(s3c, 108, "Manipulation check passers", "Romantic self-perception")
)
write.csv(tA4, "output/tables/tableA4_sensitivity_analyses.csv", row.names = FALSE)
cat("\nFour appendix tables written.\n")
list.files("output/tables", pattern = "^tableA")
pe <- lavaan::parameterEstimates(fit_med, boot.ci.type = "bca.simple")
format(pe$pvalue[pe$op %in% c("~", ":=")], scientific = FALSE)
dat <- readRDS("data/derived/dat_scales.rds")
# zero-order correlations
cor.test(dat$age, dat$anx_post)
cor.test(dat$age, dat$self_perc)
m1_age <- lm(anx_post ~ condition * ics_c + relstat3 + anx_pre_c + age, data = dat)
sessionInfo()
citation("emmeans")
names(dat)
dat$ics_c     <- scale(dat$ics, center = TRUE, scale = FALSE)[, 1]
dat$anx_pre_c <- scale(dat$anx_pre, center = TRUE, scale = FALSE)[, 1]
dat$mvs_pre_c <- scale(dat$mvs_pre, center = TRUE, scale = FALSE)[, 1]
cor.test(dat$age, dat$anx_post)
m1_age <- lm(anx_post ~ condition * ics_c + relstat3 + anx_pre_c + age, data = dat)
names(dat)
# whichever script builds ics_c, anx_pre_c, mvs_pre_c, relstat3 —
# run it, then the models are reproducible exactly as reported
source("scripts/02_analysis.R")   # substitute your actual path/filename
getwd()
list.files(recursive = TRUE, pattern = "\\.R$")
list.files(recursive = TRUE)
savehistory("scripts/02_analysis_RECOVERED.R")
dat_analysed <- readRDS("data/derived/dat_analysed.rds")
names(dat_analysed)
savehistory("scripts/02_analysis_RECOVERED.R")
names(readRDS("data/derived/dat_scales.rds"))
savehistory("scripts/02_analysis_RECOVERED.R")
dat <- readRDS("data/derived/dat_scales.rds")
table(dat$Q17, useNA = "ifany")
table(dat$Q17, useNA = "ifany")
table(dat$Q18, useNA = "ifany")
savehistory("scripts/02_analysis_RECOVERED.R")
emmeans::joint_tests(m1)   # condition F should be 7.81, p < .001
emmeans::joint_tests(m3)   # condition F should be 1.52, p = .222
# ---- Derived model variables ------------------------------------------------
dat <- readRDS("data/derived/dat_scales.rds")
# Relationship status: five categories collapsed to three.
# Q17: 1 = single, not dating   2 = single, casually dating
#      3 = committed relationship   4 = engaged or married   5 = other
# Engaged/married (n = 5) and other (n = 6) are too sparse to estimate;
# 1-2 collapse to single, 3-4 to partnered, 5 retained as other.
dat$relstat3 <- factor(
dplyr::case_when(
dat$Q17 %in% c(1, 2) ~ "single",
dat$Q17 %in% c(3, 4) ~ "partnered",
dat$Q17 == 5         ~ "other"
),
levels = c("single", "partnered", "other")   # single = reference
)
# Condition: neutral as reference category
dat$condition <- relevel(factor(dat$condition), ref = "neutral")
# Mean-centred continuous covariates
dat$ics_c     <- dat$ics     - mean(dat$ics,     na.rm = TRUE)
dat$anx_pre_c <- dat$anx_pre - mean(dat$anx_pre, na.rm = TRUE)
dat$mvs_pre_c <- dat$mvs_pre - mean(dat$mvs_pre, na.rm = TRUE)
# ---- Verification -----------------------------------------------------------
stopifnot(nrow(dat) == 146)
table(dat$relstat3)        # expect single 87, partnered 53, other 6
levels(dat$condition)      # expect "neutral" first
round(sapply(dat[c("ics_c","anx_pre_c","mvs_pre_c")], mean, na.rm = TRUE), 10)  # all ~0
m1 <- lm(anx_post  ~ condition * ics_c + relstat3 + anx_pre_c, data = dat)
m3 <- lm(self_perc ~ condition * ics_c + relstat3 + mvs_pre_c, data = dat)
emmeans::joint_tests(m1)   # condition F should be 7.81, p < .001
emmeans::joint_tests(m3)   # condition F should be 1.52, p = .222
table(dat$Q17, useNA = "ifany")
table(dat$Q15, useNA = "ifany")
table(dat$Q19, useNA = "ifany")
table(dat$Q17, useNA = "ifany")
savehistory("scripts/02_analysis_RECOVERED.R")

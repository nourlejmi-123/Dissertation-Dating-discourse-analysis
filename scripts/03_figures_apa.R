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

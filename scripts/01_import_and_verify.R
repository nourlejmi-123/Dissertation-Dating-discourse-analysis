# ==============================================================================
# 01_import_and_verify.R
# Peer dating discourse and romantic self-perception
#
# PURPOSE: import the Qualtrics export, strip its header rows, build a codebook,
#          and verify that condition assignment is intact.
#
# NOTHING IN THIS SCRIPT TOUCHES AN OUTCOME MEAN. Running it does not
# compromise pre-specification.
#
# Run date: ____________
# ==============================================================================


# ---- 0. Project structure ----------------------------------------------------
# Create these folders inside your RStudio project before running:
#   data/raw/        <- the Qualtrics CSV. Never edit by hand.
#   data/derived/    <- cleaned objects this pipeline writes
#   scripts/         <- these numbered scripts
#   output/          <- tables and figures
#
# dir.create("data/raw", recursive = TRUE)
# dir.create("data/derived", recursive = TRUE)
# dir.create("output", recursive = TRUE)


# ---- 1. Packages -------------------------------------------------------------
# Install once, then comment out.
# install.packages(c("tidyverse", "janitor", "psych", "car", "emmeans",
#                    "effectsize", "boot", "broom", "lavaan", "patchwork"))

library(tidyverse)   # data handling
library(janitor)     # clean_names(), tabyl()
library(psych)       # alpha() for Cronbach's alpha and item-total correlations
library(car)         # Anova()
library(emmeans)     # estimated marginal means, contrasts, simple slopes
library(effectsize)  # eta_squared()
library(broom)       # tidy model output

set.seed(20260814)   # fix the seed NOW so bootstraps are reproducible.
                     # Record this number in the pre-spec note.


# ---- 2. Import ---------------------------------------------------------------
# Qualtrics CSVs have THREE header rows:
#   row 1 = column names (read as headers)
#   row 2 = full question text
#   row 3 = ImportId JSON
# We keep row 2 as a codebook and discard row 3.

raw_path <- "data/raw/REPLACE_WITH_FILENAME.csv"

raw <- read_csv(raw_path, show_col_types = FALSE)

# Codebook: column name -> question text
codebook <- tibble(
  variable = names(raw),
  question = as.character(unlist(raw[1, ]))
)

write_csv(codebook, "data/derived/codebook.csv")

# The data proper
dat <- raw |>
  slice(-(1:2)) |>
  type_convert(guess_integer = TRUE)

cat("\nRows imported:", nrow(dat), "\n")
cat("Columns imported:", ncol(dat), "\n\n")


# ---- 3. First look at structure ----------------------------------------------
# Send me the output of these three lines.

names(dat)

glimpse(dat)

print(codebook, n = Inf)


# ---- 4. Standard Qualtrics metadata ------------------------------------------
# These should exist in every export. Confirm before proceeding.

meta_expected <- c("ResponseId", "Finished", "Progress",
                   "Duration (in seconds)", "RecordedDate")

setdiff(meta_expected, names(dat))   # should return character(0)


# ---- 5. CONDITION VERIFICATION -----------------------------------------------
# The most important check in this script. Do not proceed past it.
#
# Replace `condition_raw` with whatever your embedded data field is called
# (common names: "condition", "Condition", "cond", "FL_12_DO").

condition_var <- "REPLACE_WITH_CONDITION_COLUMN"

if (condition_var %in% names(dat)) {

  dat |>
    count(.data[[condition_var]]) |>
    mutate(pct = round(100 * n / sum(n), 1)) |>
    print()

  # How many rows have no condition recorded?
  n_missing_cond <- sum(is.na(dat[[condition_var]]))
  cat("\nRows with missing condition:", n_missing_cond, "\n")

} else {
  cat("\nCondition column not found. Listing candidate columns:\n")
  print(grep("cond|FL_|DO|rand|block", names(dat),
             ignore.case = TRUE, value = TRUE))
}

# ---- 5b. Fallback: reconstruct condition from display order ------------------
# Only needed if section 5 shows missing or empty condition data.
#
# With "Export viewing order data for randomized surveys" ticked, Qualtrics
# writes columns ending in "_DO" recording which items were displayed.
# Each participant saw exactly one vignette, so the vignette that is
# non-missing identifies the condition.

do_cols <- grep("_DO$", names(dat), value = TRUE)
print(do_cols)

# Once we identify the three vignette columns, reconstruction looks like:
#
# dat <- dat |>
#   mutate(condition = case_when(
#     !is.na(VIGNETTE_COMPETITIVE_COL) ~ "competitive",
#     !is.na(VIGNETTE_SUPPORTIVE_COL)  ~ "supportive",
#     !is.na(VIGNETTE_NEUTRAL_COL)     ~ "neutral",
#     TRUE ~ NA_character_
#   ))
#
# Do not write this until we have confirmed the column names together.


# ---- 6. Set condition as a factor with NEUTRAL as reference ------------------
# Neutral must be the reference level for every model. Set it once, here.

dat <- dat |>
  mutate(condition = factor(condition,
                            levels = c("neutral", "competitive", "supportive")))

levels(dat$condition)   # confirm "neutral" is first


# ---- 7. Sanity checks that involve no outcome means --------------------------

# Completion
dat |> count(Finished)

# Duration distribution — needed for the 5th percentile speeder rule in Stage 3
summary(dat$`Duration (in seconds)`)
quantile(dat$`Duration (in seconds)`, probs = 0.05, na.rm = TRUE)

# Duplicate response IDs
sum(duplicated(dat$ResponseId))


# ---- 8. Save ------------------------------------------------------------------
saveRDS(dat, "data/derived/dat_imported.rds")

# STOP HERE. Send output from sections 3, 5 and 7 before running Stage 3.
# ==============================================================================

# ---------------------------------------------------------------------------- #
# Compute Missing Data Rates
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/01_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./results/descriptives/res_outcomes_itt_by_cond.RData")
load("./results/descriptives/res_outcomes_s5_train_compl_by_cond.RData")

# ---------------------------------------------------------------------------- #
# Compute rates of scale-level missingness ----
# ---------------------------------------------------------------------------- #

# Define function to compute rate of scale-level missingness for given outcome
# based on number of ITT participants (N) and number of time points (J)

compute_scale_missingness_itt <- function(desc_tbl, outcome, J) {
  n_cols <- c("n_TRAINING", "n_LR_TRAINING", "n_HR_NO_COACH", "n_HR_COACH", 
              "n_CTRL_cls", "n_CTRL_ncls")
  N <- 1234
  
  # Restrict to sample size columns for outcome and convert to numeric
  
  dat <- desc_tbl[desc_tbl$Outcome == outcome, n_cols]
  dat <- sapply(dat, as.numeric)
  
  # Compute proportion of scale-level missingness
  
  prop <- 1 - sum(dat, na.rm = TRUE)/(N*J)
  
  cat(outcome, ": ", prop, "\n", sep = "")
}

# Run function and write results

missing_rates_path <- "./results/missing_rates/"
dir.create(missing_rates_path)

sink(file = paste0(missing_rates_path, "scale_level.txt"))

cat("Proportions of Scale-Level Missingness for Each Outcome:", "\n\n")

compute_scale_missingness_itt(res_outcomes_itt_by_cond, "rr_pos_threat_m", 4)
compute_scale_missingness_itt(res_outcomes_itt_by_cond, "rr_neg_threat_m", 4)
compute_scale_missingness_itt(res_outcomes_itt_by_cond, "bbsiq_ben_m",     4)
compute_scale_missingness_itt(res_outcomes_itt_by_cond, "bbsiq_neg_m",     4)
compute_scale_missingness_itt(res_outcomes_itt_by_cond, "oa_m",            7)
compute_scale_missingness_itt(res_outcomes_itt_by_cond, "dass21_as_m",     4)

sink()






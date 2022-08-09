# ---------------------------------------------------------------------------- #
# Specify Model Input Combinations
# Authors: Jeremy W. Eberle
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
# Specify model input combinations ----
# ---------------------------------------------------------------------------- #

# Specify input elements

analysis_samples <- c("c1_corr_itt", "c1_corr_s5_train_compl",
                      "c2_4_class_meas_compl", "c2_4_s5_train_compl")

a_contrasts <- c("a1", "a2_1", "a2_2", "a2_3")

eff_y_vars <- c("rr_neg_threat_m", "rr_pos_threat_m",
                "bbsiq_neg_m", "bbsiq_ben_m", 
                "oa", "dass21_as_m")

drp_y_vars <- "miss_session_train_sum"

# Compute lengths of input elements

n_analysis_samples <- length(analysis_samples)
n_a_contrasts      <- length(a_contrasts)
n_eff_y_vars       <- length(eff_y_vars)
n_drp_y_vars       <- length(drp_y_vars)

n_eff_combos <- n_analysis_samples*n_a_contrasts*n_eff_y_vars
n_drp_combos <- n_analysis_samples*n_a_contrasts*n_drp_y_vars

# Specify input combinations for efficacy and dropout models

eff_combos <- data.frame(analysis_type   = rep("efficacy",
                                               times = n_eff_combos),
                         analysis_sample = rep(analysis_samples, 
                                               each  = n_a_contrasts*n_eff_y_vars),
                         a_contrast      = rep(a_contrasts,
                                               times = n_analysis_samples,
                                               each  = n_eff_y_vars),
                         y_var           = rep(eff_y_vars,
                                               times = n_analysis_samples*n_a_contrasts))

drp_combos <- data.frame(analysis_type   = rep("dropout",
                                               times = n_drp_combos),
                         analysis_sample = rep(analysis_samples, 
                                               each  = n_a_contrasts*n_drp_y_vars),
                         a_contrast      = rep(a_contrasts,
                                               times = n_analysis_samples,
                                               each  = n_drp_y_vars),
                         y_var           = rep(drp_y_vars,
                                               times = n_analysis_samples*n_a_contrasts))

model_input_combos <- rbind(eff_combos, drp_combos)

# Export data frame

save(model_input_combos, file = "./results/bayesian/model_input_combos.RData")
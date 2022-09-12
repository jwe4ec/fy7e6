# ---------------------------------------------------------------------------- #
# Pool Results
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
# Import trimmed results ----
# ---------------------------------------------------------------------------- #

# Define function to import "results_trim.RData" for many models at once

import_results <- function(anlys_path_pattern) {
  res_dir <- "./results/bayesian"
  
  res_filenames <- list.files(res_dir, pattern = "results_trim.RData", 
                              recursive = TRUE, full.names = FALSE)
  res_filenames <- res_filenames[grep(anlys_path_pattern, res_filenames)]
  
  res <- lapply(paste0(res_dir, "/", res_filenames),
                function(x) { get(load(x, environment())) })
  
  if (anlys_path_pattern %in% c("dropout/out/c1_", "efficacy/out/c1_")) {
    names(res) <- lapply(res, function(x) {
      paste0(x$per_bs_smp$`1`$analysis_type, "_",
             x$per_bs_smp$`1`$analysis_sample, "_",
             x$per_bs_smp$`1`$a_contrast, "_",
             x$per_bs_smp$`1`$y_var)
    })
  } else if (anlys_path_pattern %in% c("dropout/out/c2_4_", "efficacy/out/c2_4_")) {
    names(res) <- lapply(res, function(x) {
      paste0(x$analysis_type, "_",
             x$analysis_sample, "_",
             x$a_contrast, "_",
             x$y_var)
    })
  }
  
  return(res)
}

# Run function for each analysis type for "a1" ("c1_") and "a2" ("c2_4_") models

res_drp_c1   <- import_results("dropout/out/c1_")
res_drp_c2_4 <- import_results("dropout/out/c2_4_")
res_eff_c1   <- import_results("efficacy/out/c1_")
res_eff_c2_4 <- import_results("efficacy/out/c2_4_")

# ---------------------------------------------------------------------------- #
# Pool results ----
# ---------------------------------------------------------------------------- #

# TODO (draw from previous code below)





# Define function to pool results across bootstrap samples for converged models

pool_results <- function(results_list) {
  # Restrict to converged models (models in which all parameters pass Geweke's test)
  
  # TODO: For testing, temporarily pool across models that *didn't* converge
  
  
  
  
  
  results_list_converged <- Filter(function(x) x$geweke_converge_all == FALSE, results_list)
  
  number_converged <- length(results_list_converged)
  
  # Extract means and SDs of estimated parameters for each model
  
  results_list_converged_stats <- lapply(results_list_converged, function(x) x[["summary"]]$statistics)
  
  results_list_converged_stats_mean <- lapply(results_list_converged_stats, function(x) x[, "Mean"])
  results_list_converged_stats_sd   <- lapply(results_list_converged_stats, function(x) x[, "SD"])
  
  # Pool results across models by computing (a) average of estimated means, (b) 
  # percentile bootstrap 95% CIs for average of estimated means (2.5th and 97.5th
  # percentiles of estimated means), (c) empirical SD (SD of estimated means), and 
  # (d) average SD (mean of estimated SDs)
  
  pooled_mean   <- apply(simplify2array(results_list_converged_stats_mean), 1, mean)
  pooled_pctl_bs_ci_ll <- apply(simplify2array(results_list_converged_stats_mean), 1, 
                                quantile, probs = .025)
  pooled_pctl_bs_ci_ul <- apply(simplify2array(results_list_converged_stats_mean), 1, 
                                quantile, probs = .975)
  pooled_emp_sd <- apply(simplify2array(results_list_converged_stats_mean), 1, sd)
  
  pooled_avg_sd <- apply(simplify2array(results_list_converged_stats_sd), 1, mean)
  
  # Combine pooled results in data frame
  
  pooled_results <- data.frame(mean = pooled_mean,
                               pctl_bs_ci_ll = pooled_pctl_bs_ci_ll,
                               pctl_bs_ci_ul = pooled_pctl_bs_ci_ul,
                               emp_sd = pooled_emp_sd,
                               avg_sd = pooled_avg_sd)
  
  # Combine number of converged models and pooled results in list
  
  pooled <- list(number_converged = number_converged,
                 results = pooled_results)
  
  return(pooled)
}




# TODO: Pool results across 500 bootstrap samples for models that converge. For
# testing, temporarily pool across models that *didn't* converge.

pooled <- pool_results(results_list)
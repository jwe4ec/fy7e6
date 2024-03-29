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

# Load packages

groundhog.library("data.table", groundhog_day)

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
res_eff_c1   <- import_results("efficacy/out/c1_")
res_drp_c2_4 <- import_results("dropout/out/c2_4_")
res_eff_c2_4 <- import_results("efficacy/out/c2_4_")

# ---------------------------------------------------------------------------- #
# Extract model info ----
# ---------------------------------------------------------------------------- #

# Define function to extract model info

extract_model_info <- function(results, a_contrast_type) {
  model_info_elements <- c("analysis_type", "analysis_sample", "a_contrast",
                           "y_var", "model_results_path_stem", "total_iterations",
                           "burn_iterations", "remaining_iterations")
  
  if (a_contrast_type == "a1") {
    # For "a1" models, extract model info based on first bootstrap sample
    
    bs_smp_1 <- results$per_bs_smp[[1]]
    
    results$model_info <- bs_smp_1[names(bs_smp_1) %in% model_info_elements]
  } else if (a_contrast_type == "a2") {
    results$model_info <- results[names(results) %in% model_info_elements]
  }
  
  return(results)
}

# Run function

res_drp_c1   <- lapply(res_drp_c1,   extract_model_info, "a1")
res_eff_c1   <- lapply(res_eff_c1,   extract_model_info, "a1")
res_drp_c2_4 <- lapply(res_drp_c2_4, extract_model_info, "a2")
res_eff_c2_4 <- lapply(res_eff_c2_4, extract_model_info, "a2")

# ---------------------------------------------------------------------------- #
# Pool Geweke's statistic across bootstrap samples for "a1" models ----
# ---------------------------------------------------------------------------- #

# Define function to pool Geweke's statistic across bootstrap samples

pool_geweke <- function(results) {
  # Count number of bootstrap samples in which model converged per Geweke's 
  # statistic for all parameters
  
  results_converge_all <- Filter(function(x) x$geweke_converge_all == TRUE, 
                                 results$per_bs_smp)
  
  num_converge_all <- length(results_converge_all)
  pct_converge_all <- 100*num_converge_all/length(results$per_bs_smp)
  
  # Extract Geweke's statistic for each bootstrap sample
  
  results_geweke <- lapply(results$per_bs_smp, function(x) x[["geweke"]]$z)

  # Pool Geweke's statistic for each parameter across bootstrap samples by
  # computing its (a) mean, (b) empirical SD, (c) minimum, (d) maximum, (e) 
  # number and (f) percentage of values outside (-1.96, 1.96), and numbers of 
  # (g) infinite and (h) finite values
  
  pooled_mean         <- apply(simplify2array(results_geweke), 1, mean)
  pooled_emp_sd       <- apply(simplify2array(results_geweke), 1, sd)
  pooled_min          <- apply(simplify2array(results_geweke), 1, min)
  pooled_max          <- apply(simplify2array(results_geweke), 1, max)
  pooled_num_outrange <- apply(simplify2array(results_geweke), 1, 
                               function(x) sum(x < -1.96 | x > 1.96))
  pooled_pct_outrange <- 100*pooled_num_outrange/length(results_geweke)
  pooled_num_infinite <- apply(simplify2array(results_geweke), 1, 
                               function(x) sum(is.infinite(x)))
  pooled_num_finite   <- apply(simplify2array(results_geweke), 1, 
                               function(x) sum(is.finite(x)))
  
  # Combine pooled statistics in data frame
  
  pooled_stats <- data.frame(mean         = pooled_mean,
                             emp_sd       = pooled_emp_sd,
                             min          = pooled_min,
                             max          = pooled_max,
                             num_outrange = pooled_num_outrange,
                             pct_outrange = pooled_pct_outrange,
                             num_infinite = pooled_num_infinite,
                             num_finite   = pooled_num_finite)
  
  # Summarize pooled stats across model parameters
  
  pooled_stats_summary <- list(num_converge_all = num_converge_all,
                               pct_converge_all = pct_converge_all,
                               min              = min(pooled_stats$min),
                               max              = max(pooled_stats$max),
                               min_num_outrange = min(pooled_stats$num_outrange),
                               max_num_outrange = max(pooled_stats$num_outrange),
                               num_infinite     = sum(pooled_stats$num_infinite),
                               all_finite       = all(pooled_stats$num_finite == 500))

  # Add pooled stats and summary to results
  
  results$pooled_geweke_stats         <- pooled_stats
  results$pooled_geweke_stats_summary <- pooled_stats_summary
  
  return(results)
}

# Run function for "a1" ("c1") models

res_drp_c1 <- lapply(res_drp_c1, pool_geweke)
res_eff_c1 <- lapply(res_eff_c1, pool_geweke)

# ---------------------------------------------------------------------------- #
# Summarize Geweke's statistic across model parameters for "a2" models ----
# ---------------------------------------------------------------------------- #

# Define function to summarize Geweke's statistic across model parameters

summarize_geweke <- function(results) {
  # Put Geweke stat and whether the value is outside (-1.96, 1.96) in data frame
  
  stats <- data.frame(z        = results$geweke$z,
                      outrange = !(results$geweke_converge_each))
  
  # Summarize stats across model parameters
  
  stats_summary <- list(min          = min(stats$z),
                        max          = max(stats$z),
                        num_outrange = sum(stats$outrange),
                        num_infinite = sum(is.infinite(stats$z)),
                        all_finite   = all(is.finite(stats$z)))
  
  # Add stats and summary to results
  
  results$geweke_stats <- stats
  results$geweke_stats_summary <- stats_summary

  return(results)
}

# Run function for "a2" ("c2_4") models

res_drp_c2_4 <- lapply(res_drp_c2_4, summarize_geweke)
res_eff_c2_4 <- lapply(res_eff_c2_4, summarize_geweke)

# ---------------------------------------------------------------------------- #
# Create summary table of Geweke's statistic ----
# ---------------------------------------------------------------------------- #

# Define function to create summary table of Geweke's statistic

create_geweke_table <- function(results, a_contrast_type) {
  # Extract model info for each model
  
  model_info <- lapply(results, function(x) x$model_info)
  
  # Extract Geweke's stats summary for each model
  
  if (a_contrast_type == "a1") {
    geweke_stats_summaries <- lapply(results, function(x) x$pooled_geweke_stats_summary)
  } else if (a_contrast_type == "a2") {
    geweke_stats_summaries <- lapply(results, function(x) x$geweke_stats_summary)
  }
  
  # Convert nested lists to data frames
  
  df_stats_summaries <- as.data.frame(do.call(rbind, geweke_stats_summaries))
  df_model_info      <- as.data.frame(do.call(rbind, model_info))
  
  # Round selected stats summary columns
  
  df_stats_summaries$min <- lapply(df_stats_summaries$min, round, 2)
  df_stats_summaries$max <- lapply(df_stats_summaries$max, round, 2)
  
  # Remove undesired model info columns
  
  df_model_info$model_results_path_stem <- NULL
  
  # Combine data frames
  
  df <- cbind(df_model_info, df_stats_summaries)
  
  # Rename rows
  
  row.names(df) <- 1:nrow(df)
  
  return(df)
}

# Run function

res_drp_c1_geweke_summary_tbl   <- create_geweke_table(res_drp_c1,   "a1")
res_eff_c1_geweke_summary_tbl   <- create_geweke_table(res_eff_c1,   "a1")
res_drp_c2_4_geweke_summary_tbl <- create_geweke_table(res_drp_c2_4, "a2")
res_eff_c2_4_geweke_summary_tbl <- create_geweke_table(res_eff_c2_4, "a2")

# Combine tables for "a1" models and for "a2" models

res_c1_geweke_summary_tbl   <- rbind(res_drp_c1_geweke_summary_tbl, 
                                     res_eff_c1_geweke_summary_tbl)
res_c2_4_geweke_summary_tbl <- rbind(res_drp_c2_4_geweke_summary_tbl, 
                                     res_eff_c2_4_geweke_summary_tbl)

# Export tables as CSV using "fwrite" (given that columns are lists)

dir.create("./results/bayesian/summary_dx")

fwrite(res_c1_geweke_summary_tbl,
       "./results/bayesian/summary_dx/res_c1_geweke_summary_tbl.csv",
       row.names = FALSE)
fwrite(res_c2_4_geweke_summary_tbl,
       "./results/bayesian/summary_dx/res_c2_4_geweke_summary_tbl.csv",
       row.names = FALSE)





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
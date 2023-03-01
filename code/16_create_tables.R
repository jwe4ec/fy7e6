# ---------------------------------------------------------------------------- #
# Create Tables
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
# Import results and parameter labels ----
# ---------------------------------------------------------------------------- #

pooled_path <- "./results/bayesian/pooled/"
param_path  <- "./code/parameter_labels/"

load(paste0(pooled_path, "res_drp_c1_2000bs.RData"))
load(paste0(pooled_path, "res_eff_c1_2000bs.RData"))
load(paste0(pooled_path, "res_drp_c2_4_1000000iter.RData"))
load(paste0(pooled_path, "res_eff_c2_4_1000000iter.RData"))

eff_param_labels <- read.csv(paste0(param_path, "eff_param_labels.csv"))
drp_param_labels <- read.csv(paste0(param_path, "drp_param_labels.csv"))

# ---------------------------------------------------------------------------- #
# Create full tables for "a1" models ----
# ---------------------------------------------------------------------------- #

# Define function to create tables with full results for "a1" models

create_full_tbl_a1 <- function(results, param_labels, out_path) {
  # Extract model info and pooled stats
  
  model_info        <- results$model_info
  pooled_conv_stats <- results$pooled_conv_stats
  
  # Add parameter labels
  
  pooled_conv_stats <- cbind(param = rownames(pooled_conv_stats), pooled_conv_stats)
  
  pooled_conv_stats <- merge(pooled_conv_stats, param_labels, "param", 
                             all.x = TRUE, sort = FALSE)
  
  # Identify significant parameters
  
  pooled_conv_stats$sig <- NA
  
  pooled_conv_stats$sig <- ifelse(pooled_conv_stats$pctl_bs_ci_ll > 0 | 
                                    pooled_conv_stats$pctl_bs_ci_ul < 0, 1, 0)
  
  # Round selected columns
  
  target_cols <- c("mean", "pctl_bs_ci_ll", "pctl_bs_ci_ul", "emp_sd", "avg_sd")
  
  pooled_conv_stats[, target_cols] <- round(pooled_conv_stats[, target_cols], 2)

  # Format CIs
  
  pooled_conv_stats$pctl_bs_ci <- paste0("[", pooled_conv_stats$pctl_bs_ci_ll, ", ",
                                         pooled_conv_stats$pctl_bs_ci_ul, "]")
  
  # Remove redundant columns and rearrange
  
  rm_cols <- c("pctl_bs_ci_ll", "pctl_bs_ci_ul")
  
  pooled_conv_stats <- pooled_conv_stats[, !(names(pooled_conv_stats) %in% rm_cols)]
  
  pooled_conv_stats <- pooled_conv_stats[, c("param", "model", "label",
                                             "num_converge_all", "mean", "emp_sd",
                                             "avg_sd", "pctl_bs_ci", "sig")]
  
  # TODO: Write to one PDF with multiple pages (for now export to CSV)
  
  model_name <- paste0(model_info$analysis_type, "_",
                       model_info$analysis_sample, "_",
                       model_info$a_contrast, "_",
                       model_info$y_var)
  
  write.csv(pooled_conv_stats, paste0(out_path, model_name, ".csv"), row.names = FALSE)
  
  
  
  
  
  return("Done")
}

# Create directory and run function

full_results_tbl_path <- "./results/bayesian/tables/full/"

dir.create(full_results_tbl_path, recursive = TRUE)

lapply(res_drp_c1_2000bs, create_full_tbl_a1, drp_param_labels, full_results_tbl_path)
lapply(res_eff_c1_2000bs, create_full_tbl_a1, eff_param_labels, full_results_tbl_path)

# ---------------------------------------------------------------------------- #
# Create full tables for "a2" models ----
# ---------------------------------------------------------------------------- #

# Define function to create tables with full results for "a2" models

create_full_tbl_a2 <- function(results, param_labels, out_path) {
  # Combine statistics in data frame
  
  stats <- data.frame(mean      = results$summary$statistics[, "Mean"],
                      emp_sd    = results$summary$statistics[, "SD"],
                      hpd_ci_ll = results$hpd_interval[, "lower"],
                      hpd_ci_ul = results$hpd_interval[, "upper"],
                      geweke_z  = results$geweke$z)
  
  # Add parameter labels
  
  stats <- cbind(param = rownames(stats), stats)
  
  stats <- merge(stats, param_labels, "param", all.x = TRUE, sort = FALSE)
  
  # Identify significant parameters
  
  stats$sig <- NA
  
  stats$sig <- ifelse(stats$hpd_ci_ll > 0 | stats$hpd_ci_ul < 0, 1, 0)
  
  # Round selected columns
  
  target_cols <- c("mean", "emp_sd", "hpd_ci_ll", "hpd_ci_ul", "geweke_z")
  
  stats[, target_cols] <- round(stats[, target_cols], 2)
  
  # Format CIs
  
  stats$hpd_ci <- paste0("[", stats$hpd_ci_ll, ", ", stats$hpd_ci_ul, "]")
  
  # Remove redundant columns and rearrange
  
  rm_cols <- c("hpd_ci_ll", "hpd_ci_ul")
  
  stats <- stats[, !(names(stats) %in% rm_cols)]
  
  stats <- stats[, c("param", "model", "label", 
                     "mean", "emp_sd", "hpd_ci", "geweke_z", "sig")]
  
  # TODO: Write to one PDF with multiple pages (for now export to CSV)
  
  model_name <- paste0(results$model_info$analysis_type, "_",
                       results$model_info$analysis_sample, "_",
                       results$model_info$a_contrast, "_",
                       results$model_info$y_var)
  
  write.csv(stats, paste0(out_path, model_name, ".csv"), row.names = FALSE)
  
  
  
  
  
  return("Done")
}

# Run function (use same directory used for "a1" above)

lapply(res_drp_c2_4_1000000iter, create_full_tbl_a2, drp_param_labels, full_results_tbl_path)
lapply(res_eff_c2_4_1000000iter, create_full_tbl_a2, eff_param_labels, full_results_tbl_path)

# ---------------------------------------------------------------------------- #
# TODO: Create summary table for "a1" models ----
# ---------------------------------------------------------------------------- #





# ---------------------------------------------------------------------------- #
# TODO: Create summary table for "a2" models ----
# ---------------------------------------------------------------------------- #





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

# Load packages

pkgs <- c("flextable", "officer")
groundhog.library(pkgs, groundhog_day)

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
# Create full tables ----
# ---------------------------------------------------------------------------- #

# Define function to create tables with full results

create_full_tbl <- function(results, param_labels, a_contrast_type) {
  # Extract model info and stats
  
  model_info <- results$model_info
  
  if (a_contrast_type == "a1") {
    full_tbl <- results$pooled_conv_stats
  } else if (a_contrast_type == "a2") {
    full_tbl <- data.frame(mean      = results$summary$statistics[, "Mean"],
                           emp_sd    = results$summary$statistics[, "SD"],
                           hpd_ci_ll = results$hpd_interval[, "lower"],
                           hpd_ci_ul = results$hpd_interval[, "upper"],
                           geweke_z  = results$geweke$z)
  }
  
  # Add model info
  
  full_tbl$analysis_type   <- model_info$analysis_type
  full_tbl$analysis_sample <- model_info$analysis_sample
  full_tbl$y_var           <- model_info$y_var
  full_tbl$a_contrast      <- model_info$a_contrast
  
  # Add parameter labels
  
  full_tbl <- cbind(param = rownames(full_tbl), full_tbl)
  
  full_tbl <- merge(full_tbl, param_labels, "param", all.x = TRUE, sort = FALSE)
  
  # Identify significant parameters
  
  full_tbl$sig <- NA
  
  if (a_contrast_type == "a1") {
    full_tbl$sig <- ifelse(full_tbl$pctl_bs_ci_ll > 0 | 
                             full_tbl$pctl_bs_ci_ul < 0, 1, 0)
  } else if (a_contrast_type == "a2") {
    full_tbl$sig <- ifelse(full_tbl$hpd_ci_ll > 0 | 
                             full_tbl$hpd_ci_ul < 0, 1, 0)
  }
  
  # Round selected columns
  
  if (a_contrast_type == "a1") {
    target_cols <- c("mean", "pctl_bs_ci_ll", "pctl_bs_ci_ul", "emp_sd", "avg_sd")
  } else if (a_contrast_type == "a2") {
    target_cols <- c("mean", "emp_sd", "hpd_ci_ll", "hpd_ci_ul", "geweke_z")
  }
  
  full_tbl[, target_cols] <- round(full_tbl[, target_cols], 2)

  # Format CIs
  
  if (a_contrast_type == "a1") {
    full_tbl$pctl_bs_ci <- paste0("[", full_tbl$pctl_bs_ci_ll, ", ",
                                       full_tbl$pctl_bs_ci_ul, "]")
  } else if (a_contrast_type == "a2") {
    full_tbl$hpd_ci     <- paste0("[", full_tbl$hpd_ci_ll, ", ",
                                       full_tbl$hpd_ci_ul, "]")
  }
  
  # Remove redundant columns
  
  if (a_contrast_type == "a1") {
    rm_cols <- c("pctl_bs_ci_ll", "pctl_bs_ci_ul")
  } else if (a_contrast_type == "a2") {
    rm_cols <- c("hpd_ci_ll", "hpd_ci_ul")
  }
  
  full_tbl <- full_tbl[, !(names(full_tbl) %in% rm_cols)]
  
  # Rearrange columns
  
  common_cols <- c("analysis_type", "analysis_sample", "y_var", "a_contrast",
                   "param", "model", "label")
  
  if (a_contrast_type == "a1") {
    full_tbl <- full_tbl[, c(common_cols, "num_converge_all", "mean", "emp_sd",
                             "avg_sd", "pctl_bs_ci", "sig")]
  } else if (a_contrast_type == "a2") {
    full_tbl <- full_tbl[, c(common_cols, "mean", "emp_sd", 
                             "hpd_ci", "geweke_z", "sig")]
  }
  
  # Add to list
  
  results$full_tbl <- full_tbl
  
  return(results)
}

# Run function

res_drp_c1_2000bs <- lapply(res_drp_c1_2000bs, create_full_tbl, drp_param_labels, "a1")
res_eff_c1_2000bs <- lapply(res_eff_c1_2000bs, create_full_tbl, eff_param_labels, "a1")

res_drp_c2_4_1000000iter <- lapply(res_drp_c2_4_1000000iter, create_full_tbl, drp_param_labels, "a2")
res_eff_c2_4_1000000iter <- lapply(res_eff_c2_4_1000000iter, create_full_tbl, eff_param_labels, "a2")

# ---------------------------------------------------------------------------- #
# Write full tables ----
# ---------------------------------------------------------------------------- #

write_full_tbl <- function(results, out_path) {
  # Extract model info and full table
  
  model_info <- results$model_info
  full_tbl   <- results$full_tbl
  
  # Remove columns
  
  rm_cols <- c("analysis_type", "analysis_sample", "y_var", "a_contrast")
  
  full_tbl <- full_tbl[, !(names(full_tbl) %in% rm_cols)]
  
  # TODO: Write to one PDF with multiple pages (for now export to CSV)
  
  
  
  model_name <- paste0(model_info$analysis_type, "_",
                       model_info$analysis_sample, "_",
                       model_info$y_var, "_",
                       model_info$a_contrast)
  
  write.csv(full_tbl, paste0(out_path, model_name, ".csv"), row.names = FALSE)
  
  return("Done")
}

# Run function

full_results_tbl_path <- "./results/bayesian/tables/full/"

dir.create(full_results_tbl_path, recursive = TRUE)

lapply(res_drp_c1_2000bs, write_full_tbl, full_results_tbl_path)
lapply(res_eff_c1_2000bs, write_full_tbl, full_results_tbl_path)

lapply(res_drp_c2_4_1000000iter, write_full_tbl, full_results_tbl_path)
lapply(res_eff_c2_4_1000000iter, write_full_tbl, full_results_tbl_path)

# ---------------------------------------------------------------------------- #
# Create summary tables ----
# ---------------------------------------------------------------------------- #

# Define function to create summary table

create_summ_tbl <- function(results_list, analysis_type, analysis_sample) {
  # Put all results in one data frame
  
  full_tbl_list <- lapply(results_list, function(x) x$full_tbl)
  
  summ_tbl <- do.call(rbind, full_tbl_list)
  row.names(summ_tbl) <- 1:nrow(summ_tbl)
  
  # Restrict to desired rows for summary
  
  if (analysis_type == "efficacy") {
    target_params <- c("para[1]", "para[2]", "para[3]", "para[4]")
  } else if (analysis_type == "dropout") {
    target_params <- c("para[1]", "para[3]")
  }
  
  summ_tbl <- summ_tbl[summ_tbl$param %in% target_params, ]
  
  # Sort table (note: alphabetical order reflects desired order for "a_contrast")
  
  if (analysis_type == "efficacy") {
    analysis_sample_order <- c("c1_corr_itt_2000", "c1_corr_s5_train_compl_2000")
    y_var_order           <- c("rr_pos_threat_m", "rr_neg_threat_m", "bbsiq_ben_m",
                               "bbsiq_neg_m", "oa_m", "dass21_as_m")
    param_order           <- c("para[3]", "para[1]", "para[4]", "para[2]")
    
    summ_tbl <- summ_tbl[order(match(summ_tbl$analysis_sample, analysis_sample_order),
                               match(summ_tbl$y_var, y_var_order),
                               summ_tbl$a_contrast,
                               match(summ_tbl$param, param_order)), ]
  } else if (analysis_type == "dropout") {
    param_order           <- c("para[3]", "para[1]")
    
    summ_tbl <- summ_tbl[order(summ_tbl$a_contrast,
                               match(summ_tbl$param, param_order)), ]
  }
  
  # Subset table for desired analysis sample
  
  if (analysis_type == "efficacy") {
    summ_tbl <- summ_tbl[summ_tbl$analysis_sample == analysis_sample, ]
  }
  
  return(summ_tbl)
}

# Run function

summ_tbl_drp_a1_itt              <- create_summ_tbl(res_drp_c1_2000bs, "dropout",  "c1_corr_itt_2000")
summ_tbl_eff_a1_itt              <- create_summ_tbl(res_eff_c1_2000bs, "efficacy", "c1_corr_itt_2000")
summ_tbl_eff_a1_s5_train_compl   <- create_summ_tbl(res_eff_c1_2000bs, "efficacy", "c1_corr_s5_train_compl_2000")

summ_tbl_drp_a2_class_meas_compl <- create_summ_tbl(res_drp_c2_4_1000000iter, "dropout",  "c2_4_class_meas_compl")
summ_tbl_eff_a2_class_meas_compl <- create_summ_tbl(res_eff_c2_4_1000000iter, "efficacy", "c2_4_class_meas_compl")
summ_tbl_eff_a2_s5_train_compl   <- create_summ_tbl(res_eff_c2_4_1000000iter, "efficacy", "c2_4_s5_train_compl")

# ---------------------------------------------------------------------------- #
# Format summary tables ----
# ---------------------------------------------------------------------------- #

# TODO (add header and footer): Define a function to format summary tables





set_flextable_defaults(font.size = 12, font.family = "Times New Roman", font.color = "black", 
                       border.width = 0.5, border.color = "black",
                       padding.bottom = 0, padding.top = 0, 
                       padding.left = 0.08, padding.right = 0.08,
                       line_spacing = 1)

target_cols <- c("y_var", "a_contrast", "param", "mean", "emp_sd", "avg_sd", "pctl_bs_ci")

summ_tbl_eff_a1_itt_ft <- flextable(data = summ_tbl_eff_a1_itt[, target_cols]) |>
  set_table_properties(align = "left") |>
  align(align = "center", part = "header") |>
  align(align = "center", part = "body") |>
  align(j = 1:3, align = "left", part = "body") |>
  bold(which(summ_tbl_eff_a1_itt$sig == 1), "pctl_bs_ci") |>
  labelizor(part = "header",
            labels = c("y_var"           = "Outcome",
                       "a_contrast"      = "Contrast",
                       "param"           = "Estimand",
                       "mean"            = "Empirical M",
                       "emp_sd"          = "Empirical SD",
                       "avg_sd"          = "Average SD",
                       "pctl_bs_ci"      = "95% CI")) |>
  labelizor(part = "body",
            labels = c("rr_pos_threat_m" = "Positive Bias (RR)",
                       "rr_neg_threat_m" = "Negative Bias (RR)",
                       "bbsiq_ben_m"     = "Benign Bias (BBSIQ)",
                       "bbsiq_neg_m"     = "Negative Bias (BBSIQ)",
                       "oa_m"            = "Anxiety (OASIS)",
                       "dass21_as_m"     = "Anxiety (DASS-21-AS)",
                       "a1"              = "CBM-I vs. Psychoed.",
                       "a2_1"            = "CBM-I HR Coaching vs. No Coaching",
                       "a2_2"            = "CBM-I HR Coaching vs. CBM-I LR",
                       "a2_3"            = "CBM-I HR No Coaching vs. CBM-I LR",
                       "para[3]"         = "Slope difference during TX",
                       "para[1]"         = "Mean difference at Session 5",
                       "para[4]"         = "Slope difference during FU",
                       "para[2]"         = "Mean difference at FU")) |>
  autofit()

sect_properties <- prop_section(page_size = page_size(orient = "landscape",
                                                      width = 8.5, height = 11),
                                type = "continuous")
my_rtf <- rtf_doc(def_sec = sect_properties)
my_rtf <- rtf_add(my_rtf, summ_tbl_eff_a1_itt_ft)

summ_tbl_path <- "./results/bayesian/tables/summ/"

dir.create(summ_tbl_path, recursive = TRUE)

print(my_rtf, target = paste0(summ_tbl_path, "summ_tbl_eff_a1_itt_ft.rtf"))





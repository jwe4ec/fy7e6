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

groundhog_day <- version_control_tables()

# Load packages

pkgs <- c("flextable", "officer", "ftExtra")
groundhog.library(pkgs, groundhog_day, tolerate.R.version = "4.1.2")

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
  
  # Round selected columns and ensure two decimal digits are printed
  
  if (a_contrast_type == "a1") {
    target_cols <- c("mean", "pctl_bs_ci_ll", "pctl_bs_ci_ul", "emp_sd", "avg_sd")
  } else if (a_contrast_type == "a2") {
    target_cols <- c("mean", "emp_sd", "hpd_ci_ll", "hpd_ci_ul", "geweke_z")
  }

  full_tbl[, target_cols] <- format(round(full_tbl[, target_cols], 2), 
                                    nsmall = 2, trim = TRUE)
  
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
  
  # Create trimmed results list
  
  results_trim <- list(model_info = results$model_info,
                       full_tbl   = full_tbl)
  
  return(results_trim)
}

# Run function

res_trm_drp_c1_2000bs <- lapply(res_drp_c1_2000bs, create_full_tbl, drp_param_labels, "a1")
res_trm_eff_c1_2000bs <- lapply(res_eff_c1_2000bs, create_full_tbl, eff_param_labels, "a1")

res_trm_drp_c2_4_1000000iter <- lapply(res_drp_c2_4_1000000iter, create_full_tbl, drp_param_labels, "a2")
res_trm_eff_c2_4_1000000iter <- lapply(res_eff_c2_4_1000000iter, create_full_tbl, eff_param_labels, "a2")

# Remove large results objects from environment

rm(res_drp_c1_2000bs)
rm(res_eff_c1_2000bs)

rm(res_drp_c2_4_1000000iter)
rm(res_eff_c2_4_1000000iter)

# ---------------------------------------------------------------------------- #
# Set "flextable" defaults and define section and text properties ----
# ---------------------------------------------------------------------------- #

# Set defaults for "flextable" package (mimic those of MS Word)

set_flextable_defaults(font.size = 12, font.family = "Times New Roman", font.color = "black", 
                       border.width = 0.5, border.color = "black",
                       padding.bottom = 0, padding.top = 0, 
                       padding.left = 0.08, padding.right = 0.08,
                       line_spacing = 1)

# Define section and text properties

psect_prop <- prop_section(page_size(orient = "portrait", width = 8.5, height = 11),
                           type = "nextPage")
lsect_prop <- prop_section(page_size(orient = "landscape", width = 8.5, height = 11),
                           type = "nextPage")
text_prop <- fp_text_lite(color = "black", font.size = 12, font.family = "Times New Roman")

# ---------------------------------------------------------------------------- #
# Format full tables ----
# ---------------------------------------------------------------------------- #

# Define function to format full tables

format_full_tbl <- function(results_trim, a_contrast_type, gen_note) {
  # Extract model info and full table
  
  analysis_type   <- results_trim$model_info$analysis_type
  analysis_sample <- results_trim$model_info$analysis_sample
  y_var           <- results_trim$model_info$y_var
  a_contrast      <- results_trim$model_info$a_contrast
  
  full_tbl <- results_trim$full_tbl
  
  # Create labels for title
  
  if (analysis_type == "efficacy") {
    analysis_type_label <- "Efficacy"
  } else if (analysis_type == "dropout") {
    analysis_type_label <- "Dropout"
  }
  
  if (a_contrast_type == "a1") {
    stage_number <- "1"
  } else if (a_contrast_type == "a2") {
    stage_number <- "2"
  }
  
  if (analysis_sample == "c1_corr_itt_2000") {
    analysis_sample_label <- "Intent-To-Treat"
  } else if (analysis_sample == "c2_4_class_meas_compl") {
    analysis_sample_label <- "Classif. Measure Compl."
  } else if (analysis_sample %in% c("c1_corr_s5_train_compl_2000", "c2_4_s5_train_compl")) {
    analysis_sample_label <- "Session 5 Training Compl."
  }
  
  if (y_var == "rr_pos_threat_m") {
    y_var_label <- "Positive Bias (RR)"
  } else if (y_var == "rr_neg_threat_m") {
    y_var_label <- "Negative Bias (RR)"
  } else if (y_var == "bbsiq_ben_m") {
    y_var_label <- "Benign Bias (BBSIQ)"
  } else if (y_var == "bbsiq_neg_m") {
    y_var_label <- "Negative Bias (BBSIQ)"
  } else if (y_var == "oa_m") {
    y_var_label <- "Anxiety (OASIS)"
  } else if (y_var == "dass21_as_m") {
    y_var_label <- "Anxiety (DASS-21-AS)"
  }
  
  if (a_contrast == "a1") {
    a_contrast_label <- "CBM-I vs. Psychoed."
  } else if (a_contrast == "a2_1") {
    a_contrast_label <- "CBM-I HR Coaching vs. No Coaching"
  } else if (a_contrast == "a2_2") {
    a_contrast_label <- "CBM-I HR Coaching vs. CBM-I LR"
  } else if (a_contrast == "a2_3") {
    a_contrast_label <- "CBM-I HR No Coaching vs. CBM-I LR"
  }
  
  # Create title
  
  if (analysis_type == "efficacy") {
    title <- paste0("Stage ", stage_number, " (", a_contrast_label, ") ",
                    analysis_type_label, " Results for ", y_var_label, 
                    " for ", analysis_sample_label, " Sample")
  } else if (analysis_type == "dropout") {
    title <- paste0("Stage ", stage_number, " (", a_contrast_label, ") ",
                    analysis_type_label, " Results for ", 
                    analysis_sample_label, " Sample")
  }
  
  # Format table
  
  left_align_body_cols <- c("param", "label")

  if (a_contrast_type == "a1") {
    target_cols <- c("model", "param", "label", "mean", "emp_sd", "avg_sd",
                     "pctl_bs_ci", "num_converge_all")
    col_to_bold <- "pctl_bs_ci"
  } else if (a_contrast_type == "a2") {
    target_cols <- c("model", "param", "label", "mean", "emp_sd",
                     "hpd_ci", "geweke_z")
    col_to_bold <- "hpd_ci"
  }
  
  # Restructure data as grouped data for table spanners based on "model"
  
  full_tbl <- as_grouped_data(full_tbl[, target_cols], groups = "model",
                              columns = target_cols[!(target_cols %in% "model")])
  
  # Create flextable
  
  full_tbl_ft <- as_flextable(full_tbl) |>
    set_table_properties(align = "left") |>
    
    set_caption(as_paragraph(as_i(title)), word_stylename = "heading 1",
                fp_p = fp_par(padding.left = 0, padding.right = 0),
                align_with_table = FALSE) |>
    
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    align(j = left_align_body_cols, align = "left", part = "body") |>
    align(align = "left", part = "footer") |>
    
    align(j = 1, i = ~ !is.na(model), align = "center", part = "body") |>
    hline(j = 1, i = ~ !is.na(model), part = "body") |>
    bold(j = 1,  i = ~ !is.na(model), part = "body") |>
  
    bold(which(full_tbl$sig == 1), col_to_bold) |>
    
    set_header_labels(param                = "Parameter",
                      label                = "Description") |>
    compose(j = "mean", part = "header",
            value = as_paragraph("Emp. ", as_i("M"))) |>
    compose(j = "emp_sd", part = "header",
            value = as_paragraph("Emp. ", as_i("SD"))) |>
    
    add_footer_lines(gen_note)
  
  if (a_contrast_type == "a1") {
    full_tbl_ft <- full_tbl_ft |>
      set_header_labels(pctl_bs_ci         = "95% PB CI",
                        num_converge_all   = "No. Samples") |>
      compose(j = "avg_sd", part = "header",
              value = as_paragraph("Avg. ", as_i("SD")))
  } else if (a_contrast_type == "a2") {
    full_tbl_ft <- full_tbl_ft |>
      set_header_labels(hpd_ci             = "95% HPD CI") |>
      compose(j = "geweke_z", part = "header",
              value = as_paragraph("Geweke's ", as_i("z")))
  }
  
  full_tbl_ft <- full_tbl_ft |>
    autofit()
  
  # Add to list
  
  results_trim$full_tbl_ft <- full_tbl_ft
  
  return(results_trim)
}

# Define general notes

full_a1_base_str <- "*Note.* Significant parameters are in boldface (only %s were interpreted). The latter level of the contrast is the reference group. Results were pooled across (No. Samples) bootstrap samples (out of 2,000) in which all parameters converged (Geweke's *z* in [-1.96, 1.96]). The posterior distribution in each bootstrap sample was based on 10,000 MCMC sampling iterations (after 10,000 burn-in iterations). CBM-I = cognitive bias modification for interpretation; Emp. *M* = *M* of empirical *M*s across bootstrap samples; Emp. *SD* = *SD* of empirical *M*s across bootstrap samples; Avg. *SD* = *M* of empirical *SD*s across bootstrap samples; PB CI = percentile bootstrap confidence interval"

gen_note_drp_a1 <- as_paragraph_md(paste0(sprintf(full_a1_base_str, "para[1] and para[3]"), 
  "; OR = odds ratio."))
gen_note_eff_a1 <- as_paragraph_md(paste0(sprintf(full_a1_base_str, "para[1] through para[4]"), 
  "; S5 = Session 5; FU = follow-up; RI = random intercept; RS = random slope."))

full_a2_base_str <- "*Note.* Significant parameters are in boldface (only %s were interpreted). The latter level of the contrast is the reference group. The posterior distribution was based on 500,000 MCMC sampling iterations (after 500,000 burn-in iterations). CBM-I = cognitive bias modification for interpretation; HR = High Risk; LR = Low Risk; HPD CI = Highest Posterior Density Credible Interval"

gen_note_drp_a2 <- as_paragraph_md(paste0(sprintf(full_a2_base_str, "para[1] and para[3]"),
  "; OR = odds ratio."))
gen_note_eff_a2 <- as_paragraph_md(paste0(sprintf(full_a2_base_str, "para[1] through para[4]"),
  "; S5 = Session 5; FU = follow-up; RI = random intercept; RS = random slope."))

# Run function

res_trm_drp_c1_2000bs <- lapply(res_trm_drp_c1_2000bs, format_full_tbl, "a1", gen_note_drp_a1)
res_trm_eff_c1_2000bs <- lapply(res_trm_eff_c1_2000bs, format_full_tbl, "a1", gen_note_eff_a1)

res_trm_drp_c2_4_1000000iter <- lapply(res_trm_drp_c2_4_1000000iter, format_full_tbl, "a2", gen_note_drp_a2)
res_trm_eff_c2_4_1000000iter <- lapply(res_trm_eff_c2_4_1000000iter, format_full_tbl, "a2", gen_note_eff_a2)

# ---------------------------------------------------------------------------- #
# Write full tables to MS Word ----
# ---------------------------------------------------------------------------- #

# Write full tables (note: for some reason a blank page shows at end of doc)

# Order list so full tables match the order of summary tables below

analysis_type_order   <- c("efficacy", "dropout")
analysis_sample_order <- c("c1_corr_itt_2000", "c2_4_class_meas_compl", 
                           "c1_corr_s5_train_compl_2000", "c2_4_s5_train_compl")
y_var_order           <- c("rr_pos_threat_m", "rr_neg_threat_m", "bbsiq_ben_m",
                           "bbsiq_neg_m", "oa_m", "dass21_as_m")
a_contrast_order      <- c("a1", "a2_1", "a2_2", "a2_3")

res_trm_all <- c(res_trm_eff_c1_2000bs,
                 res_trm_eff_c2_4_1000000iter,
                 res_trm_drp_c1_2000bs,
                 res_trm_drp_c2_4_1000000iter)

res_trm_all <- res_trm_all[order(sapply(res_trm_all, function(x) match(x$model_info$analysis_type,
                                                                       analysis_type_order)),
                                 sapply(res_trm_all, function(x) match(x$model_info$analysis_sample,
                                                                       analysis_sample_order)),
                                 sapply(res_trm_all, function(x) match(x$model_info$y_var,
                                                                       y_var_order)),
                                 sapply(res_trm_all, function(x) match(x$model_info$a_contrast,
                                                                       a_contrast_order)))]

full_tbl_ft_list <- lapply(res_trm_all, function(x) x$full_tbl_ft)

# Write tables

full_tbl_orientations <- rep("l", length(full_tbl_ft_list))
full_tbl_numbers <- paste0("SB", 1:length(full_tbl_ft_list))

doc <- read_docx()
doc <- body_set_default_section(doc, psect_prop)

# TODO: Fix TOC (see https://github.com/davidgohel/flextable/discussions/519)

doc <- body_add_toc(doc, style = "heading 1")





for (i in 1:length(full_tbl_ft_list)) {
  doc <- body_add_fpar(doc, fpar(ftext(paste0("Table ", full_tbl_numbers[[i]]),
                                       prop = update(text_prop, bold = TRUE))))
  doc <- body_add_par(doc, "")
  
  doc <- body_add_flextable(doc, full_tbl_ft_list[[i]], align = "left")

  doc <- body_end_block_section(doc, block_section(lsect_prop))
}

full_tbl_path <- "./results/bayesian/tables/full/"

dir.create(full_tbl_path, recursive = TRUE)

print(doc, target = paste0(full_tbl_path, "full_tbls.docx"))

# ---------------------------------------------------------------------------- #
# Create summary tables ----
# ---------------------------------------------------------------------------- #

# Define function to create summary table

create_summ_tbl <- function(results_trim_list, analysis_type, analysis_sample) {
  # Put all results in one data frame
  
  full_tbl_list <- lapply(results_trim_list, function(x) x$full_tbl)
  
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

summ_tbl_drp_a1_itt              <- create_summ_tbl(res_trm_drp_c1_2000bs, "dropout",  "c1_corr_itt_2000")
summ_tbl_eff_a1_itt              <- create_summ_tbl(res_trm_eff_c1_2000bs, "efficacy", "c1_corr_itt_2000")
summ_tbl_eff_a1_s5_train_compl   <- create_summ_tbl(res_trm_eff_c1_2000bs, "efficacy", "c1_corr_s5_train_compl_2000")

summ_tbl_drp_a2_class_meas_compl <- create_summ_tbl(res_trm_drp_c2_4_1000000iter, "dropout",  "c2_4_class_meas_compl")
summ_tbl_eff_a2_class_meas_compl <- create_summ_tbl(res_trm_eff_c2_4_1000000iter, "efficacy", "c2_4_class_meas_compl")
summ_tbl_eff_a2_s5_train_compl   <- create_summ_tbl(res_trm_eff_c2_4_1000000iter, "efficacy", "c2_4_s5_train_compl")

# ---------------------------------------------------------------------------- #
# Format efficacy summary tables ----
# ---------------------------------------------------------------------------- #

# Define function to format efficacy summary tables

format_summ_tbl_eff <- function(summ_tbl, a_contrast_type, gen_note, title) {
  if (a_contrast_type == "a1") {
    target_cols <- c("y_var", "param", "mean", "emp_sd", "avg_sd", "pctl_bs_ci")
    left_align_body_cols <- c("y_var", "param")
    merge_v_cols         <- "y_var"
    col_to_bold          <- "pctl_bs_ci"
  } else if (a_contrast_type == "a2") {
    target_cols <- c("y_var", "a_contrast", "param", "mean", "emp_sd", "hpd_ci")
    left_align_body_cols <- c("y_var", "a_contrast", "param")
    merge_v_cols         <- c("y_var", "a_contrast")
    col_to_bold          <- "hpd_ci"
  }
  
  # Create flextable
  
  summ_tbl_ft <- flextable(data = summ_tbl[, target_cols]) |>
    set_table_properties(align = "left") |>
    
    set_caption(as_paragraph(as_i(title)),
                fp_p = fp_par(padding.left = 0, padding.right = 0),
                align_with_table = FALSE) |>
    
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    align(j = left_align_body_cols, align = "left", part = "body") |>
    align(align = "left", part = "footer") |>
    
    merge_v(j = merge_v_cols, part = "body") |>
    valign(j = merge_v_cols, valign = "top", part = "body") |>
    fix_border_issues(part = "body") |>
    
    bold(which(summ_tbl$sig == 1), col_to_bold) |>
    
    set_header_labels(y_var                = "Outcome",
                      param                = "Estimand") |>
    compose(j = "mean", part = "header",
            value = as_paragraph("Emp. ", as_i("M"))) |>
    compose(j = "emp_sd", part = "header",
            value = as_paragraph("Emp. ", as_i("SD"))) |>
    
    labelizor(part = "body",
              labels = c("rr_pos_threat_m" = "Positive Bias (RR)",
                         "rr_neg_threat_m" = "Negative Bias (RR)",
                         "bbsiq_ben_m"     = "Benign Bias (BBSIQ)",
                         "bbsiq_neg_m"     = "Negative Bias (BBSIQ)",
                         "oa_m"            = "Anxiety (OASIS)",
                         "dass21_as_m"     = "Anxiety (DASS-21-AS)",
                         "para[3]"         = "Slope during TX",
                         "para[1]"         = "Mean at Session 5",
                         "para[4]"         = "Slope during FU",
                         "para[2]"         = "Mean at FU"))
    
  if (a_contrast_type == "a1") {
    summ_tbl_ft <- summ_tbl_ft |>
      set_header_labels(pctl_bs_ci         = "95% PB CI") |>
      compose(j = "avg_sd", part = "header",
              value = as_paragraph("Avg. ", as_i("SD")))
  } else if (a_contrast_type == "a2") {
    summ_tbl_ft <- summ_tbl_ft |>
      set_header_labels(a_contrast         = "Contrast",
                        hpd_ci             = "95% HPD CI") |>
    
      labelizor(part = "body",
                labels = c("a2_1"          = "CBM-I HR Coaching vs. No Coaching",
                           "a2_2"          = "CBM-I HR Coaching vs. CBM-I LR",
                           "a2_3"          = "CBM-I HR No Coaching vs. CBM-I LR"))
  }
  
  summ_tbl_ft <- summ_tbl_ft |>
    add_footer_lines(gen_note) |>
    
    autofit()
  
  return(summ_tbl_ft)
}

# Define general notes

summ_base_str <- "*Note.* Significant differences between contrast levels are in boldface. Separate models were fit for each %s. The latter level of the contrast is the reference group.%s CBM-I = cognitive bias modification for interpretation; %s; TX = treatment; FU = follow-up; RR = Recognition Ratings; BBSIQ = Brief Body Sensations Interpretation Questionnaire; OASIS = Overall Anxiety Severity and Impairment Scale; DASS-21-AS = Anxiety Subscale of Depression Anxiety Stress Scales."

gen_note_a1 <- as_paragraph_md(paste0(sprintf(summ_base_str, "outcome", 
  " Results were pooled across bootstrap samples in which all parameters converged. Only estimands of interest are shown (for all model parameters and number of samples in which all parameters converged, see Supplement B). The posterior distribution in each bootstrap sample was based on 10,000 MCMC sampling iterations (after 10,000 burn-in iterations).",
  "Emp. *M* = *M* of empirical *M*s across bootstrap samples; Emp. *SD* = *SD* of empirical *M*s across bootstrap samples; Avg. *SD* = *M* of empirical *SD*s across bootstrap samples; PB CI = percentile bootstrap confidence interval")))

gen_note_a2 <- as_paragraph_md(paste0(sprintf(summ_base_str, "outcome and contrast",
  " Only estimands of interest are shown (for all model parameters and convergence diagnostics, see Supplement B). The posterior distribution was based on 500,000 MCMC sampling iterations (after 500,000 burn-in iterations).",
  "HPD CI = Highest Posterior Density Credible Interval")))

# Run function

summ_tbl_eff_a1_itt_ft              <- format_summ_tbl_eff(summ_tbl_eff_a1_itt,              "a1", gen_note_a1,
  "Stage 1 (CBM-I vs. Psychoed.) Efficacy Results for Intent-To-Treat Sample")
summ_tbl_eff_a1_s5_train_compl_ft   <- format_summ_tbl_eff(summ_tbl_eff_a1_s5_train_compl,   "a1", gen_note_a1,
  "Stage 1 (CBM-I vs. Psychoed.) Efficacy Results for Session 5 Training Completer Sample")

summ_tbl_eff_a2_class_meas_compl_ft <- format_summ_tbl_eff(summ_tbl_eff_a2_class_meas_compl, "a2", gen_note_a2,
  "Stage 2 Efficacy Results for Classification Measure Completer Sample")
summ_tbl_eff_a2_s5_train_compl_ft   <- format_summ_tbl_eff(summ_tbl_eff_a2_s5_train_compl,   "a2", gen_note_a2,
  "Stage 2 Efficacy Results for Session 5 Training Completer Sample")

# ---------------------------------------------------------------------------- #
# Format dropout summary table ----
# ---------------------------------------------------------------------------- #

# Combine dropout summary tables

summ_tbl_drp <- merge(summ_tbl_drp_a1_itt, summ_tbl_drp_a2_class_meas_compl, 
                      all = TRUE, sort = FALSE)

# Define function to format dropout summary table

format_summ_tbl_drp <- function(summ_tbl, gen_note, footnotes, title) {
  target_cols <- c("analysis_sample", "a_contrast", "param", "mean", "emp_sd", 
                   "avg_sd", "pctl_bs_ci", "hpd_ci")
  left_align_body_cols <- c("analysis_sample", "a_contrast", "param")
  merge_v_cols         <- c("analysis_sample", "a_contrast")
  cols_to_bold         <- c("pctl_bs_ci", "hpd_ci")
  
  # Create flextable

  summ_tbl_ft <- flextable(data = summ_tbl[, target_cols]) |>
    set_table_properties(align = "left") |>
    
    set_caption(as_paragraph(as_i(title)),
                fp_p = fp_par(padding.left = 0, padding.right = 0),
                align_with_table = FALSE) |>
    
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    align(j = left_align_body_cols, align = "left", part = "body") |>
    align(align = "left", part = "footer") |>
    
    merge_v(j = merge_v_cols, part = "body") |>
    valign(j = merge_v_cols, valign = "top", part = "body") |>
    fix_border_issues(part = "body") |>
    
    bold(which(summ_tbl$sig == 1), cols_to_bold) |>
    
    set_header_labels(analysis_sample            = "Sample",
                      a_contrast                 = "Contrast",
                      param                      = "Estimand",
                      pctl_bs_ci                 = "95% PB CI",
                      hpd_ci                     = "95% HPD CI") |>
    compose(j = "mean", part = "header",
            value = as_paragraph("Emp. ", as_i("M"))) |>
    compose(j = "emp_sd", part = "header",
            value = as_paragraph("Emp. ", as_i("SD"))) |>
    compose(j = "avg_sd", part = "header",
            value = as_paragraph("Avg. ", as_i("SD"))) |>
    
    labelizor(part = "body",
              labels = c("c1_corr_itt_2000"      = "ITT",
                         "c2_4_class_meas_compl" = "CMC",
                         "a1"                    = "CBM-I vs. Psychoeducation",
                         "a2_1"                  = "CBM-I HR Coaching vs. No Coaching",
                         "a2_2"                  = "CBM-I HR Coaching vs. CBM-I LR",
                         "a2_3"                  = "CBM-I HR No Coaching vs. CBM-I LR",
                         "para[3]"               = "> 0 incompl. sessions (OR)",
                         "para[1]"               = "No. incomplete sessions")) |>
    
    add_footer_lines(gen_note) |>
    
    footnote(i = c(1, 3), j = 1,
             value = as_paragraph_md(c(footnotes$ITT_a, footnotes$CMC_b)),
             ref_symbols = c(" a", " b"),
             part = "body") |>
  
    autofit()
  
  return(summ_tbl_ft)
}

# Define general note

footnotes <- list(ITT_a = "\\ For the ITT model, results were pooled across bootstrap samples in which all parameters converged, and the posterior disribution in each bootstrap sample was based on 10,000 MCMC sampling iterations (after 10,000 burn-in iterations). Emp. *M* = *M* of empirical *M*s across bootstrap samples; Emp. *SD* = *SD* of empirical *M*s across bootstrap samples; Avg. *SD* = *M* of empirical *SD*s across bootstrap samples; PB CI = percentile bootstrap confidence interval.",
                  CMC_b = "\\ For the CMC models, the posterior distribution was based on 500,000 MCMC sampling iterations (after 500,000 burn-in iterations). Emp. *M* = empirical *M*; Emp. *SD* = empirical *SD*; HPD CI = Highest Posterior Density Credible Interval.")

gen_note <- as_paragraph_md("*Note.* The zero-inflation (logistic regression) model predicts whether a participant may (vs. will not) have > 0 incomplete sessions; the count (Poisson regression) model predicts number of incomplete sessions (0-5) for participants who may have > 0 incomplete sessions. Significant differences between contrast levels are in boldface. Separate models were fit for each contrast. The latter level of the contrast is the reference group. Only estimands of interest are shown (for all model parameters and convergence diagnostics, see Supplement B). CBM-I = cognitive bias modification for interpretation; HR = High Risk; LR = Low Risk; OR = odds ratio.")

# Run function

summ_tbl_drp_ft <- format_summ_tbl_drp(summ_tbl_drp, gen_note, footnotes,
  "Stages 1-2 Dropout Results for Intent-To-Treat (ITT) and Classification Measure Completer (CMC) Samples")

# ---------------------------------------------------------------------------- #
# Write summary tables to MS Word ----
# ---------------------------------------------------------------------------- #

# Write summary tables (Note: "flextable" seems to have a bug in which blank 
# page is at end of doc. Also, a border issue involving "merge_v_cols" needs 
# to be manually fixed after export to Word)

summ_tbls <- list(summ_tbl_eff_a1_itt_ft,
                  summ_tbl_eff_a2_class_meas_compl_ft,
                  summ_tbl_drp_ft,
                  summ_tbl_eff_a1_s5_train_compl_ft,
                  summ_tbl_eff_a2_s5_train_compl_ft)

summ_tbl_orientations <- c("p", "l", "l", "p", "l")
summ_tbl_numbers <- c("1", "2", "3", "S8", "S10")

doc <- read_docx()
doc <- body_set_default_section(doc, psect_prop)

for (i in 1:length(summ_tbls)) {
  doc <- body_add_fpar(doc, fpar(ftext(paste0("Table ", summ_tbl_numbers[[i]]),
                                       prop = update(text_prop, bold = TRUE))))
  doc <- body_add_par(doc, "")
  
  doc <- body_add_flextable(doc, summ_tbls[[i]], align = "left")
  
  if (summ_tbl_orientations[[i]] == "p") {
    doc <- body_end_block_section(doc, block_section(psect_prop))
  } else if (summ_tbl_orientations[[i]] == "l") {
    doc <- body_end_block_section(doc, block_section(lsect_prop))
  }
}

summ_tbl_path <- "./results/bayesian/tables/summ/"

dir.create(summ_tbl_path, recursive = TRUE)

print(doc, target = paste0(summ_tbl_path, "summ_tbls.docx"))

# ---------------------------------------------------------------------------- #
# DEPRECATED: Write full tables to CSV ----
# ---------------------------------------------------------------------------- #

# Write full tables to CSV files

# write_full_tbl <- function(results, out_path) {
#   # Extract model info and full table
#   
#   model_info <- results$model_info
#   full_tbl   <- results$full_tbl
#   
#   # Remove columns
#   
#   rm_cols <- c("analysis_type", "analysis_sample", "y_var", "a_contrast")
#   
#   full_tbl <- full_tbl[, !(names(full_tbl) %in% rm_cols)]
#   
#   # Write to CSV files
#   
#   model_name <- paste0(model_info$analysis_type, "_",
#                        model_info$analysis_sample, "_",
#                        model_info$y_var, "_",
#                        model_info$a_contrast)
#   
#   write.csv(full_tbl, paste0(out_path, model_name, ".csv"), row.names = FALSE)
#   
#   return("Done")
# }
# 
# # Run function
# 
# full_results_tbl_path <- "./results/bayesian/tables/full/"
# 
# dir.create(full_results_tbl_path, recursive = TRUE)
# 
# lapply(res_drp_c1_2000bs, write_full_tbl, full_results_tbl_path)
# lapply(res_eff_c1_2000bs, write_full_tbl, full_results_tbl_path)
# 
# lapply(res_drp_c2_4_1000000iter, write_full_tbl, full_results_tbl_path)
# lapply(res_eff_c2_4_1000000iter, write_full_tbl, full_results_tbl_path)

# ---------------------------------------------------------------------------- #
# DEPRECATED: Write summary tables to RTF ----
# ---------------------------------------------------------------------------- #

# Note: Code to export to RTF had issues with bottom border of header in tables 
# that spanned multiple pages (which could not then be edited in MS Word). The
# flextable captions were also not being exported, requiring a manual solution.
# The code is retained in case RTF export is improved in the future.

# summ_tbl_titles <- 
#   c("Stage 1 (CBM-I vs. Psychoed.) Efficacy Results for Intent-To-Treat Sample",
#     "Stage 1 (CBM-I vs. Psychoed.) Efficacy Results for Session 5 Training Completer Sample",
#     "Stage 2 Efficacy Results for Classification Measure Completer Sample",
#     "Stage 2 Efficacy Results for Session 5 Training Completer Sample")
# 
# my_rtf <- rtf_doc(def_sec = psect_prop)
# 
# for (i in 1:length(summ_tbls)) {
#   my_rtf <- rtf_add(my_rtf, fpar(ftext(paste0("Table ", summ_tbl_numbers[[i]]),
#                                        prop = update(text_prop, bold = TRUE))))
#   my_rtf <- rtf_add(my_rtf, "")
#   my_rtf <- rtf_add(my_rtf, fpar(ftext(summ_tbl_titles[[i]],
#                                        prop = update(text_prop, italic = TRUE))))
#   my_rtf <- rtf_add(my_rtf, summ_tbls[[i]])
#   
#   if (i < length(summ_tbls)) {
#     if (summ_tbl_orientations[[i + 1]] == "p") {
#       my_rtf <- rtf_add(my_rtf, block_section(psect_prop))
#     } else if (summ_tbl_orientations[[i + 1]] == "l") {
#       my_rtf <- rtf_add(my_rtf, block_section(lsect_prop))
#     }
#   }
# }
# 
# print(my_rtf, target = paste0(summ_tbl_path, "summ_tbls.rtf"))
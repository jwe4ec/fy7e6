# ---------------------------------------------------------------------------- #
# Create Plots
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

groundhog_day <- version_control_tables_plots()

# Load packages

pkgs <- c("ggplot2", "cowplot")

groundhog.library(pkgs, groundhog_day, tolerate.R.version = "4.1.2")

# ---------------------------------------------------------------------------- #
# Import results and parameter labels ----
# ---------------------------------------------------------------------------- #

load("./results/bayesian/pooled/w_marg_effects/res_trm_eff_c1_2000bs.RData")

# ---------------------------------------------------------------------------- #
# Structure predicted time-specific means by treatment arm for efficacy plots ----
# ---------------------------------------------------------------------------- #

# Define function to structure predicted means for plots of "a1" efficacy models

structure_pred_means <- function(results_trim) {
  # Extract predicted means from marginal effects table
  
  marg_tbl <- results_trim$marg_tbl
  
  pred_means <- marg_tbl[marg_tbl$param %in% c(paste0("marg[", 5:18, "]")), ]
  
  pred_means$a_contrast_level <- NA
  pred_means$a_contrast_level[pred_means$param %in% c(paste0("marg[", 5:11,  "]"))] <- 1
  pred_means$a_contrast_level[pred_means$param %in% c(paste0("marg[", 12:18, "]"))] <- -1
  
  assessments <- c("Baseline", paste0("Session ", 1:5), "Follow-Up")
  pred_means$assessment <- rep(assessments, times = 2)
  
  # Change "estimate" from character to numeric for ggplot2
  
  pred_means$estimate <- as.numeric(pred_means$estimate)

  # Code contrast level and assessment as factors for ggplot2
  
  if (results_trim$model_info$a_contrast == "a1") {
    arms <- c("CBM-I", "Psychoeducation")
  }
  
  pred_means$arm        <- factor(pred_means$a_contrast_level,
                                  levels = c(1, -1),
                                  labels = arms)
  pred_means$assessment <- factor(pred_means$assessment,
                                  levels = assessments)
  
  # Add to list
  
  results_trim$pred_means <- pred_means
  
  return(results_trim)
}

# Run function

res_trm_eff_c1_2000bs <- lapply(res_trm_eff_c1_2000bs, structure_pred_means)

# ---------------------------------------------------------------------------- #
# Plot predicted time-specific means by treatment arm for efficacy models ----
# ---------------------------------------------------------------------------- #

# Define function to plot predicted means for "a1" efficacy models. Colors obtained
# from Color Brewer 2.0 three-class Dark2 palette
# (https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3). Checked for
# vision deficiency using HCL Wizard (http://hclwizard.org:3000/cvdemulator/).

create_plot <- function(results_trim) {
  # Extract predicted means and define plot properties
  
  pred_means <- results_trim$pred_means
  
  assessments_without_measure <- c(paste0("Session ", c(1, 2, 4)))
  
  if (results_trim$model_info$y_var == "rr_pos_threat_m") {
    hidden_pts      <- assessments_without_measure
    title           <- "Positive Bias (RR)"
    scale_min       <- 1
    scale_max       <- 4
    legend_position <- c(0.8, 0.2)
  } else if (results_trim$model_info$y_var == "rr_neg_threat_m") {
    hidden_pts      <- assessments_without_measure
    title           <- "Negative Bias (RR)"
    scale_min       <- 1
    scale_max       <- 4
    legend_position <- c(0.8, 0.2)
  } else if (results_trim$model_info$y_var == "bbsiq_ben_m") {
    hidden_pts      <- assessments_without_measure
    title           <- "Benign Bias (BBSIQ)"
    scale_min       <- 0
    scale_max       <- 4
    legend_position <- c(0.8, 0.2)
  } else if (results_trim$model_info$y_var == "bbsiq_neg_m") {
    hidden_pts      <- assessments_without_measure
    title           <- "Negative Bias (BBSIQ)"
    scale_min       <- 0
    scale_max       <- 4
    legend_position <- c(0.8, 0.8)
  } else if (results_trim$model_info$y_var == "oa_m") {
    hidden_pts      <- NULL
    title           <- "Anxiety (OASIS)"
    scale_min       <- 0
    scale_max       <- 4
    legend_position <- c(0.8, 0.8)
  } else if (results_trim$model_info$y_var == "dass21_as_m") {
    hidden_pts      <- assessments_without_measure
    title           <- "Anxiety (DASS-21-AS)"
    scale_min       <- 0
    scale_max       <- 3
    legend_position <- c(0.8, 0.8)
  }
  
  # Create plot
  
  pred_means_plot <- ggplot(pred_means,
                            aes(x = assessment, y = estimate, 
                            group = arm, color = arm, linetype = arm)) +
    geom_line() +
    geom_point(data = pred_means[!(pred_means$assessment %in% hidden_pts), ]) +
    # geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se),
    #               pred_means[!(pred_means$assessment %in% hidden_pts), ],
    #               width = .3) +
    labs(title = title, 
         x = "Assessment",
         y = "Average Item Score") +
    scale_linetype_manual(name   = "Contrast Level",
                          values = c("CBM-I"           = "solid",
                                     "Psychoeducation" = "longdash")) +
    scale_color_manual(name      = "Contrast Level",
                       values    = c("CBM-I"           = "#1b9e77", 
                                     "Psychoeducation" = "#d95f02")) + 
    scale_y_continuous(breaks = scale_min:scale_max,
                       limits = c(scale_min, scale_max)) +
    theme_classic() +
    theme(plot.title   = element_text(hjust = 0.5),
          legend.title = element_text(size = 14),
          legend.text  = element_text(size = 11),
          legend.key.width = unit(2, "cm")) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(legend.position = legend_position)
  
  # Add to list
  
  results_trim$pred_means_plot <- pred_means_plot
  
  return(results_trim)
}

# Run function

res_trm_eff_c1_2000bs <- lapply(res_trm_eff_c1_2000bs, create_plot)

# Define function to arrange plots for ITT and Session 5 training completer samples

arrange_plots <- function(results_trim_list, analysis_sample) {
  # Extract predicted means plots and restrict to analysis sample
  
  pred_means_plot_list <- lapply(results_trim_list, function(x) x$pred_means_plot)
  
  pred_means_plot_list <- pred_means_plot_list[grepl(analysis_sample, 
                                               names(pred_means_plot_list))]
  
  p_rr_pos_threat_m <- pred_means_plot_list[grepl("rr_pos_threat_m", names(pred_means_plot_list))][[1]]
  p_rr_neg_threat_m <- pred_means_plot_list[grepl("rr_neg_threat_m", names(pred_means_plot_list))][[1]]
  p_bbsiq_ben_m     <- pred_means_plot_list[grepl("bbsiq_ben_m",     names(pred_means_plot_list))][[1]]
  p_bbsiq_neg_m     <- pred_means_plot_list[grepl("bbsiq_neg_m",     names(pred_means_plot_list))][[1]]
  
  plot_grid <- plot_grid(p_rr_pos_threat_m +
                           theme(legend.position = "none") + 
                           xlab(NULL), 
                         p_rr_neg_threat_m + 
                           theme(legend.position = "none") + 
                           xlab(NULL) + 
                           ylab(NULL), 
                         p_bbsiq_ben_m + 
                           theme(legend.position = "none"), 
                         p_bbsiq_neg_m + 
                           theme(legend.position = "none") +
                           ylab(NULL),
                           align = "hv",
                           ncol = 2)
  plot_leg <- get_legend(p_rr_pos_threat_m + 
                         theme(legend.position = "bottom"))
  plot <- plot_grid(plot_grid, plot_leg, ncol = 1, rel_heights = c(1, .1))
  
  return(plot)
}

# Run function

plot_eff_c1_corr_itt_2000    <- arrange_plots(res_trm_eff_c1_2000bs, "c1_corr_itt_2000")
plot_eff_s5_train_compl_2000 <- arrange_plots(res_trm_eff_c1_2000bs, "s5_train_compl_2000")

# Save plots

plots_path <- "./results/bayesian/plots/"
dir.create(plots_path)

ggsave2(paste0(plots_path, "plot_eff_c1_corr_itt_2000.png"),
        plot = plot_eff_c1_corr_itt_2000, 
        width = 10, height = 10)
ggsave2(paste0(plots_path, "plot_eff_s5_train_compl_2000.png"),
        plot = plot_eff_s5_train_compl_2000, 
        width = 10, height = 10)

# TODO: Revise function above to arrange separate plots for anxiety outcomes





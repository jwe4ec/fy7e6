# ---------------------------------------------------------------------------- #
# Compute Raw Means and Standard Deviations Over Time
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

pkgs <- c("flextable", "officer", "ftExtra")
groundhog.library(pkgs, groundhog_day, tolerate.R.version = "4.1.2")

# Set "flextable" package defaults and load "officer" package properties

source("./code/01b_set_flextable_defaults.R")
source("./code/01c_set_officer_properties.R")

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate_clean_further/anlys_df.RData")
load("./data/intermediate_clean_further/dem_tbl.RData")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Add condition, analysis sample indicators, and "condition_sep" from "dem_tbl"

target_vars <- c("participant_id", "conditioning",
                 "exclude_analysis", "itt_anlys", "s5_train_compl_anlys_uncorrected_c1",
                 "class_meas_compl_anlys", "s5_train_compl_anlys_c2_4",
                 "condition_sep")

anlys_df <- merge(anlys_df, dem_tbl[, target_vars],
                  by = "participant_id", all.x = TRUE)

# Check sample sizes (divide by 7 time points to compute number of participants)

table(anlys_df$condition_sep[anlys_df$itt_anlys == 1])/7
table(anlys_df$condition_sep[anlys_df$s5_train_compl_anlys_uncorrected_c1 == 1])/7

table(anlys_df$condition_sep[anlys_df$class_meas_compl_anlys == 1])/7
table(anlys_df$condition_sep[anlys_df$s5_train_compl_anlys_c2_4 == 1])/7

# Make "session_only_col" a factor (for correct order when aggregating below)

ordered_levs <- c("baseline",
                  paste0(c("first", "second", "third", "fourth", "fifth"), "Session"),
                  "PostFollowUp")

anlys_df$session_only_col <- factor(anlys_df$session_only_col, levels = ordered_levs)

# Restrict to ITT and Session 5 training completer samples

anlys_df_itt            <- anlys_df[anlys_df$itt_anlys == 1, ]
anlys_df_s5_train_compl <- anlys_df[anlys_df$s5_train_compl_anlys_c2_4 == 1, ]

# ---------------------------------------------------------------------------- #
# Compute n, M, and SD for each average item score by condition over time ----
# ---------------------------------------------------------------------------- #

# Define outcomes

outcomes <- c("rr_pos_threat_m", "rr_neg_threat_m", "bbsiq_ben_m", "bbsiq_neg_m",
              "oa_m", "dass21_as_m")

# Define function for computing n, M, and SD for each score over time

compute_desc_outcomes <- function(df) {
  for (k in 1:length(outcomes)) {
    fml <- as.formula(paste0(outcomes[k], "~ session_only_col"))
    ag <- do.call(data.frame, aggregate(fml, 
                                        data = df,
                                        FUN = function(x) {
          c(n    = length(x), 
            m_sd = paste0(format(round(mean(x), 2), nsmall = 2, trim = TRUE), 
                          " (",
                          format(round(sd(x), 2), nsmall = 2, trim = TRUE),
                          ")"))
          }))
          
    outcome_output <- cbind(outcomes[k], ag)
    names(outcome_output) <- c("Outcome", "Assessment", "n", "m_sd")
    
    if (k == 1) {
      output <- outcome_output
    } else if (k > 1) {
      output <- rbind(output, outcome_output)
    } 
  }
  
  return(output)
}

# Create empty data frame with all outcomes at all time points to store output 
# (given that not all conditions will have observations at all time points)

out <- data.frame(Outcome    = rep(outcomes, each = length(ordered_levs)),
                  Assessment = rep(ordered_levs, length(outcomes)))

# Define function for computing n, M, and SD for each score by condition over time

compute_desc_outcomes_by_cond <- function(df, out, outcomes, ordered_levs) {
  conditions <- levels(droplevels(df$condition_sep))
  
  for (i in 1:length(conditions)) {
    df_cond <- df[df$condition_sep == conditions[i], ]
    
    cond_res <- compute_desc_outcomes(df_cond)
    
    cond_out <- merge(out, cond_res, by = c("Outcome", "Assessment"), all.x = TRUE)
    
    cond_out$Outcome    <- factor(cond_out$Outcome,    levels = outcomes)
    cond_out$Assessment <- factor(cond_out$Assessment, levels = ordered_levs)
    
    cond_out <- cond_out[order(cond_out$Outcome, cond_out$Assessment), ]

    names(cond_out)[names(cond_out) == "n"]     <- paste0("n_",    conditions[i])
    names(cond_out)[names(cond_out) == "m_sd"]  <- paste0("m_sd_", conditions[i])

    if (i == 1) {
      res_by_cond <- cond_out
    } else if (i > 1) {
      cond_out[, c("Outcome", "Assessment")] <- NULL

      res_by_cond <- cbind(res_by_cond, cond_out)
    }
  }
  
  res_by_cond <- res_by_cond[!(res_by_cond$Outcome != "oa_m" & 
                               res_by_cond$Assessment %in% 
                                 paste0(c("first", "second", "fourth"), "Session")), ]
  
  return(res_by_cond)
}

# Compute descriptives by condition for the ITT sample (which includes the
# classification measure completer sample) and Session 5 training completers

res_outcomes_itt_by_cond <- 
  compute_desc_outcomes_by_cond(anlys_df_itt,            out, outcomes, ordered_levs)
res_outcomes_s5_train_compl_by_cond <- 
  compute_desc_outcomes_by_cond(anlys_df_s5_train_compl, out, outcomes, ordered_levs)

# ---------------------------------------------------------------------------- #
# Format descriptives tables ----
# ---------------------------------------------------------------------------- #

# "flextable" defaults are set in "set_flextable_defaults.R" above

# Section and text properties are sourced from "set_officer_properties.R" above

# Define function to format descriptives tables (note: horizontal borders for added 
# header rows for columns in which the ITT and CMC sample headings don't apply must be
# manually removed after export to MS Word)

format_desc_tbl <- function(desc_tbl, analysis_sample, gen_note, footnotes, title) {
  # Define columns
  
  target_cols <- names(desc_tbl)
  left_align_body_cols <- c("Outcome", "Assessment")
  merge_v_cols         <- "Outcome"
  
  # Define column formats
  
  n_format    <- as_paragraph(as_i("n"))
  m_sd_format <- as_paragraph(as_i("M"), " (", as_i("SD"), ")")

  # Create flextable
  
  desc_tbl_ft <- flextable(desc_tbl[, target_cols]) |>
    set_table_properties(align = "left") |>
    
    set_caption(as_paragraph(as_i(title)), word_stylename = "heading 1",
                fp_p = fp_par(padding.left = 0, padding.right = 0),
                align_with_table = FALSE) |>
    
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    align(j = left_align_body_cols, align = "left", part = "body") |>
    align(align = "left", part = "footer") |>
    
    merge_v(j = merge_v_cols, part = "body") |>
    valign(j = merge_v_cols, valign = "top", part = "body") |>
    fix_border_issues(part = "body") |>
    
    valign(valign = "bottom", part = "header") |>
    
    compose(j = "n_LR_TRAINING", part = "header", value = n_format) |>
    compose(j = "n_HR_NO_COACH", part = "header", value = n_format) |>
    compose(j = "n_HR_COACH",    part = "header", value = n_format) |>
    compose(j = "n_CTRL_cls",    part = "header", value = n_format) |>
    
    compose(j = "m_sd_LR_TRAINING", part = "header", value = m_sd_format) |>
    compose(j = "m_sd_HR_NO_COACH", part = "header", value = m_sd_format) |>
    compose(j = "m_sd_HR_COACH",    part = "header", value = m_sd_format) |>
    compose(j = "m_sd_CTRL_cls",    part = "header", value = m_sd_format) |>
    
    add_footer_lines(gen_note) |>
    
    labelizor(part = "body",
              labels = c("rr_pos_threat_m" = "Positive Bias (RR)",
                         "rr_neg_threat_m" = "Negative Bias (RR)",
                         "bbsiq_ben_m"     = "Benign Bias (BBSIQ)",
                         "bbsiq_neg_m"     = "Negative Bias (BBSIQ)",
                         "oa_m"            = "Anxiety (OASIS)",
                         "dass21_as_m"     = "Anxiety (DASS-21-AS)",
                         "baseline"        = "Baseline",
                         "firstSession"    = "Session 1",
                         "secondSession"   = "Session 2",
                         "thirdSession"    = "Session 3",
                         "fourthSession"   = "Session 4",
                         "fifthSession"    = "Session 5",
                         "PostFollowUp"    = "Follow-up")) |>
    
    fontsize(size = 10, part = "all")
  if (analysis_sample == "itt") {
    desc_tbl_ft <- desc_tbl_ft |>
      compose(j = "n_TRAINING",    part = "header", value = n_format) |>
      compose(j = "n_CTRL_ncls",   part = "header", value = n_format) |>
      
      compose(j = "m_sd_TRAINING",    part = "header", value = m_sd_format) |>
      compose(j = "m_sd_CTRL_ncls",   part = "header", value = m_sd_format) |>
      
      add_header_row(values = as_paragraph_md(c("",
                                                "CBM-I Lacking\\\nClassification\\\nMeasures or\\\nUnclassified",
                                                "CBM-I LR",
                                                "CBM-I HR\\\nNo Coaching",
                                                "CBM-I HR\\\nCoaching",
                                                "Psychoed.",
                                                "Psychoed.\\\nLacking\\\nClassification\\\nMeasures")),
                     colwidths = rep(2, 7)) |>
      add_header_row(values = c("", "Classification Measure Completer Sample", ""),
                     colwidths = c(4, 8, 2)) |>
      add_header_row(values = c("", "Intent-To-Treat Sample"),
                     colwidths = c(2, 12)) |>
      
      footnote(i = 3, j = c(3, 9),
               value = as_paragraph_md(c(footnotes$TRAINING, footnotes$HR_COACH)),
               ref_symbols = c(" a", " b"),
               part = "header") |>
      footnote(i = 24, j = 2,
               value = as_paragraph_md(footnotes$screening),
               ref_symbols = " c",
               part = "body") |>
    
      autofit() |>
      
      width(j = 1, width = .8) |>
      width(j = c(3, 5, 7, 9, 11, 13), .5)
  } else if (analysis_sample == "s5_train_compl") {
    desc_tbl_ft <- desc_tbl_ft |>
      add_header_row(values = as_paragraph_md(c("",
                                                "CBM-I LR",
                                                "CBM-I HR\\\nNo Coaching",
                                                "CBM-I HR\\\nCoaching",
                                                "Psychoed.")),
                     colwidths = rep(2, 5)) |>
      
      footnote(i = 1, j = 7,
               value = as_paragraph_md(footnotes$HR_COACH),
               ref_symbols = " a",
               part = "header") |>
      footnote(i = 24, j = 2,
               value = as_paragraph_md(footnotes$screening),
               ref_symbols = " b",
               part = "body") |>
      
      autofit()
  }
}

# Define notes

gen_note <- as_paragraph_md("*Note.* CBM-I = cognitive bias modification for interpretation; LR = Lower Risk; HR = Higher Risk.")

footnotes <- list(TRAINING = "\\ Includes 2 participants who completed classification measures but were not classified (software bug).",
                  HR_COACH = "\\ Excluded from CBM-I vs. psychoeducation comparison.",
                  screening = "\\ Screening. All other baseline values were assessed at pretreatment.")

# TODO: Consider looking into why some baseline numbers are off by one (see yellow 
# highlight in "desc_tbls.docx". Likely due to refusals to answer all items.)






# Run function

desc_tbl_itt_by_cond_ft <- 
  format_desc_tbl(res_outcomes_itt_by_cond, "itt", gen_note, footnotes,
  "Raw Means and Standard Deviations by Treatment Arm for Intent-To-Treat and Classification Measure Completer Samples")
desc_tbl_s5_train_compl_by_cond_ft <- 
  format_desc_tbl(res_outcomes_s5_train_compl_by_cond, "s5_train_compl", gen_note, footnotes,
  "Raw Means and Standard Deviations by Treatment Arm for Session 5 Training Completer Sample")

# ---------------------------------------------------------------------------- #
# Write descriptives tables to MS Word ----
# ---------------------------------------------------------------------------- #

# Write descriptives tables (note: "flextable" seems to have a bug in which blank 
# page is at end of doc)

desc_tbls <- list(desc_tbl_itt_by_cond_ft, desc_tbl_s5_train_compl_by_cond_ft)

desc_tbl_orientations <- c("l", "l")
desc_tbl_numbers      <- c("SA1", "SA2")

doc <- read_docx()
doc <- body_set_default_section(doc, psect_prop)

for (i in 1:length(desc_tbls)) {
  doc <- body_add_fpar(doc, fpar(ftext(paste0("Table ", desc_tbl_numbers[[i]]),
                                       prop = text_prop_bold)))
  doc <- body_add_par(doc, "")
  
  doc <- body_add_flextable(doc, desc_tbls[[i]], align = "left")
  
  if (desc_tbl_orientations[[i]] == "p") {
    doc <- body_end_block_section(doc, block_section(psect_prop))
  } else if (desc_tbl_orientations[[i]] == "l") {
    doc <- body_end_block_section(doc, block_section(lsect_prop))
  }
}

desc_tbl_path <- "./results/descriptives/tables/"

dir.create(desc_tbl_path, recursive = TRUE)

print(doc, target = paste0(desc_tbl_path, "desc_tbls.docx"))
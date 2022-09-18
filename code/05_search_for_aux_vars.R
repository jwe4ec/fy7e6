# ---------------------------------------------------------------------------- #
# Search for Auxiliary Variables
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

pkgs <- c("dplyr", "DescTools", "aod")
groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate_clean_further/dat2.RData")

# ---------------------------------------------------------------------------- #
# Prepare "completion" data ----
# ---------------------------------------------------------------------------- #

completion <- dat2$completion

# Compute indicators for missing a given session's training or assessment

completion$miss_session_train <- NA
completion$miss_session_train[completion$compl_session_train == 1] <- 0
completion$miss_session_train[completion$compl_session_train == 0] <- 1

completion$miss_session_assess <- NA
completion$miss_session_assess[completion$compl_session_assess == 1] <- 0
completion$miss_session_assess[completion$compl_session_assess == 0] <- 1

# Add condition

completion <- merge(completion, dat2$study[, c("participant_id", "conditioning")],
                    by = "participant_id", all.x = TRUE)

# Add analysis sample indicators

completion <- merge(completion,
                    dat2$participant[, c("participant_id", "exclude_analysis",
                                         "itt_anlys", "s5_train_compl_anlys_uncorrected_c1",
                                         "class_meas_compl_anlys", "s5_train_compl_anlys_c2_4")],
                    by = "participant_id", all.x = TRUE)

# Restrict to ITT sample

compl_itt <- completion[completion$itt_anlys == 1, ]

# Collapse "Eligibility" and "preTest" into "baseline" given that no analysis
# variable was assessed at both time points. To do so, remove "Eligibility"
# rows (because ITT participants had to complete "Eligibility" and "preTest")
# and rename "preTest" to "baseline"

compl_itt <- compl_itt[compl_itt$session_only != "Eligibility", ]
compl_itt$session_only[compl_itt$session_only == "preTest"] <- "baseline"

# ---------------------------------------------------------------------------- #
# Compute proportion of missing training sessions across time points ----
# ---------------------------------------------------------------------------- #

# Note: Proportion of missing training sessions is an outcome (not relevant to
# missing data handling but computed here for convenience)

tmp_ag <- aggregate(miss_session_train ~ participant_id,
                    compl_itt,
                    FUN = sum)

names(tmp_ag)[names(tmp_ag) == "miss_session_train"] <- "miss_session_train_sum"

tmp_ag$miss_session_train_prop <- tmp_ag$miss_session_train_sum / 5

compl_itt <- merge(compl_itt, tmp_ag, by = "participant_id", all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Compute proportion of missing assessments across time points ----
# ---------------------------------------------------------------------------- #

tmp_ag <- aggregate(miss_session_assess ~ participant_id,
                    compl_itt,
                    FUN = sum)

names(tmp_ag)[names(tmp_ag) == "miss_session_assess"] <- "miss_session_assess_sum"

tmp_ag$miss_session_assess_prop <- tmp_ag$miss_session_assess_sum / 7

compl_itt <- merge(compl_itt, tmp_ag, by = "participant_id", all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Add potential auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Extract demographic variables

tmp_dem <- dat2$demographics[, c("participant_id", "education", "employment_stat",
                                 "ethnicity", "gender", "marital_stat", "race_col",
                                 "country", "country_col")]

# After inspection of differences in mean proportion of missing assessments (see
# below), we decided to collapse "gender" levels

tmp_dem$gender_col <- as.character(tmp_dem$gender)

tmp_dem$gender_col[tmp_dem$gender_col %in%
                     c("Transgender", "Transgender Female", "Transgender Male",
                       "Other")] <- "Transgender/Other"

tmp_dem$gender_col <- factor(tmp_dem$gender_col,
                             levels = c("Female", "Male", "Transgender/Other", 
                                        "Prefer not to answer"))

# After inspection of differences in proportion of missing values for "income" (see
# below), we decided to collapse "employment_stat" and "marital_stat" levels

tmp_dem$employment_stat_col <- as.character(tmp_dem$employment_stat)

tmp_dem$employment_stat_col[tmp_dem$employment_stat_col %in%
                              c("Working part-time", "Working full-time")] <- "Working"
tmp_dem$employment_stat_col[tmp_dem$employment_stat_col %in%
                              c("Homemaker",
                                "Unemployed or laid off", "Looking for work", 
                                "Retired", 
                                "Unknown", "Other")] <- "Other or Unknown"

tmp_dem$employment_stat_col <- factor(tmp_dem$employment_stat_col,
                                      levels = c("Student", "Working", "Other or Unknown",
                                                 "Prefer not to answer"))

tmp_dem$marital_stat_col <- as.character(tmp_dem$marital_stat)

tmp_dem$marital_stat_col[tmp_dem$marital_stat_col %in%
                           c("Single", "Single,dating")] <- "Single or Dating"
tmp_dem$marital_stat_col[tmp_dem$marital_stat_col %in%
                           c("Single,engaged", "Single,marriagelike", "Married",
                             "civilunion")] <- "Engaged or Married-Like"
tmp_dem$marital_stat_col[tmp_dem$marital_stat_col %in%
                           c("Separated", "Divorced", 
                             "Widow/widower")] <- "Separated, Divorced, or Widowed"

tmp_dem$marital_stat_col <- factor(tmp_dem$marital_stat_col,
                                   levels = c("Single or Dating", 
                                              "Engaged or Married-Like",
                                              "Separated, Divorced, or Widowed",
                                              "Other",
                                              "Prefer not to answer"))

# Extract training confidence and change importance items

tmp_cred <- dat2$credibility[, c("participant_id", "confident_online", "important")]

tmp_at <- dat2$angular_training[dat2$angular_training$stimulus_name == "readiness_rulers",
                                c("participant_id", "button_pressed")]
names(tmp_at)[names(tmp_at) == "button_pressed"] <- "confident_design"

# Recode "confident_design" using values from MindTrails Future Thinking Study.
# Note, however, that in Future Thinking, "very" was replaced with "extremely".
# See Eberle et al. (2020): https://doi.org/d54p.

tmp_at$confident_design[tmp_at$confident_design == "Not at all"] <- 0
tmp_at$confident_design[tmp_at$confident_design == "Slightly"] <- 1
tmp_at$confident_design[tmp_at$confident_design == "Somewhat"] <- 2
tmp_at$confident_design[tmp_at$confident_design == "Mostly"] <- 3
tmp_at$confident_design[tmp_at$confident_design == "Very"] <- 4
tmp_at$confident_design[tmp_at$confident_design == "Prefer not to answer"] <- 555

tmp_at$confident_design <- as.numeric(tmp_at$confident_design)

# Extract device. Note that "device" is not recorded at "Eligibility" or at "task_name" 
# of "SESSION_COMPLETE"; remove these rows.

tmp_tl <- dat2$task_log[, c("participant_id", "session_only", "device", "task_name")]

tmp_tl <- tmp_tl[tmp_tl$session_only != "Eligibility" & 
                   tmp_tl$task_name != "SESSION_COMPLETE", ]

# Compute time-invariant "device_col" representing device types used throughout
# study (where "multiple types" is more than one type)

tmp_tl_unq <- unique(tmp_tl[, c("participant_id", "device")])

n_devices <- tmp_tl_unq %>%
  group_by(across("participant_id")) %>%
  summarise(count=n()) %>%
  as.data.frame()

names(n_devices)[names(n_devices) == "count"] <- "n_devices"

tmp_tl_unq <- merge(tmp_tl_unq, n_devices, 
                    by = c("participant_id"), all.x = TRUE)

tmp_tl_unq$device_col <- tmp_tl_unq$device
tmp_tl_unq$device_col[tmp_tl_unq$n_devices > 1] <- "multiple types"

tmp_tl_unq$device_col <-
  factor(tmp_tl_unq$device_col,
         levels = c("desktop", "tablet", "mobile", "multiple types"))

tmp_tl_unq2 <- unique(tmp_tl_unq[, c("participant_id", "n_devices", "device_col")])

# After inspection of differences in mean proportion of missing assessments (see
# below), it was decided to collapse "device_col" further into a binary variable

tmp_tl_unq2$device_col_bin <- as.character(tmp_tl_unq2$device_col)

tmp_tl_unq2$device_col_bin[tmp_tl_unq2$device_col_bin %in%
                             c("desktop", "tablet", "mobile")] <- "one type"

tmp_tl_unq2$device_col_bin <- factor(tmp_tl_unq2$device_col_bin,
                                     levels = c("one type", "multiple types"))

# Add extracted variables

compl_itt <- merge(compl_itt, tmp_dem, by = "participant_id", all.x = TRUE)
compl_itt <- merge(compl_itt, tmp_cred, by = "participant_id", all.x = TRUE)
compl_itt <- merge(compl_itt, tmp_at, by = "participant_id", all.x = TRUE)
compl_itt <- merge(compl_itt, tmp_tl_unq2, by = "participant_id", all.x = TRUE)

# Sort by "participant_id" and "session_only"

sessions <- c("baseline",
              paste0(c("first", "second", "third", "fourth", "fifth"), "Session"),
              "PostFollowUp")

compl_itt$session_only <- factor(compl_itt$session_only, levels = sessions)

compl_itt <- compl_itt[order(compl_itt$participant_id, compl_itt$session_only), ]

# Recode "prefer not to answer" in potential continuous auxiliary variables

target_cols <- c("confident_online", "confident_design", "important")

compl_itt[, target_cols][compl_itt[, target_cols] == 555] <- NA

# ---------------------------------------------------------------------------- #
# Create data frame for time-invariant auxiliary variables ----
# ---------------------------------------------------------------------------- #

time_varying_cols <- c("session_only", "compl_session_train", "compl_session_assess",
                       "miss_session_train", "miss_session_assess")

compl_itt_iv <- compl_itt[, names(compl_itt)[!(names(compl_itt) %in% time_varying_cols)]]

compl_itt_iv <- unique(compl_itt_iv)

# ---------------------------------------------------------------------------- #
# Consider correlation between training confidence items ----
# ---------------------------------------------------------------------------- #

# "confident_design" and "confident_online" are highly correlated, r = .49

cor.test(compl_itt_iv$confident_design, compl_itt_iv$confident_online, 
         method = "pearson")

plot(compl_itt_iv$confident_design, compl_itt_iv$confident_online)
abline(lm(compl_itt_iv$confident_online ~ compl_itt_iv$confident_design))

# However, the items are not normal, so also estimate their association using 
# nonparametric test. Goodman & Kruskal's gamma (given many tied ranks) = .64. See
# https://statistics.laerd.com/spss-tutorials/goodman-and-kruskals-gamma-using-spss-statistics.php

par(mfrow = c(2, 1))
hist(compl_itt_iv$confident_design, main = "confident_design")
hist(compl_itt_iv$confident_online, main = "confident_online")
par(mfrow = c(1, 1))

shapiro.test(compl_itt_iv$confident_design)
shapiro.test(compl_itt_iv$confident_online)

GoodmanKruskalGamma(compl_itt_iv$confident_online, 
                    compl_itt_iv$confident_design, conf.level = .95)

# Given the strong association, compute and analyze mean of available items, following
# Hohensee et al. (2020, https://doi.org/hmbk), who found that the mean predicted dropout. 
# However, given that far more participants have data for "confident_online" (given at 
# "preTest") than "confident_design" (given during "firstSession" training), also analyze 
# the items separately. Note: Henry Behan stated on 3/21/22 that "confident_design" data
# were likely not collected for participants in conditions other than "CONTROL" after
# 8/2019 due to a software bug (perhaps addition of a videos page interfered with data
# collection; the videos page was not added to the "CONTROL" condition).

sum(!is.na(compl_itt_iv$confident_online)) == 1229
sum(!is.na(compl_itt_iv$confident_design)) == 662

compl_itt_iv$confident_m <- rowMeans(compl_itt_iv[, c("confident_online", "confident_design")],
                                     na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Prepare and add "income" data ----
# ---------------------------------------------------------------------------- #

# Compute missing data indicator for "income" (i.e., "Unknown", "Prefer not to answer")

dat2$demographics$income_ind <- 0
dat2$demographics$income_ind[dat2$demographics$income %in% 
                               c("Unknown", "Prefer not to answer")] <- 1

# Add indicator to time-invariant data frame

compl_itt_iv <- merge(compl_itt_iv, 
                      dat2$demographics[, c("participant_id", "income_ind")], 
                      by = "participant_id", all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Restrict analysis samples ----
# ---------------------------------------------------------------------------- #

# Restrict to three samples: (a) unrestricted ITT sample (i.e., all randomized to 
# CBM-I or Psychoed. who started S1 training, including "HR_COACH"), (b) restricted 
# ITT sample (i.e., exclude "HR_COACH", but don't use bootstrapping to correct size
# of "HR_TRAINING"), and (c) classification measure completer sample

compl_itt_unrestricted <- compl_itt_iv
compl_itt_restricted <-   compl_itt_iv[compl_itt_iv$conditioning != "HR_COACH", ]
compl_class_meas_compl <- compl_itt_iv[compl_itt_iv$class_meas_compl_anlys == 1, ]

# ---------------------------------------------------------------------------- #
# Search for time-invariant categorical and ordinal auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Per consult with Cynthia Tong on 2/22/22, define function to compute mean 
# proportion of missing assessments across time points within each level of the 
# potential auxiliary variable (treat ordinal variables as categorical). Per
# consult with Cynthia on 4/12/22, also compute proportion of missing "income" 
# responses within each level of the potential auxiliary variable.

compute_desc_by_level <- function(df, miss_data_ind) {
  vars <- c("gender", "gender_col", "race_col", "ethnicity", "country_col", 
            "education", "employment_stat", "employment_stat_col", "marital_stat",
            "marital_stat_col", "device_col", "device_col_bin")
  
  var_labels <- c("Gender", "Gender (Collapsed)", "Race", "Ethnicity", "Country", 
                  "Education", "Employment Status", "Employment Status (Collapsed)",
                  "Marital Status", "Marital Status (Collapsed)",
                  "Device (Collapsed)", "Device (Binary)")
  
  res <- data.frame()
  
  for (i in 1:length(vars)) {
    tbl <-     table(df[, vars[i]])
    
    if (miss_data_ind == "miss_session_assess_prop") {
      # Compute count, mean, and standard deviation
      
      ag_mean <- aggregate(df[, miss_data_ind], list(df[, vars[i]]), 
                           FUN = mean, drop = FALSE)
      ag_sd <-   aggregate(df[, miss_data_ind], list(df[, vars[i]]), 
                           FUN = sd, drop = FALSE)
      
      var_res <- rbind(data.frame(label = var_labels[i],
                                  n     = NA,
                                  M     = NA,
                                  SD    = NA),
                       data.frame(label = names(tbl),
                                  n     = as.numeric(tbl),
                                  M     = round(ag_mean$x, 2),
                                  SD    = round(ag_sd$x, 2)))
    } else if (miss_data_ind == "income_ind") {
      # Compute proportion (same as mean for binary missing data indicator)
      
      ag_prop <- aggregate(df[, miss_data_ind], list(df[, vars[i]]), 
                           FUN = mean, drop = FALSE)

      var_res <- rbind(data.frame(label = var_labels[i],
                                  n     = NA,
                                  Prop. = NA),
                       data.frame(label = names(tbl),
                                  n     = as.numeric(tbl),
                                  Prop. = round(ag_prop$x, 2)))
    }
    
    res <- rbind(res, var_res)
  }
  
  return(res)
}

# For "miss_session_assess_prop", run function for each analysis sample, compute 
# size of each sample, combine results into table, and export table

miss_session_assess_prop_fct_res_itt_unrestricted <- 
  compute_desc_by_level(compl_itt_unrestricted, "miss_session_assess_prop")
miss_session_assess_prop_fct_res_itt_restricted <- 
  compute_desc_by_level(compl_itt_restricted, "miss_session_assess_prop")
miss_session_assess_prop_fct_res_class_meas_compl <- 
  compute_desc_by_level(compl_class_meas_compl, "miss_session_assess_prop")

nrow(compl_itt_unrestricted) == 1234
nrow(compl_itt_restricted)   == 953
nrow(compl_class_meas_compl) == 1073

miss_session_assess_prop_fct_res <- 
  cbind(miss_session_assess_prop_fct_res_itt_unrestricted, 
        miss_session_assess_prop_fct_res_itt_restricted[, names(miss_session_assess_prop_fct_res_itt_restricted) != "label"], 
        miss_session_assess_prop_fct_res_class_meas_compl[, names(miss_session_assess_prop_fct_res_class_meas_compl) != "label"])

dir.create("./results/search_aux_vars")

write.csv(miss_session_assess_prop_fct_res,
          file = "./results/search_aux_vars/potential_miss_session_assess_prop_cat_ord_aux_vars.csv",
          row.names = FALSE)

# For "income_ind", run function for each analysis sample, combine results into 
# table, and export table

income_ind_fct_res_itt_unrestricted <- 
  compute_desc_by_level(compl_itt_unrestricted, "income_ind")
income_ind_fct_res_itt_restricted <- 
  compute_desc_by_level(compl_itt_restricted, "income_ind")
income_ind_fct_res_class_meas_compl <- 
  compute_desc_by_level(compl_class_meas_compl, "income_ind")

income_ind_fct_res <- 
  cbind(income_ind_fct_res_itt_unrestricted, 
        income_ind_fct_res_itt_restricted[, names(income_ind_fct_res_itt_restricted) != "label"], 
        income_ind_fct_res_class_meas_compl[, names(income_ind_fct_res_class_meas_compl) != "label"])

write.csv(income_ind_fct_res,
          file = "./results/search_aux_vars/potential_income_ind_cat_ord_aux_vars.csv",
          row.names = FALSE)

# Inspect tables using Cynthia Tong's guidance on 4/1/22: For very small differences 
# in mean proportion of missing assessments or proportion of missing income between 
# levels of a potential auxiliary variable, exclude the variable from missing data 
# handling. For clear differences (i.e., difference > .2 between two levels that each 
# have > 200 participants), include the variable. If unsure (i.e., a difference > .1 
# between two levels that each have > 200 participants), test differences and include 
# variable if differences are significant; otherwise, exclude it.

# On 4/1/22, Cynthia Tong, Katie Daniel, and Jeremy Eberle deemed variables except
# gender and device to have very small differences for "miss_session_assess_prop". 
# Device was deemed to have clear differences; however, Cynthia advised that its 
# levels be collapsed into a binary variable ("device_col_bin" above) to promote 
# model convergence. We were unsure re gender and decided to collapse sparse levels 
# (above) and test differences (below).

# On 4/21/22, Cynthia Tong, Katie Daniel, and Jeremy Eberle deemed variables except
# employment status and marital status to have very small differences for "income_ind". 
# We were unsure re employment status and marital status and decided to collapse 
# sparse levels (above) and test differences (below).

# Define function to recode "prefer not to answer" as NA for collapsed variables

recode_pna <- function(df, aux_var) {
  df[, aux_var][df[, aux_var] == "Prefer not to answer"] <- NA
  df[, aux_var] <- droplevels(df[, aux_var])
  
  return(df)
}

target_vars <- c("gender_col", "employment_stat_col", "marital_stat_col")

for (i in 1:length(target_vars)) {
  compl_itt_unrestricted <- recode_pna(compl_itt_unrestricted, target_vars[i])
  compl_itt_restricted   <- recode_pna(compl_itt_restricted, target_vars[i])
  compl_class_meas_compl <- recode_pna(compl_class_meas_compl, target_vars[i])
}

# Export data for unrestricted ITT sample

save(compl_itt_unrestricted, file = "./data/temp/compl_itt_unrestricted.RData")

# Define function to test differences. For differences in mean proportion of missing
# assessments, use one-way ANOVA as long as sample sizes are sufficiently large (i.e., 
# 2-9 groups with >= 15 participants per group) see 
# https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/anova/how-to/kruskal-wallis-test/before-you-start/data-considerations/).
# For differences in proportion of missing values for income, use logistic regression.

test_diffs <- function(df, model, outcome, aux_var) {
  fml <- as.formula(paste0(outcome, " ~ ", aux_var))
  
  n_aux_var_levels <- length(levels(df[, aux_var]))
  
  if (model == "lm") {
    lm_mod <- lm(fml, df)
    print(summary(lm_mod))
    print(anova(lm_mod))
  } else if (model == "glm") {
    glm_mod <- glm(fml, df, family = "binomial")
    print(summary(glm_mod))
    print(wald.test(Sigma = vcov(glm_mod), b = coef(glm_mod), Terms = 2:n_aux_var_levels))
  }
}

sink_test_diffs <- function(itt_unrestricted_df, itt_restricted_df, class_meas_compl_df,
                            model, outcome, aux_var) {
  sink(file = paste0("./results/search_aux_vars/", outcome, "_", aux_var, "_diffs.txt"))
  
  cat("ITT Sample (Unrestricted):", "\n", "\n")
  test_diffs(itt_unrestricted_df, model, outcome, aux_var)
  cat("\n")
  cat("--------------------", "\n", "\n")
  
  cat("ITT Sample (Restricted):", "\n", "\n")
  test_diffs(itt_restricted_df, model, outcome, aux_var)
  cat("\n")
  cat("--------------------", "\n", "\n")
  
  cat("Classif. Meas. Compl. Sample:", "\n", "\n")
  test_diffs(class_meas_compl_df, model, outcome, aux_var)
  
  sink()
}

# Given significant differences, include "gender_col" in missing data handling for
# longitudinal outcomes

sink_test_diffs(compl_itt_unrestricted, compl_itt_restricted, compl_class_meas_compl,
                "lm", "miss_session_assess_prop", "gender_col")

# Given significant differences, include "employment_stat_col" and "marital_stat_col"
# in missing data handling for income covariate

sink_test_diffs(compl_itt_unrestricted, compl_itt_restricted, compl_class_meas_compl,
                "glm", "income_ind", "employment_stat_col")
sink_test_diffs(compl_itt_unrestricted, compl_itt_restricted, compl_class_meas_compl,
                "glm", "income_ind", "marital_stat_col")

# ---------------------------------------------------------------------------- #
# Search for time-invariant continuous auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Relevant continuous variables are not normal; thus, compute Spearman's rank-order 
# correlation in addition to Pearson's product-moment correlation for correlations
# with "miss_session_assess_prop". For correlations with binary "income_ind", compute 
# only Pearson's point-biserial correlation coefficient.

par(mfrow = c(3, 2))
hist(compl_itt_unrestricted$miss_session_assess_prop, main = "miss_session_assess_prop")
hist(compl_itt_unrestricted$confident_m, main = "confident_m")
hist(compl_itt_unrestricted$confident_online, main = "confident_online")
hist(compl_itt_unrestricted$confident_design, main = "confident_design")
hist(compl_itt_unrestricted$important, main = "important")
par(mfrow = c(1, 1))

shapiro.test(compl_itt_unrestricted$miss_session_assess_prop)
shapiro.test(compl_itt_unrestricted$confident_m)
shapiro.test(compl_itt_unrestricted$confident_online)
shapiro.test(compl_itt_unrestricted$confident_design)
shapiro.test(compl_itt_unrestricted$important)

par(mfrow = c(2, 2))
plot(compl_itt_unrestricted$confident_m, compl_itt_unrestricted$miss_session_assess_prop,
     xlab = "confident_m", ylab = "miss_session_assess_prop")
plot(compl_itt_unrestricted$confident_online, compl_itt_unrestricted$miss_session_assess_prop,
     xlab = "confident_online", ylab = "miss_session_assess_prop")
plot(compl_itt_unrestricted$confident_design, compl_itt_unrestricted$miss_session_assess_prop,
     xlab = "confident_design", ylab = "miss_session_assess_prop")
plot(compl_itt_unrestricted$important, compl_itt_unrestricted$miss_session_assess_prop,
     xlab = "important", ylab = "miss_session_assess_prop")
par(mfrow = c(1, 1))

# Per consult with Cynthia Tong on 2/22/22, define function to compute correlation 
# with proportion of missing assessments across time points. No need to test "age" 
# and "income" as auxiliary variables (treated as continuous in analysis) as they 
# are already in analysis. However, per consult with Cynthia on 4/12/22, also compute
# correlation with "income" missing data indicator given that income is a covariate.
# Predictors of missing values in "age" covariate will not be investigated given that
# only a few participants have missing "age" (more have unknown or missing income).

compute_corr <- function(df, miss_data_ind) {
  vars <- c("confident_m", 
            "confident_online", "confident_design", 
            "important")
  
  var_labels <- c("Training Confidence", 
                  "Online Training Confidence", "Present Training Confidence", 
                  "Change Importance")
  
  res <- data.frame()
  
  for (i in 1:length(vars)) {
    n <- sum(!is.na(df[, vars[i]]))
    
    if (miss_data_ind == "miss_session_assess_prop") {
      # Compute Pearson's product-moment correlation and Spearman's rank-order correlation
      
      corr_pearson <- cor.test(df[, miss_data_ind], df[, vars[i]], 
                               method = "pearson")
      corr_spearman <- cor.test(df[, miss_data_ind], df[, vars[i]], 
                                method = "spearman")
      
      var_res <- data.frame(Variable = var_labels[i],
                            n = n,
                            
                            r_pearson = round(corr_pearson$estimate, 2),
                            CI_95_pct = paste0("[",  round(corr_pearson$conf.int[1], 2),
                                               ", ", round(corr_pearson$conf.int[2], 2), "]"),
                            t = round(corr_pearson$statistic, 2),
                            df = corr_pearson$parameter,
                            p_pearson = round(corr_pearson$p.value, 3),
                            
                            r_spearman = round(corr_spearman$estimate, 2),
                            S = round(corr_spearman$statistic, 2),
                            p_spearman = round(corr_spearman$p.value, 3))
    } else if (miss_data_ind == "income_ind") {
      # Compute Pearson's point-biserial correlation coefficient
      
      corr_pb <- cor.test(df[, miss_data_ind], df[, vars[i]], 
                          method = "pearson")

      var_res <- data.frame(Variable = var_labels[i],
                            n = n,
                            
                            r_pb = round(corr_pb$estimate, 2),
                            CI_95_pct = paste0("[",  round(corr_pb$conf.int[1], 2),
                                               ", ", round(corr_pb$conf.int[2], 2), "]"),
                            t = round(corr_pb$statistic, 2),
                            df = corr_pb$parameter,
                            p = round(corr_pb$p.value, 3))
    }
    
    res <- rbind(res, var_res)
  }
  
  return(res)
}

# For "miss_session_assess_prop", run function for each analysis sample, combine 
# results into table, and export table

miss_session_assess_prop_num_res_itt_unrestricted <- 
  compute_corr(compl_itt_unrestricted, "miss_session_assess_prop")
miss_session_assess_prop_num_res_itt_restricted <- 
  compute_corr(compl_itt_restricted, "miss_session_assess_prop")
miss_session_assess_prop_num_res_class_meas_compl <- 
  compute_corr(compl_class_meas_compl, "miss_session_assess_prop")

ncol <- length(miss_session_assess_prop_num_res_itt_unrestricted)

miss_session_assess_prop_num_res <- 
  rbind(c("ITT Sample (Unrestricted)", rep(NA, ncol - 1)),
        miss_session_assess_prop_num_res_itt_unrestricted,
        c("ITT Sample (Restricted)", rep(NA, ncol - 1)),
        miss_session_assess_prop_num_res_itt_restricted,
        c("Classification Measure Completer Sample", rep(NA, ncol - 1)),
        miss_session_assess_prop_num_res_class_meas_compl)

write.csv(miss_session_assess_prop_num_res,
          file = "./results/search_aux_vars/potential_miss_session_assess_prop_num_aux_vars.csv",
          row.names = FALSE)

# For "income_ind", run function for each analysis sample, combine results into 
# table, and export table

income_ind_num_res_itt_unrestricted <- 
  compute_corr(compl_itt_unrestricted, "income_ind")
income_ind_num_res_itt_restricted <- 
  compute_corr(compl_itt_restricted, "income_ind")
income_ind_num_res_class_meas_compl <- 
  compute_corr(compl_class_meas_compl, "income_ind")

ncol <- length(income_ind_num_res_itt_unrestricted)

income_ind_num_res <- 
  rbind(c("ITT Sample (Unrestricted)", rep(NA, ncol - 1)),
        income_ind_num_res_itt_unrestricted,
        c("ITT Sample (Restricted)", rep(NA, ncol - 1)),
        income_ind_num_res_itt_restricted,
        c("Classification Measure Completer Sample", rep(NA, ncol - 1)),
        income_ind_num_res_class_meas_compl)

write.csv(income_ind_num_res,
          file = "./results/search_aux_vars/potential_income_ind_num_aux_vars.csv",
          row.names = FALSE)

# Inspect results for "miss_session_assess_prop" using Cynthia Tong's guidance on 
# 4/1/22. Given that none of the correlations are significant, exclude all variables 
# from missing data handling.

# Given significant correlations for "income_ind", include "confident_online" and
# "important" in missing data handling for income covariate
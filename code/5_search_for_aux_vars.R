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

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

groundhog.library(DescTools, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

completion <- read.csv("./data/temp/completion.csv")
load("./data/intermediate_clean_further/dat2.RData")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Compute indicator for missing a given session's assessment

completion$miss_session_assess <- NA
completion$miss_session_assess[completion$compl_session_assess == 1] <- 0
completion$miss_session_assess[completion$compl_session_assess == 0] <- 1

# TODO: Add condition

completion <- merge(completion, dat2$study[, c("participant_id", "conditioning")],
                    by = "participant_id", all.x = TRUE)





# TODO: Add analysis sample indicators

completion <- merge(completion,
                    dat2$participant[, c("participant_id", "exclude_analysis",
                                         "itt_anlys", "s5_train_compl_anlys_uncorrected_c1",
                                         "class_meas_compl_anlys", "s5_train_compl_anlys_c2_4")],
                    by = "participant_id", all.x = TRUE)





# TODO: Restrict to ITT sample

compl_itt <- completion[completion$itt_anlys == 1, ]





# Collapse "Eligibility" and "preTest" into "baseline" given that no analysis
# variable was assessed at both time points. To do so, remove "Eligibility"
# rows (because ITT participants had to complete "Eligibility" and "preTest")
# and rename "preTest" to "baseline"

compl_itt <- compl_itt[compl_itt$session_only != "Eligibility", ]
compl_itt$session_only[compl_itt$session_only == "preTest"] <- "baseline"

# ---------------------------------------------------------------------------- #
# Compute proportion of missing assessments across time points ----
# ---------------------------------------------------------------------------- #

compl_itt_ag <- aggregate(miss_session_assess ~ participant_id,
                          compl_itt,
                          FUN = sum)

names(compl_itt_ag)[names(compl_itt_ag) == "miss_session_assess"] <- 
  "miss_session_assess_sum"

compl_itt_ag$miss_session_assess_prop <- compl_itt_ag$miss_session_assess_sum / 7

# ---------------------------------------------------------------------------- #
# Add potential auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Extract demographic variables

tmp_dem <- dat2$demographics[, c("participant_id", "education", "employment_stat",
                                 "ethnicity", "gender", "marital_stat", "race_col",
                                 "country", "country_col")]

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

# Add extracted variables

compl_itt_ag <- merge(compl_itt_ag, tmp_dem, by = "participant_id", all.x = TRUE)
compl_itt_ag <- merge(compl_itt_ag, tmp_cred, by = "participant_id", all.x = TRUE)
compl_itt_ag <- merge(compl_itt_ag, tmp_at, by = "participant_id", all.x = TRUE)

# Recode "prefer not to answer" in potential auxiliary variables

target_cols <- c("confident_online", "confident_design", "important")

compl_itt_ag[, target_cols][compl_itt_ag[, target_cols] == 555] <- NA

# ---------------------------------------------------------------------------- #
# Consider correlation between training confidence items ----
# ---------------------------------------------------------------------------- #

# "confident_design" and "confident_online" are highly correlated, r = .49

cor.test(compl_itt_ag$confident_design, compl_itt_ag$confident_online, 
         method = "pearson")

# However, the items are not normal, so also estimate their association using 
# nonparametric test. Goodman & Kruskal's gamma (given many tied ranks) = .64. See
# https://statistics.laerd.com/spss-tutorials/goodman-and-kruskals-gamma-using-spss-statistics.php

hist(compl_itt_ag$confident_design)
shapiro.test(compl_itt_ag$confident_design)

hist(compl_itt_ag$confident_online)
shapiro.test(compl_itt_ag$confident_online)

GoodmanKruskalGamma(compl_itt_ag$confident_online, 
                    compl_itt_ag$confident_design, conf.level = .95)

# Given the strong association, compute and analyze mean of available items, following
# Hohensee et al. (2020, https://doi.org/hmbk), who found that the mean predicted dropout. 
# However, given that far more participants have data for "confident_online" (given at 
# "preTest") than "confident_design" (given during "firstSession" training, unclear why 
# so few have data), also analyze the items separately.

sum(!is.na(compl_itt_ag$confident_online)) == 1229
sum(!is.na(compl_itt_ag$confident_design)) == 662

compl_itt_ag$confident_m <- rowMeans(compl_itt_ag[, c("confident_online", "confident_design")],
                                     na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Restrict analysis samples ----
# ---------------------------------------------------------------------------- #

# TODO: Conduct search for auxiliary variables in three samples: (a) unrestricted ITT
# sample, (b) restricted ITT sample (i.e., excluding "HR_COACH", but not using
# bootstrapping to correct sample size of "HR_TRAINING"), and (c) classification
# measure completer sample

# compl_itt_unrestricted <-          # Unrestricted ITT sample
# compl_itt_restricted <-            # Restricted ITT sample
# compl_class_meas_compl <-          # Classification measure completers sample


  


# ---------------------------------------------------------------------------- #
# Search for time-invariant categorical and ordinal auxiliary variables ----
# ---------------------------------------------------------------------------- #

# TODO: Per consultation with Cynthia Tong on 2/22/22, compute mean proportion of missing 
# data across time points within each level of the potential auxiliary variable
# (treat ordinal variables as categorical)

compute_desc_by_level <- function(df) {
  # Compute count, mean, and standard deviation
  
  vars <- c("gender", "race_col", "ethnicity", "country_col", "education",
            "employment_stat", "marital_stat")
  
  var_labels <- c("Gender", "Race", "Ethnicity", "Country", "Education",
                  "Employment Status", "Marital Status")
  
  res <- data.frame()
  
  for (i in 1:length(vars)) {
    tbl <-     table(df[, vars[i]])
    ag_mean <- aggregate(df$miss_session_assess_prop, list(df[, vars[i]]), 
                         FUN = mean, drop = FALSE)
    ag_sd <-   aggregate(df$miss_session_assess_prop, list(df[, vars[i]]), 
                         FUN = sd, drop = FALSE)
    
    var_res <- rbind(data.frame(label = var_labels[i],
                                n =     NA,
                                M =     NA,
                                SD =    NA),
                     data.frame(label = names(tbl),
                                n =     as.numeric(tbl),
                                M =     round(ag_mean$x, 2),
                                SD =    round(ag_sd$x, 2)))
    
    res <- rbind(res, var_res)
  }
  
  return(res)
}

fct_res_itt_uncorrected <- compute_desc_by_level(compl_itt_ag)





# ---------------------------------------------------------------------------- #
# Search for time-invariant continuous auxiliary variables ----
# ---------------------------------------------------------------------------- #

# TODO: Per consultation with Cynthia Tong on 2/22/22, compute correlation with 
# proportion of missing data across time points. No need to test "age" and "income"
# (treated as continuous in analysis) because they are already analysis variables.

compute_corr <- function(df) {
  # Compute correlation
  
  vars <- c("confident_m", 
            "confident_online", "confident_design", 
            "important")
  
  var_labels <- c("Training Confidence", 
                  "Online Training Confidence", "Present Training Confidence", 
                  "Change Importance")
  
  res <- data.frame()
  
  for (i in 1:length(vars)) {
    n <- sum(!is.na(df[, vars[i]]))
    corr <- cor.test(df$miss_session_assess_prop, df[, vars[i]], 
                     method = "pearson")
    
    var_res <- data.frame(Variable = var_labels[i],
                          n = n,
                          r = round(corr$estimate, 2),
                          CI_95_pct = paste0("[",  round(corr$conf.int[1], 2),
                                             ", ", round(corr$conf.int[2], 2), "]"),
                          t = round(corr$statistic, 2),
                          df = corr$parameter,
                          p = round(corr$p.value, 3))
    
    res <- rbind(res, var_res)
  }
  
  return(res)
}

num_res_itt_uncorrected <- compute_corr(compl_itt_ag)





# ---------------------------------------------------------------------------- #
# Search for time-varying categorical auxiliary variables ----
# ---------------------------------------------------------------------------- #

# TODO: Per consultation with Cynthia Tong on 2/22/22, compute proportion of missing
# data within each level at each time point





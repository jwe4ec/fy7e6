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

pkgs <- c("dplyr", "DescTools")
groundhog.library(pkgs, groundhog_day)

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
# Compute proportion of missing assessments across time points ----
# ---------------------------------------------------------------------------- #

temp_ag <- aggregate(miss_session_assess ~ participant_id,
                     compl_itt,
                     FUN = sum)

names(temp_ag)[names(temp_ag) == "miss_session_assess"] <- "miss_session_assess_sum"

temp_ag$miss_session_assess_prop <- temp_ag$miss_session_assess_sum / 7

compl_itt <- merge(compl_itt, temp_ag, by = "participant_id", all.x = TRUE)

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

# Recode "prefer not to answer" in potential auxiliary variables

target_cols <- c("confident_online", "confident_design", "important")

compl_itt[, target_cols][compl_itt[, target_cols] == 555] <- NA

# ---------------------------------------------------------------------------- #
# Create data frame for time-invariant auxiliary variables ----
# ---------------------------------------------------------------------------- #

time_varying_cols <- c("session_only", "compl_session_train", "compl_session_assess",
                       "miss_session_assess")

compl_itt_iv <- compl_itt[, names(compl_itt)[!(names(compl_itt) %in% time_varying_cols)]]

compl_itt_iv <- unique(compl_itt_iv)

# ---------------------------------------------------------------------------- #
# Consider correlation between training confidence items ----
# ---------------------------------------------------------------------------- #

# "confident_design" and "confident_online" are highly correlated, r = .49

cor.test(compl_itt_iv$confident_design, compl_itt_iv$confident_online, 
         method = "pearson")

# However, the items are not normal, so also estimate their association using 
# nonparametric test. Goodman & Kruskal's gamma (given many tied ranks) = .64. See
# https://statistics.laerd.com/spss-tutorials/goodman-and-kruskals-gamma-using-spss-statistics.php

hist(compl_itt_iv$confident_design)
shapiro.test(compl_itt_iv$confident_design)

hist(compl_itt_iv$confident_online)
shapiro.test(compl_itt_iv$confident_online)

GoodmanKruskalGamma(compl_itt_iv$confident_online, 
                    compl_itt_iv$confident_design, conf.level = .95)

# Given the strong association, compute and analyze mean of available items, following
# Hohensee et al. (2020, https://doi.org/hmbk), who found that the mean predicted dropout. 
# However, given that far more participants have data for "confident_online" (given at 
# "preTest") than "confident_design" (given during "firstSession" training, unclear why 
# so few have data), also analyze the items separately.

sum(!is.na(compl_itt_iv$confident_online)) == 1229
sum(!is.na(compl_itt_iv$confident_design)) == 662

compl_itt_iv$confident_m <- rowMeans(compl_itt_iv[, c("confident_online", "confident_design")],
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
            "employment_stat", "marital_stat", "device_col")
  
  var_labels <- c("Gender", "Race", "Ethnicity", "Country", "Education",
                  "Employment Status", "Marital Status", "Device")
  
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

fct_res_itt_uncorrected <- compute_desc_by_level(compl_itt_iv)





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

num_res_itt_uncorrected <- compute_corr(compl_itt_iv)





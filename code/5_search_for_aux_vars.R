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
# "preTest") than "confident_design" (given during "firstSession" training, unclear why 
# so few have data), also analyze the items separately.

sum(!is.na(compl_itt_iv$confident_online)) == 1229
sum(!is.na(compl_itt_iv$confident_design)) == 662

compl_itt_iv$confident_m <- rowMeans(compl_itt_iv[, c("confident_online", "confident_design")],
                                     na.rm = TRUE)

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
# potential auxiliary variable (treat ordinal variables as categorical)

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
                                n     = NA,
                                M     = NA,
                                SD    = NA),
                     data.frame(label = names(tbl),
                                n     = as.numeric(tbl),
                                M     = round(ag_mean$x, 2),
                                SD    = round(ag_sd$x, 2)))
    
    res <- rbind(res, var_res)
  }
  
  return(res)
}

# Run function for each analysis sample, compute size of each sample, combine 
# results into table, and export table

fct_res_itt_unrestricted <- compute_desc_by_level(compl_itt_unrestricted)
fct_res_itt_restricted   <- compute_desc_by_level(compl_itt_restricted)
fct_res_class_meas_compl <- compute_desc_by_level(compl_class_meas_compl)

nrow(compl_itt_unrestricted) == 1234
nrow(compl_itt_restricted)   == 953
nrow(compl_class_meas_compl) == 1073

fct_res <- cbind(fct_res_itt_unrestricted, 
                 fct_res_itt_restricted[, names(fct_res_itt_restricted) != "label"], 
                 fct_res_class_meas_compl[, names(fct_res_class_meas_compl) != "label"])

dir.create("./results/search_aux_vars")

write.csv(fct_res,
          file = "./results/search_aux_vars/potential_cat_ord_aux_vars.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Search for time-invariant continuous auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Relevant variables are not normally distributed. Thus, compute Spearman's rank-
# order correlation in addition to Pearson's product-moment correlation

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
# and "income" (treated as continuous in analysis) as they are already in analysis.

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
    corr_pearson <- cor.test(df$miss_session_assess_prop, df[, vars[i]], 
                             method = "pearson")
    corr_spearman <- cor.test(df$miss_session_assess_prop, df[, vars[i]], 
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
    
    res <- rbind(res, var_res)
  }
  
  return(res)
}

# Run function for each analysis sample, compute size of each sample, combine 
# results into table, and export table

num_res_itt_unrestricted <- compute_corr(compl_itt_unrestricted)
num_res_itt_restricted   <- compute_corr(compl_itt_restricted)
num_res_class_meas_compl <- compute_corr(compl_class_meas_compl)

nrow(compl_itt_unrestricted) == 1234
nrow(compl_itt_restricted)   == 953
nrow(compl_class_meas_compl) == 1073

ncol <- length(num_res_itt_unrestricted)

num_res <- rbind(c("ITT Sample (Unrestricted)", rep(NA, ncol - 1)),
                 num_res_itt_unrestricted,
                 c("ITT Sample (Restricted)", rep(NA, ncol - 1)),
                 num_res_itt_restricted,
                 c("Classification Measure Completer Sample", rep(NA, ncol - 1)),
                 num_res_class_meas_compl)

write.csv(num_res,
          file = "./results/search_aux_vars/potential_num_aux_vars.csv",
          row.names = FALSE)
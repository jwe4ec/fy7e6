# ---------------------------------------------------------------------------- #
# Further Clean Other Data
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# TODO: Compute training noncompletion





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
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate_clean_further/dat2.RData")
load("./data/temp/compl_itt_unrestricted.RData")

# ---------------------------------------------------------------------------- #
# Restrict tables ----
# ---------------------------------------------------------------------------- #

# Tables for substantive analysis:
#   - "rr" (pos and neg bias), "bbsiq" (pos and neg bias, multiple entries)
#   - "oa" (anxiety, multiple entries) and "dass21_as" (anxiety)
#   - "completion" (training session completion)
#   - "demographics" ("income", "age")
# Tables for baseline characteristics:
#   - "dass21_as", "demographics"
# Tables for potential auxiliary variables:
#   - "demographics", "credibility" (training confidence, change importance), 
#     "task_log" ("device"--note: not recorded at "Eligibility")
# Support tables
#   - "participant" and "study"

dat3 <- dat2[c("angular_training", "bbsiq", "completion", "credibility", "dass21_as", 
               "demographics", "oa", "participant", "rr", "study", "task_log")]

# ---------------------------------------------------------------------------- #
# Define scale items ----
# ---------------------------------------------------------------------------- #

# Define items for RR, BBSIQ, OASIS, and DASS-21-AS

rr_scenarios <- c("blood_test", "elevator", "job", "lunch", "meeting_friend",
                  "noise", "scrape", "shopping", "wedding")

rr_neg_threat_items <-    paste0(rr_scenarios, "_ns")
rr_neg_nonthreat_items <- paste0(rr_scenarios, "_nf")
rr_pos_threat_items <-    paste0(rr_scenarios, "_ps")
rr_pos_nonthreat_items <- paste0(rr_scenarios, "_pf")

rr_items <- c(rr_neg_threat_items, rr_neg_nonthreat_items, 
              rr_pos_threat_items, rr_pos_nonthreat_items)

length(rr_neg_threat_items) == 9    # Analyze for negative threat-relevant bias
length(rr_neg_nonthreat_items) == 9
length(rr_pos_threat_items) == 9    # Analyze for positive threat-relevant bias
length(rr_pos_nonthreat_items) == 9

bbsiq_neg_int_items <- 
  c("breath_suffocate", "vision_illness", "lightheaded_faint", "chest_heart", 
    "heart_wrong", "confused_outofmind", "dizzy_ill")
bbsiq_neg_ext_items <- 
  c("visitors_bored", "shop_irritating", "smoke_house", "friend_incompetent", 
    "jolt_burglar", "party_boring", "urgent_died")
bbsiq_neg_items <- c(bbsiq_neg_int_items, bbsiq_neg_ext_items)

bbsiq_ben_int_items <- 
  c("breath_flu", "breath_physically", "vision_glasses", "vision_strained",
    "lightheaded_eat", "lightheaded_sleep", "chest_indigestion", "chest_sore",
    "heart_active", "heart_excited", "confused_cold", "confused_work",
    "dizzy_ate", "dizzy_overtired")
bbsiq_ben_ext_items <- 
  c("visitors_engagement", "visitors_outstay", "shop_bored", "shop_concentrating",
    "smoke_cig", "smoke_food", "friend_helpful", "friend_moreoften", "jolt_dream",
    "jolt_wind", "party_hear", "party_preoccupied", "urgent_bill", "urgent_junk")
bbsiq_ben_items <- c(bbsiq_ben_int_items, bbsiq_ben_ext_items)

bbsiq_items <- c(bbsiq_neg_items, bbsiq_ben_items)

length(bbsiq_neg_int_items) == 7
length(bbsiq_neg_ext_items) == 7
length(bbsiq_neg_items) == 14     # Analyze for negative bias

length(bbsiq_ben_int_items) == 14
length(bbsiq_ben_ext_items) == 14
length(bbsiq_ben_items) == 28     # Analyze for benign bias

oa_items <- c("axf", "axs", "avo", "wrk", "soc")

length(oa_items) == 5

dass21_as_items <- c("bre", "dry", "hea", "pan", "sca", "tre", "wor")
dass21_as_mean_items <- paste0(dass21_as_items, "_mean")

length(dass21_as_items) == 7

# ---------------------------------------------------------------------------- #
# Handle repeated screenings ----
# ---------------------------------------------------------------------------- #

# Centralized Calm Thinking data cleaning revealed that "dass21_as" table has 
# repeated attempts at screening for some participants. Participants with more 
# than two unique sets of item responses will be excluded from analysis, and 
# "dass21_as_total_anal" (i.e., total score based on row mean of available column 
# means of available multiple item responses) will serve as basis for analysis.

# View(dat2$dass21_as[!is.na(dat2$dass21_as$n_eligibility_rows) &
#                              dat2$dass21_as$n_eligibility_rows > 1, ])

# Remove attempt-specific columns except "X", "id", and date-related columns

dass21_as_exclude_cols <- c("session_and_eligibility_status", "time_on_page",
                            "over18", "dass21_as_total", "dass21_as_total_interp",
                            "dass21_as_eligible")

dat3$dass21_as <- dat3$dass21_as[, !(names(dat3$dass21_as) %in% dass21_as_exclude_cols)]

# Temporarily remove original items

tmp_dass21_as <- dat3$dass21_as[, !(names(dat3$dass21_as) %in% dass21_as_items)]

# For duplicated rows on "participant_id" and "session_only" when excluding the
# original items above, keep only last row

nrow(tmp_dass21_as) == nrow(dat3$dass21_as)

dat3$dass21_as <- 
  dat3$dass21_as[!duplicated(tmp_dass21_as[, c("participant_id", "session_only")], 
                             fromLast = TRUE), ]

# Recode original items as NA at "Eligibility" given that items with "_mean"
# appended are the ones that comprise "dass21_as_total_anal" at that time point

dat3$dass21_as[dat3$dass21_as$session_only == "Eligibility", dass21_as_items] <- NA

# ---------------------------------------------------------------------------- #
# Handle unexpected multiple entries ----
# ---------------------------------------------------------------------------- #

# Centralized Calm Thinking data cleaning revealed that "bbsiq" and "oa" tables 
# have unexpected multiple entries but identical item responses (just different 
# "time_on_page", so use "time_on_page_mean" for analysis) and that "task_log" 
# table has unexpected multiple entries (some reflecting repeat screenings for 
# "dass21_as" table; so use "time_on_task_mean" for analysis). Note: Centralized
# cleaning did not check "angular_training" table for unexpected multiple entries.

# View(dat2$bbsiq[dat2$bbsiq$n_rows > 1, ])
# View(dat2$oa[dat2$oa$n_rows > 1, ])
# View(dat2$task_log[dat2$task_log$n_rows > 1, ])

# Remove attempt-specific columns except "X", "id", and date-related columns from
# "bbsiq" and "oa" tables

dat3$bbsiq$time_on_page <- NULL
dat3$oa$time_on_page <- NULL

# For duplicated rows on "participant_id" and "session_only" for "bbsiq" and "oa"
# tables, keep only last row. Leave "task_log" as it is.

dat3$bbsiq <- dat3$bbsiq[!duplicated(dat3$bbsiq[, c("participant_id", "session_only")], 
                                     fromLast = TRUE), ]
dat3$oa <-    dat3$oa[!duplicated(dat3$oa[, c("participant_id", "session_only")], 
                                  fromLast = TRUE), ]

# ---------------------------------------------------------------------------- #
# Compute "income_dollar" ----
# ---------------------------------------------------------------------------- #

# Compute "income_dollar" to treat ordinal "income" levels as continuous. Use
# midpoint of each level's range (but lower limit of highest level).

tempDem <- dat3$demographics[, c("participant_id", "age", "income")]

tempDem$income_dollar <- NA
tempDem$income_dollar[tempDem$income %in% c("Unknown", 
                                            "Prefer not to answer")] <- NA
tempDem$income_dollar[tempDem$income == "Less than $5,000"]          <- 2500
tempDem$income_dollar[tempDem$income == "$5,000 through $11,999"]    <- 8500
tempDem$income_dollar[tempDem$income == "$12,000 through $15,999"]   <- 14000
tempDem$income_dollar[tempDem$income == "$16,000 through $24,999"]   <- 20500
tempDem$income_dollar[tempDem$income == "$25,000 through $34,999"]   <- 30000
tempDem$income_dollar[tempDem$income == "$35,000 through $49,999"]   <- 42500
tempDem$income_dollar[tempDem$income == "$50,000 through $74,999"]   <- 62500
tempDem$income_dollar[tempDem$income == "$75,000 through $99,999"]   <- 87500
tempDem$income_dollar[tempDem$income == "$100,000 through $149,999"] <- 125000
tempDem$income_dollar[tempDem$income == "$150,000 through $199,999"] <- 175000
tempDem$income_dollar[tempDem$income == "$200,000 through $249,999"] <- 225000
tempDem$income_dollar[tempDem$income == "$250,000 or greater"]       <- 250000

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values ----
# ---------------------------------------------------------------------------- #

# Check for outcomes. No relevant tables already have NA values except "dass21_as". 
# For "dass21_as" at "Eligibility", 555 ("prefer not to answer") was already recoded 
# to NA for "_mean" items, and original items were recoded as NA. For "dass21_as" at 
# other time points, "_mean" items are NA (as they were computed only at "Eligibility").

sum(is.na(dat3$rr[, rr_items])) == 0
sum(is.na(dat3$bbsiq[, bbsiq_items])) == 0
sum(is.na(dat3$oa[, oa_items])) == 0

sum(is.na(dat3$dass21_as[dat3$dass21_as$session_only != "Eligibility", dass21_as_items]))
sum(is.na(dat3$dass21_as[dat3$dass21_as$session_only == "Eligibility", dass21_as_mean_items]))

sum(dat3$rr[, rr_items] == 555, na.rm = TRUE)
sum(dat3$bbsiq[, bbsiq_items] == 555, na.rm = TRUE)
sum(dat3$oa[, oa_items] == 555, na.rm = TRUE)

sum(dat3$dass21_as[, dass21_as_items] == 555, na.rm = TRUE)
sum(dat3$dass21_as[, dass21_as_mean_items] == 555, na.rm = TRUE)

dat3$rr[, rr_items][dat3$rr[, rr_items] == 555] <- NA
dat3$bbsiq[, bbsiq_items][dat3$bbsiq[, bbsiq_items] == 555] <- NA
dat3$oa[, oa_items][dat3$oa[, oa_items] == 555] <- NA

dat3$dass21_as[, dass21_as_items][dat3$dass21_as[, dass21_as_items] == 555] <- NA

# Check for covariates. "Prefer not to answer" already recoded as NA.

sum(tempDem$age == 555, na.rm = TRUE) == 0
sum(tempDem$income_dollar == 555, na.rm = TRUE) == 0

# Check for auxiliary variables. "Prefer not to answer" already recoded as NA except
# for "device_col_bin", which has no such values.

table(compl_itt_unrestricted$gender_col, useNA = "always")
table(compl_itt_unrestricted$device_col_bin, useNA = "always")

table(compl_itt_unrestricted$employment_stat_col, useNA = "always")
table(compl_itt_unrestricted$marital_stat_col, useNA = "always")
sum(compl_itt_unrestricted$confident_online == 555, na.rm = TRUE) == 0
sum(compl_itt_unrestricted$important == 555, na.rm = TRUE) == 0

# ---------------------------------------------------------------------------- #
# Check response ranges ----
# ---------------------------------------------------------------------------- #

# Check for outcomes

all(sort(unique(as.vector(as.matrix(dat3$rr[, rr_items])))) %in% 1:4)
all(sort(unique(as.vector(as.matrix(dat3$bbsiq[, bbsiq_items])))) %in% 0:4)
all(sort(unique(as.vector(as.matrix(dat3$oa[, oa_items])))) %in% 0:4)

all(sort(unique(as.vector(as.matrix(dat3$dass21_as[, dass21_as_items])))) %in% 0:3)

dass21_as_mean_range <- 
  range(sort(unique(as.vector(as.matrix(dat3$dass21_as[, dass21_as_mean_items])))))
all(dass21_as_mean_range >= 0 & dass21_as_mean_range <= 3)

# Check for covariates

all(sort(unique(tempDem$age)) %in% 18:75)
table(tempDem$income_dollar)

# Check for continuous auxiliary variables (categorical variables already checked)

all(sort(unique(compl_itt_unrestricted$confident_online)) %in% 0:4)
all(sort(unique(compl_itt_unrestricted$important)) %in% 0:4)

# ---------------------------------------------------------------------------- #
# Compute average item scores ----
# ---------------------------------------------------------------------------- #

dat3$rr$rr_neg_threat_m <- rowMeans(dat3$rr[, rr_neg_threat_items], na.rm = TRUE)
dat3$rr$rr_pos_threat_m <- rowMeans(dat3$rr[, rr_pos_threat_items], na.rm = TRUE)
dat3$bbsiq$bbsiq_neg_m <- rowMeans(dat3$bbsiq[, bbsiq_neg_items], na.rm = TRUE)
dat3$bbsiq$bbsiq_ben_m <- rowMeans(dat3$bbsiq[, bbsiq_ben_items], na.rm = TRUE)
dat3$oa$oa_m <- rowMeans(dat3$oa[, oa_items], na.rm = TRUE)

# Recode "dass21_as_total_anal" (total score) into average item score

length(dass21_as_items) == 7
dat3$dass21_as$dass21_as_m <- dat3$dass21_as$dass21_as_total_anal / 7

# Change NaN values (occur when all items are NA) to NA. This was already done
# for "dass21_as_total_anal" used to compute "dass21_as_m".

dat3$rr$rr_neg_threat_m[is.nan(dat3$rr$rr_neg_threat_m)] <- NA
dat3$rr$rr_pos_threat_m[is.nan(dat3$rr$rr_pos_threat_m)] <- NA
dat3$bbsiq$bbsiq_neg_m[is.nan(dat3$bbsiq$bbsiq_neg_m)] <- NA
dat3$bbsiq$bbsiq_ben_m[is.nan(dat3$bbsiq$bbsiq_ben_m)] <- NA
dat3$oa$oa_m[is.nan(dat3$oa$oa_m)] <- NA

sum(is.nan(dat3$dass21_as$dass21_as_m)) == 0

# ---------------------------------------------------------------------------- #
# Collapse "Eligibility" and "preTest" into "baseline" ----
# ---------------------------------------------------------------------------- #

target_dfs <- c("rr", "bbsiq", "oa", "dass21_as")

for (i in 1:length(dat3)) {
  if (names(dat3)[i] %in% target_dfs) {
    dat3[[i]][, "session_only_col"] <- dat3[[i]][, "session_only"]
    dat3[[i]][, "session_only_col"][dat3[[i]][, "session_only_col"] %in% 
                                      c("Eligibility", "preTest")] <- "baseline"
  }
}

# ---------------------------------------------------------------------------- #
# Combine relevant time-varying columns into one long-format table ----
# ---------------------------------------------------------------------------- #

# Create template data frame

participant_ids <- sort(unique(compl_itt_unrestricted$participant_id))
N <- length(participant_ids)
sessions_col <- c("baseline", 
                  paste0(c("first", "second", "third", "fourth", "fifth"), "Session"),
                  "PostFollowUp")
J <- length(sessions_col)

anlys_df <- data.frame(participant_id = rep(participant_ids,
                                            each = length(sessions_col)),
                       session_only_col = rep(sessions_col, N),
                       j = rep(1:J, N))

# Add outcomes

index_vars <- c("participant_id", "session_only_col")

anlys_df <- merge(anlys_df,
                  dat3$rr[, c(index_vars, "rr_neg_threat_m", "rr_pos_threat_m")],
                  by = index_vars, all.x = TRUE)
anlys_df <- merge(anlys_df,
                  dat3$bbsiq[, c(index_vars, "bbsiq_neg_m", "bbsiq_ben_m")],
                  by = index_vars, all.x = TRUE)
anlys_df <- merge(anlys_df,
                  dat3$oa[, c(index_vars, "oa_m")],
                  by = index_vars, all.x = TRUE)
anlys_df <- merge(anlys_df,
                  dat3$dass21_as[, c(index_vars, "dass21_as_m")],
                  by = index_vars, all.x = TRUE)

# Sort table

anlys_df <- anlys_df[order(anlys_df$participant_id, anlys_df$j), ]

# ---------------------------------------------------------------------------- #
# Convert to wide format ----
# ---------------------------------------------------------------------------- #

ignore_vars <- "participant_id"
varying_vars <- setdiff(names(anlys_df), ignore_vars)

anlys_df_wd <- reshape(anlys_df,
                       v.names = varying_vars,
                       timevar = "j",
                       idvar = "participant_id",
                       direction = "wide")

# ---------------------------------------------------------------------------- #
# Add time-invariant variables ----
# ---------------------------------------------------------------------------- #

# Add condition, covariates, and auxiliary variables

target_vars <- c("participant_id", "conditioning", "exclude_analysis", 
                 "itt_anlys", "s5_train_compl_anlys_uncorrected_c1", 
                 "class_meas_compl_anlys", "s5_train_compl_anlys_c2_4",
                 "gender_col", "device_col_bin",
                 "employment_stat_col", "marital_stat_col", 
                 "confident_online", "important")

anlys_df_wd <- merge(anlys_df_wd,
                     compl_itt_unrestricted[, target_vars],
                     by = "participant_id", all.x = TRUE)
anlys_df_wd <- merge(anlys_df_wd,
                     tempDem[, c("participant_id", "age", "income_dollar")],
                     by = "participant_id", all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Define "a1" and "a2" contrasts ----
# ---------------------------------------------------------------------------- #

anlys_df_wd$a1 <- NA
anlys_df_wd$a1[anlys_df_wd$conditioning ==     "NONE"]           <- NA
anlys_df_wd$a1[anlys_df_wd$conditioning %in% c("TRAINING",
                                               "LR_TRAINING",
                                               "HR_NO_COACH")]   <- 1
anlys_df_wd$a1[anlys_df_wd$conditioning ==     "HR_COACH"]       <- 0
anlys_df_wd$a1[anlys_df_wd$conditioning ==     "CONTROL"]        <- -1

anlys_df_wd$a2_1 <- NA
anlys_df_wd$a2_1[anlys_df_wd$conditioning %in% c("NONE",
                                                 "TRAINING")]    <- NA
anlys_df_wd$a2_1[anlys_df_wd$conditioning ==     "LR_TRAINING"]  <- 0
anlys_df_wd$a2_1[anlys_df_wd$conditioning ==     "HR_NO_COACH"]  <- -1
anlys_df_wd$a2_1[anlys_df_wd$conditioning ==     "HR_COACH"]     <- 1
anlys_df_wd$a2_1[anlys_df_wd$conditioning ==     "CONTROL"]      <- 0

anlys_df_wd$a2_2 <- NA
anlys_df_wd$a2_2[anlys_df_wd$conditioning %in% c("NONE",
                                                 "TRAINING")]    <- NA
anlys_df_wd$a2_2[anlys_df_wd$conditioning ==     "LR_TRAINING"]  <- -1
anlys_df_wd$a2_2[anlys_df_wd$conditioning ==     "HR_NO_COACH"]  <- 0
anlys_df_wd$a2_2[anlys_df_wd$conditioning ==     "HR_COACH"]     <- 1
anlys_df_wd$a2_2[anlys_df_wd$conditioning ==     "CONTROL"]      <- 0

anlys_df_wd$a2_3 <- NA
anlys_df_wd$a2_3[anlys_df_wd$conditioning %in% c("NONE",
                                                 "TRAINING")]    <- NA
anlys_df_wd$a2_3[anlys_df_wd$conditioning ==     "LR_TRAINING"]  <- -1
anlys_df_wd$a2_3[anlys_df_wd$conditioning ==     "HR_NO_COACH"]  <- 1
anlys_df_wd$a2_3[anlys_df_wd$conditioning ==     "HR_COACH"]     <- 0
anlys_df_wd$a2_3[anlys_df_wd$conditioning ==     "CONTROL"]      <- 0

# ---------------------------------------------------------------------------- #
# Correct ITT sample for Comparison 1 ----
# ---------------------------------------------------------------------------- #

# Restrict to ITT participants (uncorrected as this includes "HR_COACH" participants
# and has original sample size for "HR_NO_COACH")

wd_c1_uncorr_itt <- anlys_df_wd[anlys_df_wd$itt_anlys == 1, ]

# Confirm number of ITT participants in "HR_COACH"

n_itt_hr_coach <- 
  length(unique(wd_c1_uncorr_itt$participant_id[wd_c1_uncorr_itt$conditioning == "HR_COACH"]))
n_itt_hr_coach == 281

# Define function to correct "HR_NO_COACH" sample size in ITT sample for Comparison 1
# by (a) removing "HR_COACH" participants, (b) resampling "HR_NO_COACH" participants 281 
# times (number of "HR_COACH" participants) with replacement given < 281 participants 
# originally in "HR_NO_COACH"), and (c) adding resampled rows to original data frame

bootstrap <- function(df, n_hr_coach) {
  df <- df[df$conditioning != "HR_COACH", ]
  
  df_hr_no_coach <- df[df$conditioning == "HR_NO_COACH", ]
  df_resample <- df_hr_no_coach[sample(nrow(df_hr_no_coach), 
                                       n_hr_coach,
                                       replace = TRUE), ]
  
  df_combined <- rbind(df, df_resample)
}

# Run function 500 times and put resulting data frames in list

wd_c1_corr_itt <- replicate(500,
                            bootstrap(wd_c1_uncorr_itt, n_itt_hr_coach),
                            simplify = FALSE)

# ---------------------------------------------------------------------------- #
# Restrict to analysis samples ----
# ---------------------------------------------------------------------------- #

# Corrected ITT sample for Comparison 1 is "wd_c1_corr_itt"

# Corrected Session 5 training completer sample for Comparison 1

wd_c1_corr_s5_train_compl <- lapply(wd_c1_corr_itt, function(x) {
  x[x$s5_train_compl_anlys_uncorrected_c1 == 1, ]
})

# Classification measure completer sample for Comparisons 2-4

wd_c2_4_class_meas_compl <- anlys_df_wd[anlys_df_wd$class_meas_compl_anlys == 1, ]

# Session 5 training completer sample for Comparisons 2-4

wd_c2_4_s5_train_compl <- anlys_df_wd[anlys_df_wd$s5_train_compl_anlys_c2_4 == 1, ]

# ---------------------------------------------------------------------------- #
# Grand-mean-center continuous predictors ----
# ---------------------------------------------------------------------------- #

# Define function to grand-mean-center continuous predictors (covariates "age" and 
# "income_dollar" and auxiliary variables "confident_online" and "important")

grand_mean_center <- function(df) {
  df$income_dollar_ctr <- df$income_dollar - mean(df$income_dollar, na.rm = TRUE)
  df$age_ctr <- df$age - mean(df$age, na.rm = TRUE)
  
  df$confident_online_ctr <- df$confident_online - mean(df$confident_online, na.rm = TRUE)
  df$important_ctr <- df$important - mean(df$important, na.rm = TRUE)
  
  return(df)
}

# Run function on each analysis dataset

wd_c1_corr_itt            <- lapply(wd_c1_corr_itt, grand_mean_center)
wd_c1_corr_s5_train_compl <- lapply(wd_c1_corr_s5_train_compl, grand_mean_center)

wd_c2_4_class_meas_compl <- grand_mean_center(wd_c2_4_class_meas_compl)
wd_c2_4_s5_train_compl   <- grand_mean_center(wd_c2_4_s5_train_compl)

# Remove unneeded "a1" and "a2" columns

unneeded_a2 <- c("a2_1", "a2_2", "a2_3")

wd_c1_corr_itt <-
  lapply(wd_c1_corr_itt,            function(x) { x[, !(names(x) %in% unneeded_a2)]})
wd_c1_corr_s5_train_compl <-
  lapply(wd_c1_corr_s5_train_compl, function(x) { x[, !(names(x) %in% unneeded_a2)]})

wd_c2_4_class_meas_compl$a1 <- NULL
wd_c2_4_s5_train_compl$a1 <- NULL

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/final_clean")

save(wd_c1_corr_itt,            file = "./data/final_clean/wd_c1_corr_itt.RData")
save(wd_c1_corr_s5_train_compl, file = "./data/final_clean/wd_c1_corr_s5_train_compl.RData")

save(wd_c2_4_class_meas_compl,  file = "./data/final_clean/wd_c2_4_class_meas_compl.RData")
save(wd_c2_4_s5_train_compl,    file = "./data/final_clean/wd_c2_4_s5_train_compl.RData")
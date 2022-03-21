# ---------------------------------------------------------------------------- #
# Further Clean Other Data
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
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate_clean_further/dat2.RData")

# ---------------------------------------------------------------------------- #
# Restrict tables ----
# ---------------------------------------------------------------------------- #

# TODO: Identify relevant tables. Asked Sonia what variable she has used to look
# at actual device usage on 1/25/22.

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
# Handle repeated screenings ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





# Centralized data cleaning revealed that "dass21_as" table has repeated attempts
# at screening for some participants. Participants with more than two unique sets
# of item responses will be excluded from analysis, and "dass21_as_total_anal"
# (i.e., total score based on row mean of available column means of available 
# multiple item responses) will serve as the basis for analysis.

# View(dat2$dass21_as[!is.na(dat2$dass21_as$n_eligibility_rows) &
#                              dat2$dass21_as$n_eligibility_rows > 1, ])

# Remove attempt-specific columns except "X", "id", and date-related columns

dass21_as_exclude_cols <- c("session_and_eligibility_status", "time_on_page",
                            "bre", "dry", "hea", "pan", "sca", "tre", "wor",
                            "over18", "dass21_as_total", "dass21_as_total_interp",
                            "dass21_as_eligible")

dat3$dass21_as <- dat3$dass21_as[, !(names(dat3$dass21_as) %in% dass21_as_exclude_cols)]

# For duplicated rows on "participant_id" and "session_only", keep only last row

dat3$dass21_as <- 
  dat3$dass21_as[!duplicated(dat3$dass21_as[, c("participant_id", "session_only")], 
                             fromLast = TRUE), ]

# ---------------------------------------------------------------------------- #
# Handle unexpected multiple entries ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





# Centralized data cleaning revealed that "bbsiq" and "oa" tables have unexpected
# multiple entries but identical item responses (just different "time_on_page", so 
# use "time_on_page_mean" for analysis) and that "task_log" table has unexpected
# multiple entries (some reflecting repeat screenings for "dass21_as" table; so 
# use "time_on_task_mean" for analysis). Note that "angular_training" table was 
# not checked for unexpected multiple entries in centralized data cleaning.

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
# Conduct further cleaning (ACTUAL) ----
# ---------------------------------------------------------------------------- #

# TODO: Handle "prefer not to answer"
# TODO: Check response ranges
# TODO: Score scales and compute training noncompletion
# TODO: Define j, S1, S2, S3, a1, and a2
# TODO: Add condition and covariates
# TODO: Restrict to analysis samples
# TODO: Center predictors





# ---------------------------------------------------------------------------- #
# Conduct further cleaning for "demographics" table (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





# Compute "income_dollar" to treat ordinal "income" levels as continuous

tempDem <- dat3$demographics[, c("participant_id", "age", "income")]

table(tempDem$income)

tempDem$income_dollar <- NA
tempDem$income_dollar[tempDem$income %in% c("Unknown", "Prefer not to answer")] <- NA
tempDem$income_dollar[tempDem$income == "Less than $5,000"] <- 2500
tempDem$income_dollar[tempDem$income == "$5,000 through $11,999"] <- 8500
tempDem$income_dollar[tempDem$income == "$12,000 through $15,999"] <- 14000
tempDem$income_dollar[tempDem$income == "$16,000 through $24,999"] <- 20500
tempDem$income_dollar[tempDem$income == "$25,000 through $34,999"] <- 30000
tempDem$income_dollar[tempDem$income == "$35,000 through $49,999"] <- 42500
tempDem$income_dollar[tempDem$income == "$50,000 through $74,999"] <- 62500
tempDem$income_dollar[tempDem$income == "$75,000 through $99,999"] <- 87500
tempDem$income_dollar[tempDem$income == "$100,000 through $149,999"] <- 125000
tempDem$income_dollar[tempDem$income == "$150,000 through $199,999"] <- 175000
tempDem$income_dollar[tempDem$income == "$200,000 through $249,999"] <- 225000
tempDem$income_dollar[tempDem$income == "$250,000 or greater"] <- 250000

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" for "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





sum(dat3$bbsiq[, 6:47] == 555)
sum(dat3$bbsiq[, 6:47] == -1)
sum(dat3$oa[, 7:11] == 555)
sum(dat3$oa[, 7:11] == -1)

dat3$bbsiq[, 6:47][dat3$bbsiq[, 6:47] == 555] <- NA
dat3$oa[, 7:11][dat3$oa[, 7:11] == 555] <- NA

# ---------------------------------------------------------------------------- #
# Score "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





dat3$bbsiq$neg_bias <- 
  rowMeans(dat3$bbsiq[, c("breath_suffocate", "chest_heart", "confused_outofmind", 
                          "dizzy_ill", "friend_incompetent", "heart_wrong", 
                          "jolt_burglar", "lightheaded_faint", "party_boring", 
                          "shop_irritating", "smoke_house", "urgent_died", 
                          "vision_illness", "visitors_bored")],
           na.rm = TRUE)

dat3$oa$anxiety_scale <- 
  rowMeans(dat3$oa[, c("axf", "axs", "avo", "wrk", "soc")],
           na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Add age and income to "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





dat3$bbsiq <- merge(dat3$bbsiq, tempDem, by = "participant_id", 
                    all.x = TRUE)
dat3$oa <- merge(dat3$oa, tempDem, by = "participant_id", 
                 all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Define "a1" and "a2" contrasts (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below (especially check values of NA)





dat3$study$a1_1 <- NA
dat3$study$a1_1[dat3$study$conditioning ==     "NONE"]         <- NA
dat3$study$a1_1[dat3$study$conditioning %in% c("TRAINING",
                                               "LR_TRAINING",
                                               "HR_NO_COACH")] <- 1
dat3$study$a1_1[dat3$study$conditioning ==     "HR_COACH"]     <- 0
dat3$study$a1_1[dat3$study$conditioning ==     "CONTROL"]      <- -1

dat3$study$a1_2 <- NA
dat3$study$a1_2[dat3$study$conditioning %in% c("NONE",
                                               "TRAINING")]    <- NA
dat3$study$a1_2[dat3$study$conditioning %in% c("LR_TRAINING",
                                               "HR_NO_COACH",
                                               "HR_COACH")]    <- 1
dat3$study$a1_2[dat3$study$conditioning ==     "CONTROL"]      <- -1

dat3$study$a2_1 <- NA
dat3$study$a2_1[dat3$study$conditioning %in% c("NONE",
                                               "TRAINING")]    <- NA
dat3$study$a2_1[dat3$study$conditioning ==     "LR_TRAINING"]  <- 0
dat3$study$a2_1[dat3$study$conditioning ==     "HR_NO_COACH"]  <- -1
dat3$study$a2_1[dat3$study$conditioning ==     "HR_COACH"]     <- 1
dat3$study$a2_1[dat3$study$conditioning ==     "CONTROL"]      <- 0

dat3$study$a2_2 <- NA
dat3$study$a2_2[dat3$study$conditioning %in% c("NONE",
                                               "TRAINING")]    <- NA
dat3$study$a2_2[dat3$study$conditioning ==     "LR_TRAINING"]  <- -1
dat3$study$a2_2[dat3$study$conditioning ==     "HR_NO_COACH"]  <- 0
dat3$study$a2_2[dat3$study$conditioning ==     "HR_COACH"]     <- 1
dat3$study$a2_2[dat3$study$conditioning ==     "CONTROL"]      <- 0

dat3$study$a2_3 <- NA
dat3$study$a2_3[dat3$study$conditioning %in% c("NONE",
                                               "TRAINING")]    <- NA
dat3$study$a2_3[dat3$study$conditioning ==     "LR_TRAINING"]  <- -1
dat3$study$a2_3[dat3$study$conditioning ==     "HR_NO_COACH"]  <- 1
dat3$study$a2_3[dat3$study$conditioning ==     "HR_COACH"]     <- 0
dat3$study$a2_3[dat3$study$conditioning ==     "CONTROL"]      <- 0

# ---------------------------------------------------------------------------- #
# Add condition contrasts to "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





tempStudy <- dat3$study[, c("participant_id", "conditioning",
                            "a1_1", "a1_2", "a2_1", "a2_2", "a2_3")]

dat3$bbsiq <- merge(dat3$bbsiq, tempStudy, by = "participant_id", 
                    all.x = TRUE)
dat3$oa <- merge(dat3$oa, tempStudy, by = "participant_id", 
                 all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Define "j" in "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





# Define "j"

dat3$bbsiq$j <- dat3$bbsiq$session_only
dat3$bbsiq[dat3$bbsiq$j == "preTest", ]$j <- 1
dat3$bbsiq[dat3$bbsiq$j == "thirdSession", ]$j <- 4
dat3$bbsiq[dat3$bbsiq$j == "fifthSession", ]$j <- 6
dat3$bbsiq[dat3$bbsiq$j == "PostFollowUp", ]$j <- 7
dat3$bbsiq$j <- as.integer(dat3$bbsiq$j)
table(dat3$bbsiq$j, useNA = "always")

dat3$oa$j <- dat3$oa$session_only
dat3$oa[dat3$oa$j == "preTest", ]$j <- 1
dat3$oa[dat3$oa$j == "firstSession", ]$j <- 2
dat3$oa[dat3$oa$j == "secondSession", ]$j <- 3
dat3$oa[dat3$oa$j == "thirdSession", ]$j <- 4
dat3$oa[dat3$oa$j == "fourthSession", ]$j <- 5
dat3$oa[dat3$oa$j == "fifthSession", ]$j <- 6
dat3$oa[dat3$oa$j == "PostFollowUp", ]$j <- 7
dat3$oa$j <- as.integer(dat3$oa$j)
table(dat3$oa$j, useNA = "always")

# Sort tables

dat3$bbsiq <- dat3$bbsiq[order(dat3$bbsiq$participant_id, dat3$bbsiq$j), ]
dat3$oa <- dat3$oa[order(dat3$oa$participant_id, dat3$oa$j), ]

# ---------------------------------------------------------------------------- #
# Define "S1", "S2", and "S3" in "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





dat3$bbsiq$S1 <- dat3$bbsiq$j
dat3$bbsiq[dat3$bbsiq$j == 1, ]$S1 <- 0
dat3$bbsiq[dat3$bbsiq$j == 4, ]$S1 <- 1
dat3$bbsiq[dat3$bbsiq$j == 6, ]$S1 <- 1
dat3$bbsiq[dat3$bbsiq$j == 7, ]$S1 <- 1

dat3$bbsiq$S2 <- dat3$bbsiq$j
dat3$bbsiq[dat3$bbsiq$j == 1, ]$S2 <- 0
dat3$bbsiq[dat3$bbsiq$j == 4, ]$S2 <- 2
dat3$bbsiq[dat3$bbsiq$j == 6, ]$S2 <- 4
dat3$bbsiq[dat3$bbsiq$j == 7, ]$S2 <- 4

dat3$bbsiq$S3 <- dat3$bbsiq$j
dat3$bbsiq[dat3$bbsiq$j == 1, ]$S3 <- 0
dat3$bbsiq[dat3$bbsiq$j == 4, ]$S3 <- 0
dat3$bbsiq[dat3$bbsiq$j == 6, ]$S3 <- 0
dat3$bbsiq[dat3$bbsiq$j == 7, ]$S3 <- 1

table(dat3$bbsiq$j, useNA = "always")
table(dat3$bbsiq$j, dat3$bbsiq$S1, useNA = "always")
table(dat3$bbsiq$j, dat3$bbsiq$S2, useNA = "always")
table(dat3$bbsiq$j, dat3$bbsiq$S3, useNA = "always")

dat3$oa$S1 <- dat3$oa$j
dat3$oa[dat3$oa$j == 1, ]$S1 <- 0
dat3$oa[dat3$oa$j == 2, ]$S1 <- 1
dat3$oa[dat3$oa$j == 3, ]$S1 <- 1
dat3$oa[dat3$oa$j == 4, ]$S1 <- 1
dat3$oa[dat3$oa$j == 5, ]$S1 <- 1
dat3$oa[dat3$oa$j == 6, ]$S1 <- 1
dat3$oa[dat3$oa$j == 7, ]$S1 <- 1

dat3$oa$S2 <- dat3$oa$j
dat3$oa[dat3$oa$j == 1, ]$S2 <- 0
dat3$oa[dat3$oa$j == 2, ]$S2 <- 0
dat3$oa[dat3$oa$j == 3, ]$S2 <- 1
dat3$oa[dat3$oa$j == 4, ]$S2 <- 2
dat3$oa[dat3$oa$j == 5, ]$S2 <- 3
dat3$oa[dat3$oa$j == 6, ]$S2 <- 4
dat3$oa[dat3$oa$j == 7, ]$S2 <- 4

dat3$oa$S3 <- dat3$oa$j
dat3$oa[dat3$oa$j == 1, ]$S3 <- 0
dat3$oa[dat3$oa$j == 2, ]$S3 <- 0
dat3$oa[dat3$oa$j == 3, ]$S3 <- 0
dat3$oa[dat3$oa$j == 4, ]$S3 <- 0
dat3$oa[dat3$oa$j == 5, ]$S3 <- 0
dat3$oa[dat3$oa$j == 6, ]$S3 <- 0
dat3$oa[dat3$oa$j == 7, ]$S3 <- 1

table(dat3$oa$j, useNA = "always")
table(dat3$oa$j, dat3$oa$S1, useNA = "always")
table(dat3$oa$j, dat3$oa$S2, useNA = "always")
table(dat3$oa$j, dat3$oa$S3, useNA = "always")

# ---------------------------------------------------------------------------- #
# Restrict to analysis samples (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





itt_anlys_ids <- 
  dat3$participant$participant_id[dat3$participant$itt_anlys == 1]
# s5_train_compl_anlys_c1_ids <- 
#   dat3$participant$participant_id[dat3$participant$s5_train_compl_anlys_c2_4 == 1]

class_meas_compl_anlys_ids <- 
  dat3$participant$participant_id[dat3$participant$class_meas_compl_anlys == 1]
s5_train_compl_anlys_c2_4_ids <-
  dat3$participant$participant_id[dat3$participant$s5_train_compl_anlys_c2_4_ids == 1]


bbsiq_itt <- dat3$bbsiq[dat3$bbsiq$participant_id %in% itt_anlys_ids, ]
oa_itt <- dat3$oa[dat3$oa$participant_id %in% itt_anlys_ids, ]

bbsiq_class_meas_compl <- 
  dat3$bbsiq[dat3$bbsiq$participant_id %in% class_meas_compl_anlys_ids, ]
oa_class_meas_compl <- 
  dat3$oa[dat3$oa$participant_id %in% class_meas_compl_anlys_ids, ]

dat_anlys <- list(bbsiq_itt = bbsiq_itt,
                  oa_itt = oa_itt,
                  bbsiq_class_meas_compl = bbsiq_class_meas_compl,
                  oa_class_meas_compl = oa_class_meas_compl)

# ---------------------------------------------------------------------------- #
# Grand mean center predictors (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





for (i in 1:length(dat_anlys)) {
  df <- dat_anlys[[i]]
  
  df$income_dollar_centered <- df$income_dollar - mean(df$income_dollar, na.rm = TRUE)
  df$age_centered <- df$age - mean(df$age, na.rm = TRUE)
  
  dat_anlys[[i]] <- df
}

# ---------------------------------------------------------------------------- #
# Create bootstrap samples (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO





# ---------------------------------------------------------------------------- #
# Export data (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below




write.csv(dat_anlys$bbsiq_itt, "./data/temp/bbsiq_itt.csv")
write.csv(dat_anlys$oa_itt, "./data/temp/oa_itt.csv")

write.csv(dat_anlys$bbsiq_class_meas_compl, "./data/temp/bbsiq_class_meas_compl.csv")
write.csv(dat_anlys$oa_class_meas_compl, "./data/temp/oa_class_meas_compl.csv")
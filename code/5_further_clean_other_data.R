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

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate/dat.RData")

# ---------------------------------------------------------------------------- #
# Restrict tables ----
# ---------------------------------------------------------------------------- #

# TODO: Identify relevant tables. Asked Sonia what variable she has used to look
# at actual device usage on 1/25/22.

# Tables for substantive analysis:
#   - "rr" (pos and neg bias), "bbsiq" (pos and neg bias, multiple entries)
#   - "oa" (anxiety, multiple entries) and "dass21_as" (anxiety)
#   - "task_log" or "angular_training" (training session completion)
#   - "demographics" ("income", age computed from "birth_year")
# Tables for baseline characteristics:
#   - "dass21_as", "demographics", "demographics_race"
# Tables for potential auxiliary variables:
#   - "demographics", "credibility" (training confidence, change importance), 
#     "task_log" ("device")
# Support tables
#   - "participant" and "study"

dat2 <- dat[c("angular_training", "bbsiq", "credibility", "dass21_as", 
              "demographics", "demographics_race", "oa", "participant", "rr", 
              "study", "task_log")]





# ---------------------------------------------------------------------------- #
# Handle repeated screenings ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





# Centralized data cleaning revealed that "dass21_as" table has repeated attempts
# at screening for some participants. Participants with more than two unique sets
# of item responses will be excluded from analysis, and "dass21_as_total_anal"
# (i.e., total score based on row mean of available column means of available 
# multiple item responses) will serve as the basis for analysis.

# View(dat$dass21_as[!is.na(dat$dass21_as$n_eligibility_rows) & 
#                             dat$dass21_as$n_eligibility_rows > 1, ])

# Remove attempt-specific columns except "X", "id", and date-related columns

dass21_as_exclude_cols <- c("session_and_eligibility_status", "time_on_page",
                            "bre", "dry", "hea", "pan", "sca", "tre", "wor",
                            "over18", "dass21_as_total", "dass21_as_total_interp",
                            "dass21_as_eligible")

dat2$dass21_as <- dat2$dass21_as[, !(names(dat2$dass21_as) %in% dass21_as_exclude_cols)]

# For duplicated rows on "participant_id" and "session_only", keep only last row

dat2$dass21_as <- 
  dat2$dass21_as[!duplicated(dat2$dass21_as[, c("participant_id", "session_only")], 
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

# View(dat$bbsiq[dat$bbsiq$n_rows > 1, ])
# View(dat$oa[dat$oa$n_rows > 1, ])
# View(dat$task_log[dat$task_log$n_rows > 1, ])

# Remove attempt-specific columns except "X", "id", and date-related columns from
# "bbsiq" and "oa" tables

dat2$bbsiq$time_on_page <- NULL
dat2$oa$time_on_page <- NULL

# For duplicated rows on "participant_id" and "session_only" for "bbsiq" and "oa"
# tables, keep only last row. Leave "task_log" as it is.

dat2$bbsiq <- dat2$bbsiq[!duplicated(dat2$bbsiq[, c("participant_id", "session_only")], 
                                     fromLast = TRUE), ]
dat2$oa <-    dat2$oa[!duplicated(dat2$oa[, c("participant_id", "session_only")], 
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





# Recode "prefer not to answer" for "birth_year"

tempDem <- dat2$demographics[, c("participant_id", "birth_year", "income")]

sum(tempDem$birth_year == 555)
sum(tempDem$birth_year == -1)

tempDem[tempDem$birth_year == 555, ] <- NA

# Compute age

tempDem$age <- 2019 - tempDem$birth_year

table(tempDem$age, useNA = "always")

# Recode income

table(tempDem$income)

tempDem$income_dollar <- NA
tempDem$income_dollar[tempDem$income == "555"] <- NA
tempDem$income_dollar[tempDem$income == "Don't know"] <- NA
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





sum(dat2$bbsiq[, 6:47] == 555)
sum(dat2$bbsiq[, 6:47] == -1)
sum(dat2$oa[, 7:11] == 555)
sum(dat2$oa[, 7:11] == -1)

dat2$bbsiq[, 6:47][dat2$bbsiq[, 6:47] == 555] <- NA
dat2$oa[, 7:11][dat2$oa[, 7:11] == 555] <- NA

# ---------------------------------------------------------------------------- #
# Score "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





dat2$bbsiq$neg_bias <- 
  rowMeans(dat2$bbsiq[, c("breath_suffocate", "chest_heart", "confused_outofmind", 
                          "dizzy_ill", "friend_incompetent", "heart_wrong", 
                          "jolt_burglar", "lightheaded_faint", "party_boring", 
                          "shop_irritating", "smoke_house", "urgent_died", 
                          "vision_illness", "visitors_bored")],
           na.rm = TRUE)

dat2$oa$anxiety_scale <- 
  rowMeans(dat2$oa[, c("axf", "axs", "avo", "wrk", "soc")],
           na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Add age and income to "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





dat2$bbsiq <- merge(dat2$bbsiq, tempDem, by = "participant_id", 
                    all.x = TRUE)
dat2$oa <- merge(dat2$oa, tempDem, by = "participant_id", 
                 all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Add condition to "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





tempStudy <- dat2$study[, c("participant_id", "conditioning")]

dat2$bbsiq <- merge(dat2$bbsiq, tempStudy, by = "participant_id", 
                    all.x = TRUE)
dat2$oa <- merge(dat2$oa, tempStudy, by = "participant_id", 
                 all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Define "j" in "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below



# Define "j"

dat2$bbsiq$j <- dat2$bbsiq$session_only
dat2$bbsiq[dat2$bbsiq$j == "preTest", ]$j <- 1
dat2$bbsiq[dat2$bbsiq$j == "thirdSession", ]$j <- 4
dat2$bbsiq[dat2$bbsiq$j == "fifthSession", ]$j <- 6
dat2$bbsiq[dat2$bbsiq$j == "PostFollowUp", ]$j <- 7
dat2$bbsiq$j <- as.integer(dat2$bbsiq$j)
table(dat2$bbsiq$j, useNA = "always")

dat2$oa$j <- dat2$oa$session_only
dat2$oa[dat2$oa$j == "preTest", ]$j <- 1
dat2$oa[dat2$oa$j == "firstSession", ]$j <- 2
dat2$oa[dat2$oa$j == "secondSession", ]$j <- 3
dat2$oa[dat2$oa$j == "thirdSession", ]$j <- 4
dat2$oa[dat2$oa$j == "fourthSession", ]$j <- 5
dat2$oa[dat2$oa$j == "fifthSession", ]$j <- 6
dat2$oa[dat2$oa$j == "PostFollowUp", ]$j <- 7
dat2$oa$j <- as.integer(dat2$oa$j)
table(dat2$oa$j, useNA = "always")

# Sort tables

dat2$bbsiq <- dat2$bbsiq[order(dat2$bbsiq$participant_id, dat2$bbsiq$j), ]
dat2$oa <- dat2$oa[order(dat2$oa$participant_id, dat2$oa$j), ]

# ---------------------------------------------------------------------------- #
# Define "S1", "S2", and "S3" in "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





dat2$bbsiq$S1 <- dat2$bbsiq$j
dat2$bbsiq[dat2$bbsiq$j == 1, ]$S1 <- 0
dat2$bbsiq[dat2$bbsiq$j == 4, ]$S1 <- 1
dat2$bbsiq[dat2$bbsiq$j == 6, ]$S1 <- 1
dat2$bbsiq[dat2$bbsiq$j == 7, ]$S1 <- 1

dat2$bbsiq$S2 <- dat2$bbsiq$j
dat2$bbsiq[dat2$bbsiq$j == 1, ]$S2 <- 0
dat2$bbsiq[dat2$bbsiq$j == 4, ]$S2 <- 2
dat2$bbsiq[dat2$bbsiq$j == 6, ]$S2 <- 4
dat2$bbsiq[dat2$bbsiq$j == 7, ]$S2 <- 4

dat2$bbsiq$S3 <- dat2$bbsiq$j
dat2$bbsiq[dat2$bbsiq$j == 1, ]$S3 <- 0
dat2$bbsiq[dat2$bbsiq$j == 4, ]$S3 <- 0
dat2$bbsiq[dat2$bbsiq$j == 6, ]$S3 <- 0
dat2$bbsiq[dat2$bbsiq$j == 7, ]$S3 <- 1

table(dat2$bbsiq$j, useNA = "always")
table(dat2$bbsiq$j, dat2$bbsiq$S1, useNA = "always")
table(dat2$bbsiq$j, dat2$bbsiq$S2, useNA = "always")
table(dat2$bbsiq$j, dat2$bbsiq$S3, useNA = "always")

dat2$oa$S1 <- dat2$oa$j
dat2$oa[dat2$oa$j == 1, ]$S1 <- 0
dat2$oa[dat2$oa$j == 2, ]$S1 <- 1
dat2$oa[dat2$oa$j == 3, ]$S1 <- 1
dat2$oa[dat2$oa$j == 4, ]$S1 <- 1
dat2$oa[dat2$oa$j == 5, ]$S1 <- 1
dat2$oa[dat2$oa$j == 6, ]$S1 <- 1
dat2$oa[dat2$oa$j == 7, ]$S1 <- 1

dat2$oa$S2 <- dat2$oa$j
dat2$oa[dat2$oa$j == 1, ]$S2 <- 0
dat2$oa[dat2$oa$j == 2, ]$S2 <- 0
dat2$oa[dat2$oa$j == 3, ]$S2 <- 1
dat2$oa[dat2$oa$j == 4, ]$S2 <- 2
dat2$oa[dat2$oa$j == 5, ]$S2 <- 3
dat2$oa[dat2$oa$j == 6, ]$S2 <- 4
dat2$oa[dat2$oa$j == 7, ]$S2 <- 4

dat2$oa$S3 <- dat2$oa$j
dat2$oa[dat2$oa$j == 1, ]$S3 <- 0
dat2$oa[dat2$oa$j == 2, ]$S3 <- 0
dat2$oa[dat2$oa$j == 3, ]$S3 <- 0
dat2$oa[dat2$oa$j == 4, ]$S3 <- 0
dat2$oa[dat2$oa$j == 5, ]$S3 <- 0
dat2$oa[dat2$oa$j == 6, ]$S3 <- 0
dat2$oa[dat2$oa$j == 7, ]$S3 <- 1

table(dat2$oa$j, useNA = "always")
table(dat2$oa$j, dat2$oa$S1, useNA = "always")
table(dat2$oa$j, dat2$oa$S2, useNA = "always")
table(dat2$oa$j, dat2$oa$S3, useNA = "always")

# ---------------------------------------------------------------------------- #
# Define "a1" and "a2" in "bbsiq" and "oa" tables (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below (especially check values of NA)




dat2$bbsiq$a1 <- dat2$bbsiq$conditioning
dat2$bbsiq[dat2$bbsiq$conditioning == "LR_TRAINING", ]$a1 <- 1
dat2$bbsiq[dat2$bbsiq$conditioning == "HR_COACH", ]$a1 <- 1
dat2$bbsiq[dat2$bbsiq$conditioning == "HR_NO_COACH", ]$a1 <- 1
dat2$bbsiq[dat2$bbsiq$conditioning == "TRAINING", ]$a1 <- NA
dat2$bbsiq[dat2$bbsiq$conditioning == "CONTROL", ]$a1 <- -1
dat2$bbsiq$a1 <- as.numeric(dat2$bbsiq$a1)

dat2$bbsiq$a2_1 <- dat2$bbsiq$conditioning
dat2$bbsiq[dat2$bbsiq$conditioning == "LR_TRAINING", ]$a2_1 <- 1
dat2$bbsiq[dat2$bbsiq$conditioning == "HR_COACH", ]$a2_1 <- 1
dat2$bbsiq[dat2$bbsiq$conditioning == "HR_NO_COACH", ]$a2_1 <- 1
dat2$bbsiq[dat2$bbsiq$conditioning == "TRAINING", ]$a2_1 <- NA
dat2$bbsiq[dat2$bbsiq$conditioning == "CONTROL", ]$a2_1 <- -1
dat2$bbsiq$a2_1 <- as.numeric(dat2$bbsiq$a2_1)

dat2$bbsiq$a2_2 <- dat2$bbsiq$conditioning
dat2$bbsiq[dat2$bbsiq$conditioning == "LR_TRAINING", ]$a2_2 <- 0
dat2$bbsiq[dat2$bbsiq$conditioning == "HR_COACH", ]$a2_2 <- 1
dat2$bbsiq[dat2$bbsiq$conditioning == "HR_NO_COACH", ]$a2_2 <- -1
dat2$bbsiq[dat2$bbsiq$conditioning == "TRAINING", ]$a2_2 <- NA
dat2$bbsiq[dat2$bbsiq$conditioning == "CONTROL", ]$a2_2 <- 0
dat2$bbsiq$a2_2 <- as.numeric(dat2$bbsiq$a2_2)

table(dat2$bbsiq$conditioning, useNA = "always")
table(dat2$bbsiq$conditioning, dat2$bbsiq$a1, useNA = "always")
table(dat2$bbsiq$conditioning, dat2$bbsiq$a2_1, useNA = "always")
table(dat2$bbsiq$conditioning, dat2$bbsiq$a2_2, useNA = "always")

dat2$oa$a1 <- dat2$oa$conditioning
dat2$oa[dat2$oa$conditioning == "LR_TRAINING", ]$a1 <- 1
dat2$oa[dat2$oa$conditioning == "HR_COACH", ]$a1 <- 1
dat2$oa[dat2$oa$conditioning == "HR_NO_COACH", ]$a1 <- 1
dat2$oa[dat2$oa$conditioning == "TRAINING", ]$a1 <- NA
dat2$oa[dat2$oa$conditioning == "CONTROL", ]$a1 <- -1
dat2$oa$a1 <- as.numeric(dat2$oa$a1)

dat2$oa$a2_1 <- dat2$oa$conditioning
dat2$oa[dat2$oa$conditioning == "LR_TRAINING", ]$a2_1 <- 1
dat2$oa[dat2$oa$conditioning == "HR_COACH", ]$a2_1 <- 1
dat2$oa[dat2$oa$conditioning == "HR_NO_COACH", ]$a2_1 <- 1
dat2$oa[dat2$oa$conditioning == "TRAINING", ]$a2_1 <- NA
dat2$oa[dat2$oa$conditioning == "CONTROL", ]$a2_1 <- -1
dat2$oa$a2_1 <- as.numeric(dat2$oa$a2_1)

dat2$oa$a2_2 <- dat2$oa$conditioning
dat2$oa[dat2$oa$conditioning == "LR_TRAINING", ]$a2_2 <- 0
dat2$oa[dat2$oa$conditioning == "HR_COACH", ]$a2_2 <- 1
dat2$oa[dat2$oa$conditioning == "HR_NO_COACH", ]$a2_2 <- -1
dat2$oa[dat2$oa$conditioning == "TRAINING", ]$a2_2 <- NA
dat2$oa[dat2$oa$conditioning == "CONTROL", ]$a2_2 <- 0
dat2$oa$a2_2 <- as.numeric(dat2$oa$a2_2)

table(dat2$oa$conditioning, useNA = "always")
table(dat2$oa$conditioning, dat2$oa$a1, useNA = "always")
table(dat2$oa$conditioning, dat2$oa$a2_1, useNA = "always")
table(dat2$oa$conditioning, dat2$oa$a2_2, useNA = "always")

# ---------------------------------------------------------------------------- #
# Restrict to session 1 assessment completers (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





s1_assess_comp_ids <- 
  dat2$participant$participant_id[dat2$participant$s1_assess_comp_anal == 1]

dat2$bbsiq <- dat2$bbsiq[dat2$bbsiq$participant_id %in% s1_assess_comp_ids, ]
dat2$oa <- dat2$oa[dat2$oa$participant_id %in% s1_assess_comp_ids, ]

# ---------------------------------------------------------------------------- #
# Remove analysis exclusions (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





# Remove (a) participants with more than two unique sets of "dass21_as" items at
# screening (i.e., where "exclude_analysis" = 1) and (b) CBM-I participants who
# completed attrition classification measures but were not classified (910, 1674)

anal_exclude_ids <- 
  dat2$participant$participant_id[dat2$participant$exclude_analysis == 1]
anal_exclude_ids <- c(anal_exclude_ids, c(910, 1674))

dat2$bbsiq <- dat2$bbsiq[!(dat2$bbsiq$participant_id %in% anal_exclude_ids), ]
dat2$oa <- dat2$oa[!(dat2$oa$participant_id %in% anal_exclude_ids), ]

# ---------------------------------------------------------------------------- #
# Grand mean center predictors (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below





dat2$bbsiq$income_dollar_centered <- 
  dat2$bbsiq$income_dollar - 
  mean(dat2$bbsiq$income_dollar, na.rm = TRUE)
dat2$bbsiq$age_centered <- 
  dat2$bbsiq$age - 
  mean(dat2$bbsiq$age, na.rm = TRUE)

dat2$oa$income_dollar_centered <- 
  dat2$oa$income_dollar - 
  mean(dat2$oa$income_dollar, na.rm = TRUE)
dat2$oa$age_centered <- 
  dat2$oa$age - 
  mean(dat2$oa$age, na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Export data (TEMP) ----
# ---------------------------------------------------------------------------- #

# TODO: Check below




write.csv(dat2$bbsiq, "./data/temp/bbsiq_session_1_ax_completers_v2.0.csv")
write.csv(dat2$oa, "./data/temp/oa_session_1_ax_completers_v2.0.csv")
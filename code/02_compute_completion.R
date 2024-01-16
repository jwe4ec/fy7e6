# ---------------------------------------------------------------------------- #
# Compute Training and Assessment Completion
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

groundhog.library(dplyr, groundhog_day)
groundhog.library(hash, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import intermediate clean data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of selected intermediate clean CSV data files and output a
# warning if they do not contain all those relevant to present manuscript

int_cln_data_dir <- paste0(wd_dir, "/data/intermediate_clean")
filenames <- list.files(int_cln_data_dir, pattern = "*.csv", full.names = FALSE)

check_relevant_files(filenames)

# Import tables into list and name tables

dat <- lapply(paste0(int_cln_data_dir, "/", filenames), read.csv)
names(dat) <- sub(".csv", "", filenames)

# Convert system-generated timestamps to POSIXct data types

dat <- convert_POSIXct(dat)

# ---------------------------------------------------------------------------- #
# Identify training completion per "task_log" table ----
# ---------------------------------------------------------------------------- #

# Identify participants who completed training at each session per "task_log" table

comp_s1_train_ids <- dat$task_log$participant_id[dat$task_log$task_name == "1"]
comp_s2_train_ids <- dat$task_log$participant_id[dat$task_log$task_name == "2"]
comp_s3_train_ids <- dat$task_log$participant_id[dat$task_log$task_name == "3"]
comp_s4_train_ids <- dat$task_log$participant_id[dat$task_log$task_name == "4"]
comp_s5_train_ids <- dat$task_log$participant_id[dat$task_log$task_name == "5"]

comp_train_ids <- list(comp_s1_train_ids = comp_s1_train_ids,
                       comp_s2_train_ids = comp_s2_train_ids,
                       comp_s3_train_ids = comp_s3_train_ids,
                       comp_s4_train_ids = comp_s4_train_ids,
                       comp_s5_train_ids = comp_s5_train_ids)

# ---------------------------------------------------------------------------- #
# Compare training completion in "task_log" vs. "angular_training" in CBM-I ----
# ---------------------------------------------------------------------------- #

# To estimate number of scenarios completed per session in "angular_training" table,
# count number of unique "step_index" values for "step_title" of "scenario"

cbm_conditions <-  c("TRAINING", "LR_TRAINING", "HR_COACH", "HR_NO_COACH")

cbm_ids <- 
  unique(dat$angular_training[dat$angular_training$conditioning %in% cbm_conditions, 
                              "participant_id"])
train_session <- paste0(c("first", "second", "third", "fourth", "fifth"), "Session")

output_cbm <- data.frame(participant_id = rep(cbm_ids, each = length(train_session)),
                         session = rep(train_session, length(cbm_ids)),
                         n_unq_step_index_for_scenarios = NA)

for (i in 1:length(cbm_ids)) {
  for (j in 1:length(train_session)) {
    output_cbm[output_cbm$participant_id == cbm_ids[i] & output_cbm$session == train_session[j],
               "n_unq_step_index_for_scenarios"] <- 
      length(unique(dat$angular_training[dat$angular_training$step_title == "scenario" &
                                           dat$angular_training$participant_id == cbm_ids[i] &
                                           dat$angular_training$session_and_task_info == train_session[j],
                                         "step_index"]))
  }
}

# Compare participants who completed training per "task_log" table against those
# who completed 40 scenarios per "angular_training" (i.e., have at least 40 unique
# values of "step_index")

comp_train_ids_cbm_diff <- data.frame(session = train_session,
                                      comp_train_ids_in_tl_not_at = NA,
                                      comp_train_ids_in_at_not_tl = NA)

for (i in 1:length(train_session)) {
  comp_train_ids_cbm_diff[comp_train_ids_cbm_diff$session == train_session[i],
                          "comp_train_ids_in_tl_not_at"] <-
    paste(setdiff(dat$study$participant_id[dat$study$participant_id %in% comp_train_ids[[i]] &
                                             dat$study$conditioning %in% cbm_conditions],
                  output_cbm$participant_id[output_cbm$session == train_session[i] &
                                              output_cbm$n_unq_step_index_for_scenarios >= 40]),
          collapse = ", ")
  comp_train_ids_cbm_diff[comp_train_ids_cbm_diff$session == train_session[i],
                          "comp_train_ids_in_at_not_tl"] <-
    paste(setdiff(output_cbm$participant_id[output_cbm$session == train_session[i] &
                                              output_cbm$n_unq_step_index_for_scenarios >= 40], 
                  dat$study$participant_id[dat$study$participant_id %in% comp_train_ids[[i]] &
                                             dat$study$conditioning %in% cbm_conditions]),
          collapse = ", ")
}

comp_train_ids_cbm_diff

# ---------------------------------------------------------------------------- #
# Investigate discrepancies in "task_log" vs. "angular_training" in CBM-I conditions ----
# ---------------------------------------------------------------------------- #

# 1. Investigate discrepancies in "comp_train_ids_in_tl_not_at" of "comp_train_ids_cbm_diff"

#   For participants where training completion in "task_log" table is not accompanied 
#   by full training data in "angular_training" table, list number of unique "step_index" 
#   values for "step_title" of "scenario" in "angular_training" table at Sessions 1-5.
#   Note: Participants without any data in "angular_training" will not be listed.

comp_train_ids_cbm_in_tl_not_at <- 
  paste(comp_train_ids_cbm_diff$comp_train_ids_in_tl_not_at, collapse = ", ")
comp_train_ids_cbm_in_tl_not_at <- 
  sort(unique(as.integer(unlist(strsplit(comp_train_ids_cbm_in_tl_not_at, split = ", ")))))

cbm_diff_report <- output_cbm[output_cbm$participant_id %in% comp_train_ids_cbm_in_tl_not_at, ]
cbm_diff_report <- cbm_diff_report[order(cbm_diff_report$participant_id), ]

#   For the following participants, "task_log" says they completed training at the 
#   given session, but "angular_training" lacks full training data

#     firstSession
#       No data at all in "angular_training" = 602
#       0  scenarios in "angular_training"   = 406, 465
#       10 scenarios in "angular_training"   = 639
#       39 scenarios in "angular_training"   = 384, 1496
#     secondSession  
#       25 scenarios in "angular_training"   = 639
#       30 scenarios in "angular_training"   = 779
#       37 scenarios in "angular_training"   = 768
#       39 scenarios in "angular_training"   = 429, 666, 916, 1756
#     thirdSession  
#       5  scenarios in "angular_training"   = 832
#       10 scenarios in "angular_training"   = 779
#       13 scenarios in "angular_training"   = 639
#       16 scenarios in "angular_training"   = 1659
#       39 scenarios in "angular_training"   = 429, 572
#     fourthSession  
#       36 scenarios in "angular_training"   = 714
#     fifthSession  
#       4  scenarios in "angular_training"   = 832

#   On 2/3/2022, Henry Behan compared "date_completed" timestamps in "task_log" for 
#   9 rows of the CBM-I participants where we have "angular_training" data for fewer 
#   than 20 scenarios and determined that enough time elapsed between the "preAffect" 
#   entry in "task_log" and the "1", "2", "3", "4", or "5" entry in "task_log" such
#   that the participant could have completed the training. We think the participant's 
#   online session may have timed out, at which point when they pressed the button to 
#   complete the training, they received the "1", "2", "3", "4", or "5" entry with a 
#   "date_completed" in "task_log" but were redirected to log in again, resulting in 
#   a loss of some or all of their "angular_training" data for the session. (A similar 
#   issue occurred for "js_psych_trial" table training data in Future Thinking study.)

#   Thus, we will consider these cases of training completion in the section "Compute
#   indicator of training completion by session" below.

# 2. Investigate discrepancies in "comp_train_ids_in_at_not_tl" of "comp_train_ids_cbm_diff"

#   For the following participants, inspection of "angular_training" reveals that 
#   they completed at least 40 scenarios at a given session ("firstSession" for 1189
#   and 1161, "secondSession" for 1872) but "task_log" does not indicate that they 
#   completed training at that session.

# View(dat$angular_training[dat$angular_training$participant_id == 1189, ])
# View(dat$angular_training[dat$angular_training$participant_id == 1161, ])
# View(dat$angular_training[dat$angular_training$participant_id == 1872, ])
# 
# View(dat$task_log[dat$task_log$participant_id == 1189, ])
# View(dat$task_log[dat$task_log$participant_id == 1161, ])
# View(dat$task_log[dat$task_log$participant_id == 1872, ])

#   Unclear why this occurred, but we will consider these cases of training completion
#   in the section "Compute indicator of training completion by session" below.

# 3. For participant 832, "task_log" shows that they completed the entire study, but 
# it lacks a "2" reflecting completion of training at "secondSession". "angular_training" 
# has data for about 10 scenarios at this session, but the "step_index" skips from 6 to 
# 25 and then ends at 33 (instead of being consecutive numbers through at least 40).

# View(dat$angular_training[dat$angular_training$participant_id == 832, ])
# View(dat$task_log[dat$task_log$participant_id == 832, ])

#   Unclear why this occurred, but we will consider "secondSession" training to have
#   been completed in this case in the section "Compute indicator of training completion 
#   by session" below.

# ---------------------------------------------------------------------------- #
# Compare training completion in "task_log" vs. "angular_training" in Psychoeducation ----
# ---------------------------------------------------------------------------- #

# To estimate participants who completed "CONTROL" training per session in 
# "angular_training" table, indicate whether last expected "step_title" is present

ctl_ids <-
  unique(dat$angular_training[dat$angular_training$conditioning %in% "CONTROL", 
                              "participant_id"])

last_step_title <- list(firstSession = "When Is Anxiety Impairing?",
                        secondSession = "Anxiety Disorders",
                        thirdSession = c("Maintenance", "Maintenance of Anxiety"),
                        fourthSession = "Other Psychological Problems",
                        fifthSession = c("Open to Others", "Being Open to Others"))

output_ctl <- data.frame(participant_id = rep(ctl_ids, each = length(train_session)),
                         session = rep(train_session, length(ctl_ids)),
                         last_step_title_present = NA)

for (i in 1:length(ctl_ids)) {
  for (j in 1:length(train_session)) {
    output_ctl[output_ctl$participant_id == ctl_ids[i] & output_ctl$session == train_session[j],
               "last_step_title_present"] <- 
      any(dat$angular_training[dat$angular_training$participant_id == ctl_ids[i] &
                                 dat$angular_training$session_and_task_info == train_session[j],
                               "step_title"] %in% last_step_title[[j]])
  }
}

# Compare participants who completed training per "task_log" table against those
# who have data for the last expected "step_title" per "angular_training"

comp_train_ids_ctl_diff <- data.frame(session = train_session,
                                      comp_train_ids_in_tl_not_at = NA,
                                      comp_train_ids_in_at_not_tl = NA)

for (i in 1:length(train_session)) {
  comp_train_ids_ctl_diff[comp_train_ids_ctl_diff$session == train_session[i],
                          "comp_train_ids_in_tl_not_at"] <-
    paste(setdiff(dat$study$participant_id[dat$study$participant_id %in% comp_train_ids[[i]] &
                                             dat$study$conditioning == "CONTROL"],
                  output_ctl$participant_id[output_ctl$session == train_session[i] &
                                              output_ctl$last_step_title_present == TRUE]),
          collapse = ", ")
  comp_train_ids_ctl_diff[comp_train_ids_ctl_diff$session == train_session[i],
                          "comp_train_ids_in_at_not_tl"] <-
    paste(setdiff(output_ctl$participant_id[output_ctl$session == train_session[i] &
                                              output_ctl$last_step_title_present == TRUE],
                  dat$study$participant_id[dat$study$participant_id %in% comp_train_ids[[i]] &
                                             dat$study$conditioning == "CONTROL"]),
          collapse = ", ")
}

comp_train_ids_ctl_diff

# ---------------------------------------------------------------------------- #
# Investigate discrepancies in "task_log" vs. "angular_training" in Psychoeducation ----
# ---------------------------------------------------------------------------- #

# 1. Investigate discrepancies in "comp_train_ids_in_tl_not_at" of "comp_train_ids_ctl_diff"

#   For participants where training completion in "task_log" table is not accompanied 
#   by full training data in "angular_training" table, list whether last expected step
#   title is present in "angular_training" table at Sessions 1-5. Note: Participants 
#   without any data in "angular_training" will not be listed.

comp_train_ids_ctl_in_tl_not_at <- 
  paste(comp_train_ids_ctl_diff$comp_train_ids_in_tl_not_at, collapse = ", ")
comp_train_ids_ctl_in_tl_not_at <- 
  sort(unique(as.integer(unlist(strsplit(comp_train_ids_ctl_in_tl_not_at, split = ", ")))))

ctl_diff_report <- output_ctl[output_ctl$participant_id %in% comp_train_ids_ctl_in_tl_not_at, ]
ctl_diff_report <- ctl_diff_report[order(ctl_diff_report$participant_id), ]

#   For the following participants, "task_log" says they completed training at the 
#   given session, but "angular_training" lacks full training data

#     firstSession
#       No data at all in "angular_training" = 281
#       No data at all in "angular_training" = 782
#       Completed CBM-I = 382 (as discovered in centralized data cleaning)
#       Only has Intro in "angular_training" = 1229
#       Missing parts in "angular_training" = 528
#       Missing parts in "angular_training" = 1521
#     secondSession
#       Only has Intro in "angular_training" = 159
#       Only has Intro in "angular_training" = 269
#     thirdSession
#       Only has Intro in "angular_training" = 159
#       Only has Intro in "angular_training" = 294
#       Only has Intro in "angular_training" = 382
#       Only has Intro in "angular_training" = 1204
#     fourthSession
#       Only has Intro in "angular_training" = 1032
#       Missing parts in "angular_training" = 805
#     fifthSession
#       Only has Intro in "angular_training" = 1280

#   On 2/3/2022, Jeremy Eberle compared "date_completed" timestamps in "task_log" for 
#   each of these cases (except those at Sessions 2 and 4, which have no "preAffect"
#   entry in "task_log" as it was not assessed at these time points) and determined 
#   that enough time elapsed between the "preAffect" entry in "task_log" and the "1", 
#   "3", or "5" entry in "task_log" such that the participant could have completed the 
#   training. As stated above for CBM-I participants, we think the participant's online 
#   session may have timed out, resulting in a loss of some or all of the training data.

#   Thus, we will consider these cases of training completion in the section "Compute
#   indicator of training completion by session" below.

# 2. Investigate discrepancies in "comp_train_ids_in_at_not_tl" of "comp_train_ids_ctl_diff"

#   For participants below, inspection of "angular_training" reveals that they did 
#   not complete all parts for "step_title" of "Anxiety Disorders" at "secondSession".
#   Thus, "task_log" correctly indicates that they did not complete the training.

# View(dat$angular_training[dat$angular_training$participant_id == 857, ])
# View(dat$angular_training[dat$angular_training$participant_id == 961, ])

# ---------------------------------------------------------------------------- #
# Compute indicator of training completion by session ----
# ---------------------------------------------------------------------------- #

# Create "completion" table where "compl_session_train" indicates whether participant
# completed a given session's training (1 = yes, 0 = no, NA = session has no training)

participant_ids <- unique(dat$participant$participant_id)
sessions <- c("Eligibility", "preTest", train_session, "PostFollowUp")

completion <- data.frame(
  participant_id = rep(participant_ids, each = length(sessions)),
  session_only = rep(sessions, length(participant_ids)),
  compl_session_train = NA
)

completion$session_only <- factor(completion$session_only, levels = sessions)

# Compute "compl_session_train". Based on sections "Investigate discrepancies in 
# 'task_log' vs. 'angular_training' in CBM-I conditions [Psychoeducation]" above,
# assume "task_log" entries indicate training completion even if "angular_training" 
# lacks some or all corresponding training data at a given session.

task_log_train <- dat$task_log[dat$task_log$session_only %in% train_session &
                                dat$task_log$task_name %in% 1:5, ]

task_log_train$session_only <- factor(task_log_train$session_only,
                                      levels = train_session)

task_log_train <- task_log_train[order(task_log_train$participant_id,
                                       task_log_train$session_only), ]

task_log_train <- task_log_train[, c("participant_id", "session_only", "task_name")]

#   If multiple unexpected rows are present, remove duplicated rows. However, no
#   unexpected rows are present for training entries in "task_log".

sum(task_log_train[duplicated(task_log_train), ]) == 0

#   Check whether training entries in "task_name" correspond to "session_only"

for (i in 1:length(train_session)) {
  if(sum(task_log_train[task_log_train$session_only == train_session[i] &
                          task_log_train$task_name != i, ]) == 0) {
    print(paste0("TRUE for ", train_session[i]))
  } else {
    print(paste0("FALSE for ", train_session[i]))
  }
}

#   Check for consecutive training entries in "task_name" across "session_only"
#   values within a given participant (to check for skipped entries)

task_log_train$task_name <- as.integer(task_log_train$task_name)

task_log_train <- task_log_train %>%
  group_by(participant_id) %>%
  mutate(task_name_diff = task_name - lag(task_name))

table(task_log_train$task_name_diff, task_log_train$session_only, useNA = "always")

#   Investigate cases of nonconsecutive training entries in "task_name". As found
#   in section "Investigate discrepancies in 'task_log' vs. 'angular_training' in 
#   CBM-I conditions" above, participant 832 is missing a "2" for "task_name" at
#   "secondSession" even though they have full training data at that session in
#   "angular_training". We will correct this in "compl_session_train" below.

nonconsec_task_name_ids <- 
  unique(task_log_train$participant_id[!is.na(task_log_train$task_name_diff) &
                                         task_log_train$task_name_diff != 1])

nonconsec_task_name_ids == 832

# View(task_log_train[task_log_train$participant_id %in% nonconsec_task_name_ids, ])

#   Use "task_name" entries to compute "compl_session_train"

completion <- merge(completion, task_log_train, 
                    by = c("participant_id", "session_only"),
                    all.x = TRUE)

completion$compl_session_train[completion$session_only %in% train_session] <- 0
completion$compl_session_train[!is.na(completion$task_name)] <- 1

# Clean "compl_session_train" based on section "Investigate discrepancies in 'task_log' 
# vs. 'angular_training' in CBM-I conditions" above

completion$compl_session_train[completion$participant_id %in% c(1189, 1161) &
                                 completion$session_only == "firstSession"] <- 1
completion$compl_session_train[completion$participant_id %in% c(1872, 832) &
                                 completion$session_only == "secondSession"] <- 1

# Remove unneeded columns

completion$task_name <- NULL
completion$task_name_diff <- NULL

# ---------------------------------------------------------------------------- #
# Compute indicator of task completion by session ----
# ---------------------------------------------------------------------------- #

# Note: Given that indicator of task completion is not needed for Calm Thinking 
# main outcomes paper, it is not checked for cleaning needs here. By contrast,
# the indicators of training and assessment completion are checked and cleaned.

# Compute "compl_session_all_task" to indicate whether participant completed a 
# given session's tasks (1 = yes, 0 = no). Below, this is done based on whether
# the "task_name" for the participant's latest "date_completed_as_POSIXct" time
# stamp in "task_log" at a given session matches the last expected "task_name"
# for that session. Thus, this assumes that if the participant's final task is
# the last expected task for the session, then they did all the session's tasks.

compute_compl_session_all_task <- function(dat) {
  last_exp_task_name_by_session <- hash()
  last_exp_task_name_by_session[["Eligibility"]] <- "DASS21_AS"
  last_exp_task_name_by_session[["preTest"]] <- "TechnologyUse"
  last_exp_task_name_by_session[["firstSession"]] <- "ReturnIntention"
  last_exp_task_name_by_session[["secondSession"]] <- "ReturnIntention"
  last_exp_task_name_by_session[["thirdSession"]] <- "ReturnIntention"
  last_exp_task_name_by_session[["fourthSession"]] <- "ReturnIntention"
  last_exp_task_name_by_session[["fifthSession"]] <- "AssessingProgram"
  last_exp_task_name_by_session[["PostFollowUp"]] <- "HelpSeeking"
  
  latest_task_compl_df <- tibble()
  
  for (session in keys(last_exp_task_name_by_session)) {
    tmp <- group_by(filter(dat$task_log, session_only == session), 
                    participant_id, session_only) %>%
      summarise(latest_date_completed = max(date_completed_as_POSIXct), 
                latest_task_name = task_name[which.max(date_completed_as_POSIXct)], 
                .groups = "drop")
    
    tmp$compl_session_all_task <- NA
    tmp$compl_session_all_task[tmp$latest_task_name == 
                                 last_exp_task_name_by_session[[session]]] <- 1
    tmp$compl_session_all_task[tmp$latest_task_name != 
                                 last_exp_task_name_by_session[[session]]] <- 0
    
    latest_task_compl_df <- bind_rows(latest_task_compl_df, tmp)
  }
  
  return(latest_task_compl_df)
}

latest_task_compl_df <- compute_compl_session_all_task(dat)

# ---------------------------------------------------------------------------- #
# Compute indicator of assessment completion by session ----
# ---------------------------------------------------------------------------- #

# Compute "compl_session_assess" to indicate whether participant completed a 
# given session's assessment (1 = yes, 0 = no). Below, this is done based on
# whether the last expected "task_name" for a given session's assessment is 
# present in "task_log" at that session. Thus, this assumes that if this task
# is present, then the participant did all the session's assessment tasks.

compute_compl_session_assess <- function(dat) {
  last_exp_assess_task_name_by_session <- hash()
  last_exp_assess_task_name_by_session[["Eligibility"]] <- "DASS21_AS"
  last_exp_assess_task_name_by_session[["preTest"]] <- "TechnologyUse"
  last_exp_assess_task_name_by_session[["firstSession"]] <- "CoachPrompt"
  last_exp_assess_task_name_by_session[["secondSession"]] <- "OA"
  last_exp_assess_task_name_by_session[["thirdSession"]] <- "Mechanisms"
  last_exp_assess_task_name_by_session[["fourthSession"]] <- "OA"
  last_exp_assess_task_name_by_session[["fifthSession"]] <- "AssessingProgram"
  last_exp_assess_task_name_by_session[["PostFollowUp"]] <- "HelpSeeking"
  
  compl_session_assess_df <- tibble()
  
  for (session in keys(last_exp_assess_task_name_by_session)) {
    tmp <- group_by(filter(dat$task_log, session_only == session), 
                    participant_id) %>% 
      mutate(compl_session_assess = 
               ifelse(task_name == last_exp_assess_task_name_by_session[[session]], 1, 0))
    
    tmp2 <- tmp[c("participant_id", "session_only", "task_name", "compl_session_assess")]
    
    tmp3 <- filter(tmp2, compl_session_assess == 1)
    names(tmp3)[names(tmp3) == "task_name"] <- "last_exp_assess_task_name"

    compl_session_assess_df <- bind_rows(compl_session_assess_df, tmp3)
  }
  
  return(compl_session_assess_df)
}

compl_session_assess_df <- compute_compl_session_assess(dat)

  # Remove duplicated rows

compl_session_assess_df <- compl_session_assess_df[!duplicated(compl_session_assess_df), ]

# ---------------------------------------------------------------------------- #
# Compare task and assessment completion by session ----
# ---------------------------------------------------------------------------- #

# Note: Given that this comparison is not needed for Calm Thinking main outcomes 
# paper, it is based on the uncleaned task completion indicator and the as yet
# uncleaned assessment completion indicator (which is cleaned in section below)

# The following participants did not finish the last expected task of a given
# session but finished the last expected assessment task of that session

latest_task_compl_df <- merge(latest_task_compl_df, compl_session_assess_df,
                              by = c("participant_id", "session_only"), all.x = TRUE)

compl_session_assess_not_all_task <- 
  filter(latest_task_compl_df, compl_session_all_task == 0 & compl_session_assess == 1)

all(sort(unique(compl_session_assess_not_all_task$participant_id)) == 
      c(197, 252, 510, 676, 881, 1053, 1725))

# ---------------------------------------------------------------------------- #
# Clean assessment completion by session ----
# ---------------------------------------------------------------------------- #

# Check for discrepancies between a participant having a "compl_session_assess"
# value of 1 at a given session and appearing in the corresponding last expected
# assessment table at that session.

check_last_exp_assess_task_name_vs_tbl <- function(dat, compl_session_assess_df) {
  last_exp_assess_tbl_by_session <- hash()
  last_exp_assess_tbl_by_session[["Eligibility"]] <- "dass21_as"
  last_exp_assess_tbl_by_session[["preTest"]] <- "technology_use"
  last_exp_assess_tbl_by_session[["firstSession"]] <- "coach_prompt"
  last_exp_assess_tbl_by_session[["secondSession"]] <- "oa"
  last_exp_assess_tbl_by_session[["thirdSession"]] <- "mechanisms"
  last_exp_assess_tbl_by_session[["fourthSession"]] <- "oa"
  last_exp_assess_tbl_by_session[["fifthSession"]] <- "assessing_program"
  last_exp_assess_tbl_by_session[["PostFollowUp"]] <- "help_seeking"
  
  for (session in keys(last_exp_assess_tbl_by_session)) {
    tmp <- filter(compl_session_assess_df, 
                  compl_session_assess == 1, session_only == session)
    
    assess_tbl_name <- last_exp_assess_tbl_by_session[[session]]
    assess_tbl <- dat[[assess_tbl_name]]
    tmp2 <- filter(assess_tbl, session_only == session)
    
    # Ignore "participant_id" of NA in "dass21_as" table at "Eligibility" as
    # these reflect screening attempts of non-enrolled participants
    if (assess_tbl_name == "dass21_as") {
      tmp2 <- filter(tmp2, !is.na(participant_id))
    }
    
    ids_in_tmp_not_tmp2 <- setdiff(tmp$participant_id, tmp2$participant_id)
    ids_in_tmp2_not_tmp <- setdiff(tmp2$participant_id, tmp$participant_id)
    
    if (length(ids_in_tmp_not_tmp2) == 0 & length(ids_in_tmp2_not_tmp) == 0) {
      print(paste0("At session '", session, "', ",
                   "no discrepancy between 'compl_session_assess' and table '",
                   assess_tbl_name, "'"))
    } else if (length(ids_in_tmp_not_tmp2) != 0) {
        print(paste0("At session '", session, "', ",
                     "'participant_id' ", ids_in_tmp_not_tmp2,
                     " in 'compl_session_assess' but not in table '", assess_tbl_name, "'"))
    } else if (length(ids_in_tmp2_not_tmp) != 0) {
        print(paste0("At session '", session, "', ",
                     "'participant_id' ", ids_in_tmp2_not_tmp,
                     " in table '", assess_tbl_name, "' but not in 'compl_session_assess'"))
    }
  }
}

# At "Eligibility", "participant_id" 383 is in "dass21_as" table but not in
# "compl_session_assess". We will correct this in "compl_session_assess" below.

check_last_exp_assess_task_name_vs_tbl(dat, compl_session_assess_df)

# Check for consecutive assessment completion across "session_only" values within 
# a given participant (to check for skipped entries)

compl_session_assess_df$session_only <- factor(compl_session_assess_df$session_only,
                                               levels = sessions)

compl_session_assess_df <- compl_session_assess_df[order(compl_session_assess_df$participant_id,
                                                         compl_session_assess_df$session_only), ]

compl_session_assess_df$session_only_int <- as.integer(compl_session_assess_df$session_only)

compl_session_assess_df <- compl_session_assess_df %>%
  group_by(participant_id) %>%
  mutate(session_only_int_diff = session_only_int - lag(session_only_int))

table(compl_session_assess_df$session_only_int_diff,
      compl_session_assess_df$session_only, useNA = "always")

#   Investigate cases of nonconsecutive assessment completion entries

#     As found above, participant 383 lacks "compl_session_assess" at "Eligibility",
#     resulting in NA for "session_only_int_diff" at "preTest". We will correct this below.

non_eligibility_na_session_only_int_ids <-
  unique(compl_session_assess_df$participant_id[is.na(compl_session_assess_df$session_only_int_diff) &
                                                  compl_session_assess_df$session_only != "Eligibility"])

non_eligibility_na_session_only_int_ids == 383

#     Similar to what was found in section "Investigate discrepancies in 'task_log' vs. 
#     'angular_training' in CBM-I conditions" above, participant 832 is missing
#     "compl_session_assess" at "secondSession" (because they are missing "task_name"
#     of "OA" at that session in "task_log"), leading to "session_only_int_diff" of 2
#     at "thirdSession". Participant has no data in "oa" table at "secondSession". Not
#     clear why this occurred, but given that "task_log" shows participant was able to
#     complete the entire study, we will consider this a case of assessment completion
#     at "secondSession". We will correct this in "compl_session_assess" below.

nonconsec_session_only_int_ids <-
  unique(compl_session_assess_df$participant_id[!is.na(compl_session_assess_df$session_only_int_diff) &
                                                  compl_session_assess_df$session_only_int_diff != 1])

nonconsec_session_only_int_ids == 832

# Add "compl_session_assess" to "completion"

completion <- merge(completion, compl_session_assess_df, 
                    by = c("participant_id", "session_only"), all.x = TRUE)

completion$compl_session_assess[is.na(completion$compl_session_assess)] <- 0

# Clean "compl_session_assess" based on result of running above function
# "check_last_exp_assess_task_name_vs_tbl"

completion$compl_session_assess[completion$participant_id == 383 &
                                  completion$session_only == "Eligibility"] <- 1
completion$compl_session_assess[completion$participant_id == 832 &
                                  completion$session_only == "secondSession"] <- 1

# Remove unneeded columns

completion$last_exp_assess_task_name <- NULL
completion$session_only_int <- NULL
completion$session_only_int_diff <- NULL

# ---------------------------------------------------------------------------- #
# Save table ----
# ---------------------------------------------------------------------------- #

# TODO: Consider adding table to centralized data cleaning

write.csv(completion, file = "./data/temp/completion.csv", row.names = FALSE)






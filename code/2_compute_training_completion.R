# ---------------------------------------------------------------------------- #
# Compute Training Completion
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
# Import intermediate clean data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of intermediate clean CSV data files

int_cln_data_dir <- paste0(wd_dir, "/data/intermediate_clean")
filenames <- list.files(int_cln_data_dir, pattern = "*.csv", full.names = FALSE)

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
#   they completed at least 40 scenarios at a given session but "task_log" does not 
#   indicate that they completed training at that session.

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

# TODO








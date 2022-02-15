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

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

groundhog.library(dplyr, groundhog_day)
groundhog.library(hash, groundhog_day)

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
# Compute indicator of assessment completion by session ----
# ---------------------------------------------------------------------------- #

# ------------------------------------ #
# and where "compl_session_all_task" indicates whether participant completed a given
# session's tasks (1 = yes, 0 = no)
session_all_tasks_completion <- function(dat){
  
  session_lastTask_mapping = hash()
  session_lastTask_mapping[["Eligibility"]] = "DASS21_AS"
  session_lastTask_mapping[["preTest"]] = "TechnologyUse"
  session_lastTask_mapping[["firstSession"]] = "ReturnIntention"
  session_lastTask_mapping[["secondSession"]] = "ReturnIntention"
  session_lastTask_mapping[["thirdSession"]] = "ReturnIntention"
  session_lastTask_mapping[["fourthSession"]] = "ReturnIntention"
  session_lastTask_mapping[["fifthSession"]] = "AssessingProgram"
  session_lastTask_mapping[["PostFollowUp"]] = "HelpSeeking"
  
  task_comp_df = tibble()
  
  for (session_map in keys(session_lastTask_mapping)){
    tmp = group_by(filter(dat$task_log, session_only == session_map), participant_id, session_only) %>% 
      summarise(latest_completed_date = max(date_completed_as_POSIXct), 
                latest_task_name = task_name[which.max(date_completed_as_POSIXct)], .groups = "drop")  
    tmp$compl_session_all_task = NA
    tmp$compl_session_all_task[tmp$latest_task_name == session_lastTask_mapping[[session_map]]] = 1
    tmp$compl_session_all_task[tmp$latest_task_name != session_lastTask_mapping[[session_map]]] = 0
    task_comp_df = bind_rows(task_comp_df, tmp)
  }
  
  return(task_comp_df)
  
}
task_comp_df = session_all_tasks_completion(dat)

# ------------------------------------ #
# and where "compl_session_all_assess" indicates whether participant completed a given
# session's assessment (1 = yes, 0 = no)
session_all_assess_completion <- function(data){
  
  session_lastAssess_mapping = hash()
  session_lastAssess_mapping[["Eligibility"]] = "DASS21_AS"
  session_lastAssess_mapping[["preTest"]] = "TechnologyUse"
  session_lastAssess_mapping[["firstSession"]] = "CoachPrompt"
  session_lastAssess_mapping[["secondSession"]] = "OA"
  session_lastAssess_mapping[["thirdSession"]] = "Mechanisms"
  session_lastAssess_mapping[["fourthSession"]] = "OA"
  session_lastAssess_mapping[["fifthSession"]] = "AssessingProgram"
  session_lastAssess_mapping[["PostFollowUp"]] = "HelpSeeking"
  
  
  data$compl_session_all_assess = 0
  for (session_map in keys(session_lastAssess_mapping)){
    tmp = group_by(filter(dat$task_log, 
                          session_only == session_map), 
                   participant_id) %>% mutate(compl_session_all_task = ifelse(task_name %in% session_lastAssess_mapping[[session_map]], 
                                                                              1, 0))
    test = tmp[c('participant_id', 'task_name', 'compl_session_all_task', 'session_only')]
    completed_assess_ids = filter(test, compl_session_all_task == 1)['participant_id']
    
    data[(data$session_only == session_map & 
            data$participant_id %in% completed_assess_ids$participant_id), 'compl_session_all_assess'] = 1
  }
  
  return(data)
}
task_assess_comp_df = session_all_assess_completion(task_comp_df)

# ------------------------------------ #
# these participant did not finish all tasks of a give session but finished all the assessments
filter(task_assess_comp_df, compl_session_all_assess == 1 & compl_session_all_task == 0) #197,252,510,676,881,1725,1053

# ------------------------------------ #
# double check the discrepancy between tasklog and a given assessment
# it seems there is no discrepancy and task log captured all that the data
check_discrepancy_w_task_log <- function(dat, task_assess_comp_df){
  
  session_table_mapping = hash()
  session_table_mapping[["Eligibility"]] = "dass21_as"
  session_table_mapping[["preTest"]] = "technology_use"
  session_table_mapping[["firstSession"]] = "coach_prompt"
  session_table_mapping[["secondSession"]] = "oa"
  session_table_mapping[["thirdSession"]] = "mechanisms"
  session_table_mapping[["fourthSession"]] = "oa"
  session_table_mapping[["fifthSession"]] = "assessing_program"
  session_table_mapping[["PostFollowUp"]] = "help_seeking"
  
  for (session_map in keys(session_table_mapping)){
    tmp1 = filter(task_assess_comp_df, compl_session_all_assess == 1, session_only == session_map)
    tmp = dat[[session_table_mapping[[session_map]]]]
    if (length(setdiff(tmp1$participant_id,tmp$participant_id)) == 0){
      print(paste0("no discrepancy with task_log at table: `", session_table_mapping[[session_map]], "` in the session: ", session_map))
    } 
    else{
      print(paste0("the discrepancy with task_log at table: `", session_table_mapping[[session_map]], "` in the session: ", session_map, " are:"))
      print(setdiff(tmp1$participant_id,tmp$participant_id))
    }
  }
}
check_discrepancy_w_task_log(dat, task_assess_comp_df)


# ------------------------------------ #
task_assess_comp_df$latest_completed_date = NULL
task_assess_comp_df$latest_task_name = NULL
completion_new = merge(completion, task_assess_comp_df, by=c("participant_id","session_only"))
View(completion_new)
# ---------------------------------------------------------------------------- #
# Save table ----
# ---------------------------------------------------------------------------- #

# TODO: Consider adding table to centralized data cleaning

write.csv(completion_new, file = "./data/temp/completion.csv")






# ---------------------------------------------------------------------------- #
# Investigate Training Completion
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
# Confirm that "task_log" indicates training completion in CBM-I conditions ----
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

#   TODO: Investigate discrepancies between "task_log" and "angular_training"

comp_train_ids_cbm_in_tl_not_at <- 
  paste(comp_train_ids_cbm_diff$comp_train_ids_in_tl_not_at, collapse = ", ")
comp_train_ids_cbm_in_tl_not_at <- 
  sort(unique(as.integer(unlist(strsplit(comp_train_ids_cbm_in_tl_not_at, split = ", ")))))

cbm_diff_report <- output_cbm[output_cbm$participant_id %in% comp_train_ids_cbm_in_tl_not_at, ]

#     TODO: See if "comp_train_ids_in_at_not_tl" is an issue

View(dat$angular_training[dat$angular_training$participant_id == 1189, ]) # TRAINING (looks same as 1705)
View(dat$angular_training[dat$angular_training$participant_id == 1161, ]) # TRAINING
View(dat$angular_training[dat$angular_training$participant_id == 1872, ]) # TRAINING AT S1, HR_COACH AT S2

View(dat$task_log[dat$task_log$participant_id == 1189, ])
View(dat$task_log[dat$task_log$participant_id == 1161, ])
View(dat$task_log[dat$task_log$participant_id == 1872, ])

dat$study$conditioning[dat$study$participant_id == 1189] == "TRAINING"
dat$study$conditioning[dat$study$participant_id == 1161] == "TRAINING"
dat$study$conditioning[dat$study$participant_id == 1872] == "HR_COACH"

# Compare above to 1705, who is indicated as completed S1 training in "task_log"
View(dat$angular_training[dat$angular_training$participant_id == 1705, ])
View(dat$task_log[dat$task_log$participant_id == 1705, ])






#     TODO: Why does 602 in "HR_NO_COACH" have no data in "angular_training" despite
#     having "1" in "task_log" at "firstSession"?

# View(dat$angular_training[dat$angular_training$participant_id == 602, ])
# View(dat$task_log[dat$task_log$participant_id == 602, ])
# dat$study$conditioning[dat$study$participant_id == 602]





#     TODO: 

# For the following participants in CBM-I, "task_log" says they completed training 
# at the given session, but "angular_training" lacks full training data

# firstSession
#   No data at all in "angular_training" = 602
#   0  scenarios in "angular_training"   = 406, 465
#   10 scenarios in "angular_training"   = 639
#   39 scenarios in "angular_training"   = 384, 1496
# secondSession  
#   25 scenarios in "angular_training"   = 639
#   30 scenarios in "angular_training"   = 779
#   37 scenarios in "angular_training"   = 768
#   39 scenarios in "angular_training"   = 429, 666, 916, 1756
# thirdSession  
#   5  scenarios in "angular_training"   = 832
#   10 scenarios in "angular_training"   = 779
#   13 scenarios in "angular_training"   = 639
#   16 scenarios in "angular_training"   = 1659
#   39 scenarios in "angular_training"   = 429, 572
# fourthSession  
#   36 scenarios in "angular_training"   = 714
# fifthSession  
#   4  scenarios in "angular_training"   = 832

# Note: 832 lacks data for "secondSession" in "task_log"





#     TODO: How was 832 in "HR_COACHING" given "5" in "task_log" at "fifthSession" 
#     when they didn't complete 40 scenarios (only completed a few)? Also seems that
#     S2 and S3 lacked a full complement of scenarios. Asked Henry/Dan on 1/20/22.
#     S2 also has no data in "task_log".

# View(dat$angular_training[dat$angular_training$participant_id == 832 &
#                             dat$angular_training$session_and_task_info == "fifthSession", ])
# View(dat$angular_training[dat$angular_training$participant_id == 832, ])
# View(dat$task_log[dat$task_log$participant_id == 832, ])
# dat$study$conditioning[dat$study$participant_id == 832]



# ---------------------------------------------------------------------------- #
# Confirm that "task_log" indicates training completion in Psychoeducation ----
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

#   TODO: Investigate discrepancies between "task_log" and "angular_training"

comp_train_ids_ctl_in_tl_not_at <- 
  paste(comp_train_ids_ctl_diff$comp_train_ids_in_tl_not_at, collapse = ", ")
comp_train_ids_ctl_in_tl_not_at <- 
  sort(unique(as.integer(unlist(strsplit(comp_train_ids_ctl_in_tl_not_at, split = ", ")))))

ctl_diff_report <- output_ctl[output_ctl$participant_id %in% comp_train_ids_ctl_in_tl_not_at, ]

#     TODO: See if "comp_train_ids_in_at_not_tl" is an issue

# View(dat$angular_training[dat$angular_training$session_and_task_info == "secondSession" &
#                             dat$angular_training$participant_id == 857, ])
# View(dat$angular_training[dat$angular_training$session_and_task_info == "secondSession" &
#                             dat$angular_training$participant_id == 961, ])





#     TODO: Others in "diff_report"





#     TODO: How was 1280 in "CONTROL" given "5" in "task_log" at "fifthSession" 
#     when they didn't complete "Open to Others" (only completed "Introduction")?
#     Asked Henry/Dan on 1/20/22.

# View(dat$angular_training[dat$angular_training$participant_id == 1280 &
#                             dat$angular_training$session_and_task_info == "fifthSession", ])
# View(dat$angular_training[dat$angular_training$participant_id == 1280, ])
# View(dat$task_log[dat$task_log$participant_id == 1280, ])
dat$study$conditioning[dat$study$participant_id == 1280]
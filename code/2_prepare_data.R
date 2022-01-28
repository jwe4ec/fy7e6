# ---------------------------------------------------------------------------- #
# Prepare Data
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
# Import selected coaching-related data ----
# ---------------------------------------------------------------------------- #

# Import selected columns from "R01_coach_completion_record.csv" given that the
# unselected columns have not yet been cleaned. Selected columns were saved to
# "coach_completion.csv"; thus, this is the file imported.

coach_completion <- read.csv("./data/temp/coach_completion.csv")

# ---------------------------------------------------------------------------- #
# Document data filenames ----
# ---------------------------------------------------------------------------- #

# Output file names to TXT

dir.create("./docs")

sink(file = "./docs/data_filenames.txt")
print(list.files("./data", recursive = TRUE, full.names = FALSE), width = 80)
sink()

# ---------------------------------------------------------------------------- #
# Note on filtering data ----
# ---------------------------------------------------------------------------- #

# Note: As noted in README for centralized data cleaning, end of data collection
# for Calm Thinking study was defined as 12/3/2020, but the last system-generated
# timestamp for a Calm Thinking participant was "2020-11-13 22:13:27 EST". Given
# that the preregistration for the present project states that we will analyze
# data collected through 11/27/2020, no filtering of data is needed.

# ---------------------------------------------------------------------------- #
# Compute participant flow and define analysis samples ----
# ---------------------------------------------------------------------------- #

# Note: Numbers screened (3519), ineligible (for various reasons, 774 + 111 + 23), 
# eligible but not enrolled (2611), and enrolled (1748) were computed as part of
# centralized data cleaning. For ineligible participants with multiple screening
# attempts, their reason for ineligibility is based on their most recent attempt.

# Confirm number enrolled

enrolled_ids <- dat$participant$participant_id[!is.na(dat$participant$participant_id)]
length(enrolled_ids) == 1748

# Compute number Stage 1 randomized

stg1_ids <- dat$study$participant_id[dat$study$conditioning != "NONE"]
length(stg1_ids) == 1614

# Compute number enrolled but not Stage 1 randomized. These participants did not
# complete "demographic" table, whose data are required for Stage 1 randomization,
# which was stratified by gender and baseline anxiety symptom severity

length(enrolled_ids) - length(stg1_ids) == 134

nrow(dat$demographics[!(dat$demographics$participant_id %in% stg1_ids), ]) == 0

# Compute number Stage 1 randomized to CBM-I and to psychoeducation

cbm_conditions <-  c("TRAINING", "LR_TRAINING", "HR_COACH", "HR_NO_COACH")

sum(dat$study$conditioning %in% cbm_conditions) == 1278
sum(dat$study$conditioning == "CONTROL") == 336

# Compute number in CBM-I and in psychoeducation who started S1 training

start_s1_train_ids <- dat$task_log[dat$task_log$session_only == "firstSession" &
                                   dat$task_log$task_name == "Affect" &
                                   dat$task_log$tag == "pre", "participant_id"]
length(start_s1_train_ids) == 1239

nrow(dat$study[dat$study$conditioning %in% cbm_conditions &
                dat$study$participant_id %in% start_s1_train_ids, ]) == 984
nrow(dat$study[dat$study$conditioning == "CONTROL" &
                 dat$study$participant_id %in% start_s1_train_ids, ]) == 255

# Compute number in CBM-I and psychoeducation who did not start S1 training

no_start_s1_ids <- setdiff(stg1_ids, start_s1_train_ids)
length(no_start_s1_ids) == 375

nrow(dat$study[dat$study$conditioning %in% cbm_conditions &
                 dat$study$participant_id %in% no_start_s1_ids, ]) == 294
nrow(dat$study[dat$study$conditioning == "CONTROL" &
                 dat$study$participant_id %in% no_start_s1_ids, ]) == 81

# TODO: Compute number in CBM-I and psychoeducation who completed S1 assessment.
# For now, define this as completing post-training "affect" questions, as this is
# the last piece of data Sonia stated participants needed to be classified by the
# attrition algorithm (specifically, the algorithm that the Changes/Issues Log
# indicates was revised by Sonia to remove R34 features around 5/7/2019). Note:
# "task_log" and "affect" tables correspond on this. Use "task_log" results.

comp_s1_post_affect_ids_task_log <-
  dat$task_log$participant_id[dat$task_log$session_only == "firstSession" &
                                dat$task_log$task_name == "Affect" &
                                dat$task_log$tag == "post"]
comp_s1_post_affect_ids_affect <- 
  dat$affect$participant_id[dat$affect$session_only == "firstSession" &
                              dat$affect$tag == "post"]

identical(comp_s1_post_affect_ids_task_log, comp_s1_post_affect_ids_affect)

comp_s1_post_affect_ids <- comp_s1_post_affect_ids_task_log
length(comp_s1_post_affect_ids) == 1079

nrow(dat$study[dat$study$conditioning %in% cbm_conditions &
                 dat$study$participant_id %in% comp_s1_post_affect_ids, ]) == 837
nrow(dat$study[dat$study$conditioning == "CONTROL" &
                 dat$study$participant_id %in% comp_s1_post_affect_ids, ]) == 242





#   TODO: Unclear why 910 and 1674 were not classified. They have all the
#         data Sonia said was required for classification. The tasks required
#         need to be clarified as they must be used to define the sample of S1
#         assessment completers in psychoeducation. Asked Sonia for explanation
#         and date on which attrition algorithm was actually changed.

table(dat$study$conditioning[dat$study$participant_id %in% comp_s1_post_affect_ids])
dat$study$participant_id[dat$study$participant_id %in% comp_s1_post_affect_ids &
                           dat$study$conditioning == "TRAINING"]

comp_s1_oa_ids <-
  dat$task_log$participant_id[dat$task_log$session_only == "firstSession" &
                                dat$task_log$task_name == "OA"]

table(dat$study$conditioning[dat$study$participant_id %in% comp_s1_oa_ids])
dat$study$participant_id[dat$study$participant_id %in% comp_s1_oa_ids &
                           dat$study$conditioning == "TRAINING"]

comp_s1_return_int_ids <-
  dat$task_log$participant_id[dat$task_log$session_only == "firstSession" &
                                dat$task_log$task_name == "ReturnIntention"]

table(dat$study$conditioning[dat$study$participant_id %in% comp_s1_return_int_ids])
dat$study$participant_id[dat$study$participant_id %in% comp_s1_return_int_ids &
                           dat$study$conditioning == "TRAINING"]

comp_s1_complete_ids <-
  dat$task_log$participant_id[dat$task_log$session_only == "firstSession" &
                                dat$task_log$task_name == "SESSION_COMPLETE"]

table(dat$study$conditioning[dat$study$participant_id %in% comp_s1_complete_ids])
dat$study$participant_id[dat$study$participant_id %in% comp_s1_complete_ids &
                           dat$study$conditioning == "TRAINING"]

# View(dat$task_log[dat$task_log$participant_id %in% c(910, 1674), ])
# View(dat$participant[dat$participant$participant_id %in% c(910, 1674), ])
# View(dat$study[dat$study$participant_id %in% c(910, 1674), ])
# 
# View(dat$credibility[dat$credibility$participant_id %in% c(910, 1674), ])
# View(dat$demographics[dat$demographics$participant_id %in% c(910, 1674), ])
# View(dat$mental_health_history[dat$mental_health_history$participant_id %in% c(910, 1674), ])
# View(dat$affect[dat$affect$participant_id %in% c(910, 1674), ])
# View(dat$angular_training[dat$angular_training$participant_id == 910, ])
# View(dat$angular_training[dat$angular_training$participant_id == 1674, ])
# View(dat$js_psych_trial[dat$js_psych_trial$participant_id == 910, ])
# View(dat$js_psych_trial[dat$js_psych_trial$participant_id == 1674, ])

table(dat$angular_training$conditioning[dat$angular_training$participant_id == 910])
table(dat$angular_training$conditioning[dat$angular_training$participant_id == 1674])





# Compute number in CBM-I classified as lower risk of dropout, and number classified
# as higher risk for dropout and Stage 2 randomized to no coaching or coaching

nrow(dat$study[dat$study$conditioning == "LR_TRAINING" &
                 dat$study$participant_id %in% comp_s1_post_affect_ids, ]) == 288

nrow(dat$study[dat$study$conditioning %in% c("HR_NO_COACH", "HR_COACH") &
                 dat$study$participant_id %in% comp_s1_post_affect_ids, ]) == 547
nrow(dat$study[dat$study$conditioning == "HR_NO_COACH" &
                 dat$study$participant_id %in% comp_s1_post_affect_ids, ]) == 265
nrow(dat$study[dat$study$conditioning == "HR_COACH" &
                 dat$study$participant_id %in% comp_s1_post_affect_ids, ]) == 282

# Compute number in "HR_COACH" who were and were not outreached (attempted
# to be contacted) by a coach, regardless of whether the participant responded.
# Allie Silverman stated on 1/18/2022 that coaches attempted to contact every
# participant in "HR_COACH". This can also be seen by no "coaching_completion"
# values in "coach_completion" table being NA.

HR_COACH_ids <- 
  dat$study$participant_id[dat$study$conditioning == "HR_COACH" &
                             dat$study$participant_id %in% comp_s1_post_affect_ids]

identical(sort(HR_COACH_ids), sort(coach_completion$participant_id))

sum(!is.na(coach_completion$coaching_completion)) == 282
sum(is.na(coach_completion$coaching_completion)) == 0

# Compute number in "HR_COACH" who engaged and did not engage with coach at 
# least once (i.e., completed at least one coaching phone call or text message
# session). Allie Silverman stated on 1/18/2022 that this is best reflected by
# "coaching_completion" values of 1 or 4 in "coach_completion" table.

coach_engage_ids <- coach_completion[coach_completion$coaching_completion %in% c(1, 4),
                                     "participant_id"]
length(coach_engage_ids) == 136

coach_not_engage_ids <- setdiff(coach_completion$participant_id, coach_engage_ids)
length(coach_not_engage_ids) == 146

# TODO: Compute number who completed S2-S4 training and assessment by condition.
# Waiting to hear from Sonia about features required for attrition algorithm.





# Confirm that "task_log" entries are the best indicator of completing training
# for CBM conditions

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

comp_train_ids_cbm_diff <- data.frame(session = train_session,
                                      comp_train_ids_in_tl_not_at = NA,
                                      comp_train_ids_in_at_not_tl = NA)

for (i in 1:length(train_session)) {
  comp_train_ids_cbm_diff[comp_train_ids_cbm_diff$session == train_session[i],
                          "comp_train_ids_in_tl_not_at"] <-
    paste(setdiff(dat$study$participant_id[dat$study$participant_id %in% comp_train_ids[[i]] &
                                             dat$study$conditioning %in% cbm_conditions],
                  output_cbm$participant_id[output_cbm$session == train_session[i] &
                                              output_cbm$n_unq_step_index_for_scenarios == 40]),
          collapse = ", ")
  comp_train_ids_cbm_diff[comp_train_ids_cbm_diff$session == train_session[i],
                          "comp_train_ids_in_at_not_tl"] <-
    paste(setdiff(output_cbm$participant_id[output_cbm$session == train_session[i] &
                                              output_cbm$n_unq_step_index_for_scenarios == 40], 
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




#     TODO: Why does 602 in "HR_NO_COACH" have no data in "angular_training" despite
#     having "1" in "task_log" at "firstSession"?

# View(dat$angular_training[dat$angular_training$participant_id == 602, ])
# View(dat$task_log[dat$task_log$participant_id == 602, ])
# dat$study$conditioning[dat$study$participant_id == 602]





#     TODO: 

# No data in "angular_training" = 602
# Lacks data for "secondSession" in "task_log" = 832
# 
# firstSession
# 0 = 406, 465
# 10 = 639
# 39 = 384, 1496
# secondSession
# 25 = 639
# 30 = 779
# 37 = 768
# 39 = 429, 666, 916, 1756
# thirdSession
# 5 = 832
# 10 = 779
# 13 = 639
# 16 = 1659
# 39 = 429, 572
# fourthSession
# 36 = 714
# fifthSession
# 4 = 832





#     TODO: How was 832 in "HR_COACHING" given "5" in "task_log" at "fifthSession" 
#     when they didn't complete 40 scenarios (only completed a few)? Also seems that
#     S2 and S3 lacked a full complement of scenarios. Asked Henry/Dan on 1/20/22.
#     S2 also has no data in "task_log".

# View(dat$angular_training[dat$angular_training$participant_id == 832 &
#                             dat$angular_training$session_and_task_info == "fifthSession", ])
# View(dat$angular_training[dat$angular_training$participant_id == 832, ])
# View(dat$task_log[dat$task_log$participant_id == 832, ])
# dat$study$conditioning[dat$study$participant_id == 832]





# Confirm that "task_log" entries are the best indicator of completing training
# for psychoeducation condition

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





# TODO: Compute number who completed S5 training by condition

length(comp_s5_train_ids) == 559

nrow(dat$study[dat$study$conditioning %in% c("LR_TRAINING") &
                 dat$study$participant_id %in% comp_s5_train_ids, ]) == 140
nrow(dat$study[dat$study$conditioning %in% c("HR_NO_COACH") &
                 dat$study$participant_id %in% comp_s5_train_ids, ]) == 134
nrow(dat$study[dat$study$conditioning %in% c("HR_COACH") &
                 dat$study$participant_id %in% comp_s5_train_ids, ]) == 131
nrow(dat$study[dat$study$conditioning %in% c("CONTROL") &
                 dat$study$participant_id %in% comp_s5_train_ids, ]) == 154




# Compute number of S5 training completers in "HR_COACH" who engaged and did 
# not engage with a coach at least once

length(intersect(comp_s5_train_ids, coach_engage_ids)) == 96
length(intersect(comp_s5_train_ids, coach_not_engage_ids)) == 35

# TODO: Compute number who completed S5 and 2-month FU assessments by condition.
# Waiting to hear from Sonia about features required for attrition algorithm.





# Identify analysis exclusions due to having more than two unique sets of
# DASS-21-AS screening responses by condition (for more information, see
# https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#participant-flow-and-analysis-exclusions)

exclude_repeat_screen_ids <- 
  dat$participant$participant_id[dat$participant$exclude_analysis == 1]
length(exclude_repeat_screen_ids) == 6

nrow(dat$study[dat$study$participant_id %in% exclude_repeat_screen_ids &
                 dat$study$conditioning == "TRAINING", ]) == 2    # 1644, 1893

nrow(dat$study[dat$study$participant_id %in% exclude_repeat_screen_ids &
                 dat$study$conditioning == "LR_TRAINING", ]) == 0
nrow(dat$study[dat$study$participant_id %in% exclude_repeat_screen_ids &
                 dat$study$conditioning == "HR_NO_COACH", ]) == 2 # 608, 1529
nrow(dat$study[dat$study$participant_id %in% exclude_repeat_screen_ids &
                 dat$study$conditioning == "HR_COACH", ]) == 1    # 1755
nrow(dat$study[dat$study$participant_id %in% exclude_repeat_screen_ids &
                 dat$study$conditioning == "CONTROL", ]) == 1     # 1453

#   Note: Participants 1644, 1893, and 1453 did not complete Session 1 post-
#   affect questions so are already not Session 1 assessment completers

setdiff(exclude_repeat_screen_ids, comp_s1_post_affect_ids)

#   Note: Participant 1755 did not complete Session 5 training so is already
#   not a Session 5 training completer

setdiff(exclude_repeat_screen_ids, comp_s5_train_ids)

# Identify analysis exclusions due to switching conditions, by condition 
# (for more information, see 
# https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#condition-switching)

exclude_switch_condition_ids <- 382

nrow(dat$study[dat$study$participant_id %in% exclude_switch_condition_ids &
                 dat$study$conditioning == "CONTROL", ]) == 1     # 382

# TODO: Identify S1 assessment completer sample by condition

exclude_ids <- c(exclude_repeat_screen_ids, exclude_switch_condition_ids)

s1_assess_comp_anal_ids <- setdiff(comp_s1_post_affect_ids, exclude_ids)
length(s1_assess_comp_anal_ids) == 1075

nrow(dat$study[dat$study$participant_id %in% s1_assess_comp_anal_ids &
                 dat$study$conditioning == "TRAINING", ]) == 2  # 910, 1674

nrow(dat$study[dat$study$participant_id %in% s1_assess_comp_anal_ids &
                 dat$study$conditioning == "LR_TRAINING", ]) == 288
nrow(dat$study[dat$study$participant_id %in% s1_assess_comp_anal_ids &
                 dat$study$conditioning == "HR_NO_COACH", ]) == 263
nrow(dat$study[dat$study$participant_id %in% s1_assess_comp_anal_ids &
                 dat$study$conditioning == "HR_COACH", ]) == 281
nrow(dat$study[dat$study$participant_id %in% s1_assess_comp_anal_ids &
                 dat$study$conditioning == "CONTROL", ]) == 241





# TODO: Identify S5 training completer sample by condition. For "HR_COACH", include
# only participants who engaged with coach at least once.

s5_train_comp_anal_ids <- setdiff(comp_s5_train_ids, 
                                  c(coach_not_engage_ids, exclude_ids))
length(s5_train_comp_anal_ids) == 521

nrow(dat$study[dat$study$participant_id %in% s5_train_comp_anal_ids &
                 dat$study$conditioning == "LR_TRAINING", ]) == 140
nrow(dat$study[dat$study$participant_id %in% s5_train_comp_anal_ids &
                 dat$study$conditioning == "HR_NO_COACH", ]) == 132
nrow(dat$study[dat$study$participant_id %in% s5_train_comp_anal_ids &
                 dat$study$conditioning == "HR_COACH", ]) == 96
nrow(dat$study[dat$study$participant_id %in% s5_train_comp_anal_ids &
                 dat$study$conditioning == "CONTROL", ]) == 153





# TODO: Add indicators for S1 assessment completer and S5 training completer samples
# to "participant" table

dat$participant$s1_assess_comp_anal <- 0
dat$participant[dat$participant$participant_id %in% s1_assess_comp_anal_ids,
                "s1_assess_comp_anal"] <- 1

dat$participant$s5_train_comp_anal <- 0
dat$participant[dat$participant$participant_id %in% s5_train_comp_anal_ids,
                "s5_train_comp_anal"] <- 1





# TODO: Also consider the following

# Unexpected Multiple Entries (https://github.com/jwe4ec/MT-Data-CalmThinkingStudy#unexpected-multiple-entries)





# ---------------------------------------------------------------------------- #
# Conduct further cleaning ----
# ---------------------------------------------------------------------------- #

# TODO: Identify relevant tables. Asked Sonia what variable she has used to look
# at actual device usage on 1/25/22.

# Tables for substantive analysis:
#   - "rr" (pos and neg bias), "bbsiq" (pos and neg bias)
#   - "oa" and "dass21_as" (anxiety)
#   - "task_log" or "angular_training" (training session completion)
#   - "demographics" ("income", age computed from "birth_year")
# Tables for baseline characteristics:
#   - "dass21_as", "demographics"
# Tables for potential auxiliary variables:
#   - "demographics", "credibility" (training confidence, change importance), 
#     "task_log" ("device")





# TODO: Collect potential cleaning tasks

# Handle "prefer not to answer"
# Check response ranges






# ---------------------------------------------------------------------------- #
# Compute Participant Flow
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

# Import "completion" table

dat$completion <- read.csv("./data/temp/completion.csv")

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
# Compute participant flow ----
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

cbm_ids <- dat$study$participant_id[dat$study$conditioning %in% cbm_conditions]
length(cbm_ids) == 1278

control_ids <- dat$study$participant_id[dat$study$conditioning == "CONTROL"]
length(control_ids) == 336

# Compute number in CBM-I and in psychoeducation who started S1 training

start_s1_train_ids <- dat$task_log[dat$task_log$session_only == "firstSession" &
                                   dat$task_log$task_name == "Affect" &
                                   dat$task_log$tag == "pre", "participant_id"]
length(start_s1_train_ids) == 1239

length(intersect(start_s1_train_ids, cbm_ids)) == 984
length(intersect(start_s1_train_ids, control_ids)) == 255

# Compute number in CBM-I and psychoeducation who did not start S1 training

no_start_s1_ids <- setdiff(stg1_ids, start_s1_train_ids)
length(no_start_s1_ids) == 375

length(intersect(no_start_s1_ids, cbm_ids)) == 294
length(intersect(no_start_s1_ids, control_ids)) == 81

# Compute number who completed S1 training by condition

compl_s1_train_ids <- 
  dat$completion$participant_id[dat$completion$session_only == "firstSession" &
                                  dat$completion$compl_session_train == 1]
length(compl_s1_train_ids) == 1082

length(intersect(compl_s1_train_ids, cbm_ids)) == 839
length(intersect(compl_s1_train_ids, control_ids)) == 243

# Compute number in CBM-I and psychoeducation who have a row for "affect" with
# "tag" of "post" at "firstSession", as this is the last criterion that Sonia Baee 
# stated on 1/28/22 participants needed to be classified by the revised attrition 
# algorithm (specifically, the algorithm that the Changes/Issues Log indicates was 
# revised by Sonia to remove R34 features on or around 5/21/2019).

#   Note: "task_log" and "affect" tables agree on this. Use "task_log" results.

compl_s1_post_affect_ids_task_log <-
  dat$task_log$participant_id[dat$task_log$session_only == "firstSession" &
                                dat$task_log$task_name == "Affect" &
                                dat$task_log$tag == "post"]
compl_s1_post_affect_ids_affect <- 
  dat$affect$participant_id[dat$affect$session_only == "firstSession" &
                              dat$affect$tag == "post"]

identical(compl_s1_post_affect_ids_task_log, compl_s1_post_affect_ids_affect)

compl_s1_post_affect_ids <- compl_s1_post_affect_ids_task_log
length(compl_s1_post_affect_ids) == 1079

length(intersect(compl_s1_post_affect_ids, cbm_ids)) == 837
length(intersect(compl_s1_post_affect_ids, control_ids)) == 242

#   Note: All "compl_s1_post_affect_ids" also meet the other criteria Sonia stated
#   were needed for classification by the revised algorithm, namely: (a) a row in
#   each of "credibility" table, "mental_health_history" table, "affect" table with 
#   "tag" of "pre", "affect" table with "tag" of "post", and "js_psych_trial" and
#   (b) "education", "income", and "time_on_page" values in "demographics" table

setdiff(compl_s1_post_affect_ids, dat$credibility$participant_id)
setdiff(compl_s1_post_affect_ids, dat$mental_health_history$participant_id)
setdiff(compl_s1_post_affect_ids, dat$affect$participant_id[dat$affect$tag == "pre"])
setdiff(compl_s1_post_affect_ids, dat$affect$participant_id[dat$affect$tag == "post"])
setdiff(compl_s1_post_affect_ids, dat$js_psych_trial$participant_id)

req_demog_cols <- c("education", "income", "time_on_page")
setdiff(compl_s1_post_affect_ids,
        dat$demographics$participant_id[!is.na(dat$demographics[, req_demog_cols]) &
                                          dat$demographics[, req_demog_cols] != ""])

#   Note: For participants 249, 445, 984, and 1049, a row for "js_psych_trial" was 
#   not recorded at "preTest" even though "task_log" suggests they did Recognition
#   Ratings at that time point. Of these, the CBM-I participants (249, 984, 1049) 
#   appear to have been classified after a row in "js_psych_trial" became present at
#   "thirdSession". Participant 445, in psychoeducation, was not classified.

relevant_ids <- c(249, 445, 984, 1049)

relevant_ids == 
  setdiff(compl_s1_post_affect_ids,
          dat$js_psych_trial$participant_id[dat$js_psych_trial$session_only == "preTest"])

nrow(dat$js_psych_trial[dat$js_psych_trial$participant_id %in% relevant_ids &
                          dat$js_psych_trial$session_only == "preTest", ]) == 0
setdiff(relevant_ids,
        dat$task_log$participant_id[dat$task_log$session_only == "preTest" &
                                      dat$task_log$task_name == "recognitionRatings"])

setdiff(relevant_ids,
        dat$js_psych_trial$participant_id[dat$js_psych_trial$session_only == "thirdSession"])

table(dat$angular_training$conditioning[dat$angular_training$participant_id %in%
                                          c(249, 445, 984, 1049)],
      dat$angular_training$session_and_task_info[dat$angular_training$participant_id %in%
                                                   c(249, 445, 984, 1049)])

#   Note: CBM-I participants 910 and 1674 were not classified even though they met 
#   all criteria for classification. Unclear why, but Sonia Baee stated on 1/28/22
#   that it could be that there was an issue exporting the required data from the
#   server to the attrition algorithm for these participants.

table(dat$study$conditioning[dat$study$participant_id %in% compl_s1_post_affect_ids])
dat$study$participant_id[dat$study$participant_id %in% compl_s1_post_affect_ids &
                           dat$study$conditioning == "TRAINING"]

table(dat$angular_training$conditioning[dat$angular_training$participant_id == 910])
table(dat$angular_training$conditioning[dat$angular_training$participant_id == 1674])

# Compute number in CBM-I and psychoeducation who did not complete S1 training
# and classification measures

no_compl_s1_post_affect_ids <- setdiff(stg1_ids, compl_s1_post_affect_ids)
length(no_compl_s1_post_affect_ids) == 535

length(intersect(no_compl_s1_post_affect_ids, cbm_ids)) == 441
length(intersect(no_compl_s1_post_affect_ids, control_ids)) == 94

# Compute number in CBM-I and psychoeducation who did not complete S1 training

no_compl_s1_train_ids <- setdiff(stg1_ids, compl_s1_train_ids)
length(no_compl_s1_train_ids) == 532

length(intersect(no_compl_s1_train_ids, cbm_ids)) == 439
length(intersect(no_compl_s1_train_ids, control_ids)) == 93

# Compute number in CBM-I and psychoeducation who completed S1 training but did
# not complete classification measures

compl_s1_train_but_not_s1_post_affect_ids <- intersect(compl_s1_train_ids, 
                                                       no_compl_s1_post_affect_ids)
length(compl_s1_train_but_not_s1_post_affect_ids) == 3

length(intersect(compl_s1_train_but_not_s1_post_affect_ids, cbm_ids)) == 2
length(intersect(compl_s1_train_but_not_s1_post_affect_ids, control_ids)) == 1

# Compute number in CBM-I classified as lower risk of dropout, and number classified
# as higher risk for dropout and Stage 2 randomized to no coaching or coaching

nrow(dat$study[dat$study$conditioning == "LR_TRAINING" &
                 dat$study$participant_id %in% compl_s1_post_affect_ids, ]) == 288

nrow(dat$study[dat$study$conditioning %in% c("HR_NO_COACH", "HR_COACH") &
                 dat$study$participant_id %in% compl_s1_post_affect_ids, ]) == 547
nrow(dat$study[dat$study$conditioning == "HR_NO_COACH" &
                 dat$study$participant_id %in% compl_s1_post_affect_ids, ]) == 265
nrow(dat$study[dat$study$conditioning == "HR_COACH" &
                 dat$study$participant_id %in% compl_s1_post_affect_ids, ]) == 282

# Compute number in "HR_COACH" who were and were not outreached (attempted
# to be contacted) by a coach, regardless of whether the participant responded.
# Allie Silverman stated on 1/18/2022 that coaches attempted to contact every
# participant in "HR_COACH". This can also be seen by no "coaching_completion"
# values in "coach_completion" table being NA.

HR_COACH_ids <- 
  dat$study$participant_id[dat$study$conditioning == "HR_COACH" &
                             dat$study$participant_id %in% compl_s1_post_affect_ids]

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

# TODO: Compute number who completed S1 assessment by condition





# Compute number who completed S2-S4 training by condition

compl_s2_train_ids <- 
  dat$completion$participant_id[dat$completion$session_only == "secondSession" &
                                  dat$completion$compl_session_train == 1]
compl_s3_train_ids <- 
  dat$completion$participant_id[dat$completion$session_only == "thirdSession" &
                                  dat$completion$compl_session_train == 1]
compl_s4_train_ids <- 
  dat$completion$participant_id[dat$completion$session_only == "fourthSession" &
                                  dat$completion$compl_session_train == 1]

length(compl_s2_train_ids) == 778
length(compl_s3_train_ids) == 703
length(compl_s4_train_ids) == 601

table(dat$study$conditioning[dat$study$participant_id %in% compl_s2_train_ids])
table(dat$study$conditioning[dat$study$participant_id %in% compl_s3_train_ids])
table(dat$study$conditioning[dat$study$participant_id %in% compl_s4_train_ids])

# TODO: Compute number who completed S2-S4 assessment by condition





# Compute number who completed S5 training by condition

compl_s5_train_ids <- 
  dat$completion$participant_id[dat$completion$session_only == "fifthSession" &
                                  dat$completion$compl_session_train == 1]
length(compl_s5_train_ids) == 559

table(dat$study$conditioning[dat$study$participant_id %in% compl_s5_train_ids])

# Compute number of S5 training completers in "HR_COACH" who engaged and did 
# not engage with a coach at least once

length(intersect(compl_s5_train_ids, coach_engage_ids)) == 96
length(intersect(compl_s5_train_ids, coach_not_engage_ids)) == 35

# TODO: Compute number who completed S5 and 2-month FU assessments by condition

# Note: Consider unexpected multiple entries
#   See https://github.com/jwe4ec/MT-Data-CalmThinkingStudy#unexpected-multiple-entries





# ---------------------------------------------------------------------------- #
# Define analysis samples ----
# ---------------------------------------------------------------------------- #

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
#   affect questions so are already not classification measure completers

setdiff(exclude_repeat_screen_ids, compl_s1_post_affect_ids)

#   Note: Participant 1755 did not complete Session 5 training so is already
#   not a Session 5 training completer

setdiff(exclude_repeat_screen_ids, compl_s5_train_ids)

# Identify analysis exclusions due to switching conditions, by condition 
# (for more information, see 
# https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#condition-switching)

exclude_switch_condition_ids <- 382

nrow(dat$study[dat$study$participant_id %in% exclude_switch_condition_ids &
                 dat$study$conditioning == "CONTROL", ]) == 1     # 382

# Identify classification measure completer analysis sample by condition

exclude_ids <- c(exclude_repeat_screen_ids, exclude_switch_condition_ids)

class_meas_compl_anlys_ids <- setdiff(compl_s1_post_affect_ids, exclude_ids)
length(class_meas_compl_anlys_ids) == 1075

nrow(dat$study[dat$study$participant_id %in% class_meas_compl_anlys_ids &
                 dat$study$conditioning == "TRAINING", ]) == 2  # 910, 1674

nrow(dat$study[dat$study$participant_id %in% class_meas_compl_anlys_ids &
                 dat$study$conditioning == "LR_TRAINING", ]) == 288
nrow(dat$study[dat$study$participant_id %in% class_meas_compl_anlys_ids &
                 dat$study$conditioning == "HR_NO_COACH", ]) == 263
nrow(dat$study[dat$study$participant_id %in% class_meas_compl_anlys_ids &
                 dat$study$conditioning == "HR_COACH", ]) == 281
nrow(dat$study[dat$study$participant_id %in% class_meas_compl_anlys_ids &
                 dat$study$conditioning == "CONTROL", ]) == 241

# Identify S5 training completer analysis sample by condition. For "HR_COACH", 
# include only participants who engaged with coach at least once.

s5_train_compl_anlys_ids <- setdiff(compl_s5_train_ids, 
                                    c(coach_not_engage_ids, exclude_ids))
length(s5_train_compl_anlys_ids) == 521

nrow(dat$study[dat$study$participant_id %in% s5_train_compl_anlys_ids &
                 dat$study$conditioning == "LR_TRAINING", ]) == 140
nrow(dat$study[dat$study$participant_id %in% s5_train_compl_anlys_ids &
                 dat$study$conditioning == "HR_NO_COACH", ]) == 132
nrow(dat$study[dat$study$participant_id %in% s5_train_compl_anlys_ids &
                 dat$study$conditioning == "HR_COACH", ]) == 96
nrow(dat$study[dat$study$participant_id %in% s5_train_compl_anlys_ids &
                 dat$study$conditioning == "CONTROL", ]) == 153

# Add indicators for classification measure completer sample and S5 training 
# completer sample to "participant" table

dat$participant$class_meas_compl_anlys <- 0
dat$participant[dat$participant$participant_id %in% class_meas_compl_anlys_ids,
                "class_meas_compl_anlys"] <- 1

dat$participant$s5_train_compl_anlys <- 0
dat$participant[dat$participant$participant_id %in% s5_train_compl_anlys_ids,
                "s5_train_compl_anlys"] <- 1

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/intermediate_clean_further")

save(dat, file = "./data/intermediate_clean_further/dat.RData")
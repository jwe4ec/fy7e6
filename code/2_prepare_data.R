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

# Output file names to TXT

dir.create("./docs")

sink(file = "./docs/data_filenames.txt")

cat("In './data/intermediate_clean'", "\n")
cat("\n")
print(filenames)

sink()

# Import tables into list and name tables

dat <- lapply(paste0(int_cln_data_dir, "/", filenames), read.csv)
names(dat) <- sub(".csv", "", filenames)

# Convert system-generated timestamps to POSIXct data types

dat <- convert_POSIXct(dat)

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

View(dat$task_log[dat$task_log$participant_id %in% c(910, 1674), ])
View(dat$participant[dat$participant$participant_id %in% c(910, 1674), ])
View(dat$study[dat$study$participant_id %in% c(910, 1674), ])

View(dat$credibility[dat$credibility$participant_id %in% c(910, 1674), ])
View(dat$demographics[dat$demographics$participant_id %in% c(910, 1674), ])
View(dat$mental_health_history[dat$mental_health_history$participant_id %in% c(910, 1674), ])
View(dat$affect[dat$affect$participant_id %in% c(910, 1674), ])
View(dat$angular_training[dat$angular_training$participant_id == 910, ])
View(dat$angular_training[dat$angular_training$participant_id == 1674, ])
View(dat$js_psych_trial[dat$js_psych_trial$participant_id == 910, ])
View(dat$js_psych_trial[dat$js_psych_trial$participant_id == 1674, ])

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

# TODO: Compute number in "HR_COACH" who were and were not outreached (attempted
# to be contacted) by a coach, regardless of whether the participant responded.
# Consider "Coach Session Tracking" table variables "Date initial email was sent",
# "Date second email was sent", and "Date of cold call". If at least one of these 
# is not blank, then consider coach to have outreached participant. I think these
# variables are "email1_date", "email2_date", and "coldcall1_date" in the table
# "MT_Coaching_Documentation_6.7.20.xlsx". Or a value 1-5 for "coaching_completion"
# in "R01_coach_completion_record.csv". Asked Alex/Allie to confirm on 1/18/22.

HR_COACH_ids <- 
  dat$study$participant_id[dat$study$conditioning == "HR_COACH" &
                             dat$study$participant_id %in% comp_s1_post_affect_ids]

completion_record_ids <- c(164, 192, 203, 217, 233, 259, 246, 250, 251, 235, 237,
                           248, 282, 191, 309, 321, 329, 345, 285, 354, 363, 301,
                           393, 365, 385, 359, 409, 406, 401, 412, 422, 427, 446, 
                           443, 442, 454, 453, 450, 464, 465, 435, 466, 416, 475,
                           470, 493, 488, 495, 491, 467, 539, 535, 532, 526, 286, 
                           555, 570, 578, 576, 577, 562, 561, 588, 484, 593, 485, 
                           501, 609, 617, 643, 591, 647, 657, 425, 649, 626, 655,
                           632, 664, 661, 658, 683, 665, 696, 739, 728, 701, 757,
                           811, 815, 824, 832, 730, 843, 807, 808, 837, 825, 870,
                           816, 876, 871, 810, 967, 952, 946, 921, 920, 914, 884, 
                           894, 679, 972, 933, 944, 973, 853, 917, 927, 662, 706,
                           740, 766, 856, 735, 745, 778, 847, 720, 911, 687, 714,
                           877, 990, 987, 754, 985, 993, 995, 726, 1000, 792, 996, 
                           741, 747, 831, 1020, 1017, 1062, 1053, 1065, 1080, 1093,
                           1101, 1102, 1076, 1113, 1041, 1097, 879, 1060, 1137, 1081, 
                           1098, 1168, 1145, 1193, 1173, 1222, 1224, 1040, 1236, 1235, 
                           1252, 1259, 1307, 1306, 1130, 1304, 1337, 1344, 1367, 1335, 
                           1398, 1354, 1383, 1410, 1401, 1432, 1424, 1455, 1451, 1443, 
                           1473, 1461, 1416, 1505, 1494, 1526, 1535, 1542, 1412, 1269, 
                           1538, 1555, 1400, 1361, 1571, 1562, 1395, 1575, 1597, 1594, 
                           1596, 1606, 1626, 1618, 1631, 1630, 1310, 1583, 1643, 1641,
                           1582, 1633, 1608, 1652, 1651, 1536, 1668, 1666, 1456, 1659,
                           1654, 1696, 1694, 1708, 1738, 1736, 1598, 1725, 1748, 1769,
                           1761, 1763, 1755, 1797, 1790, 1803, 1628, 1809, 1812, 1827,
                           1825, 1820, 1829, 1828, 1839, 1841, 1860, 1877, 1816, 1879, 
                           1872, 1813, 1862, 1896, 1832, 1904, 1907, 1937, 1936, 1964, 
                           1962, 1974, 1966, 1986, 1980, 1993, 1999, 1944, 1954)
completion_record_ids <- as.integer(completion_record_ids)
identical(sort(HR_COACH_ids), sort(completion_record_ids))

session_data_ids <- c(192, 203, 217, 233, 237, 246, 248, 250, 251, 259, 285, 309,
                      321, 329, 345, 354, 359, 363, 365, 385, 393, 406, 409, 412, 
                      422, 442, 443, 446, 450, 453, 454, 465, 466, 467, 470, 484, 
                      485, 488, 491, 493, 526, 532, 539, 555, 561, 562, 570, 576,
                      577, 578, 593, 609, 617, 626, 632, 643, 647, 649, 655, 661,
                      662, 664, 665, 683, 687, 696, 701, 726, 728, 730, 735, 739, 
                      741, 754, 757, 810, 811, 816, 824, 832, 837, 853, 871, 876,
                      877, 884, 894, 911, 917, 920, 921, 933, 944, 972, 973, 987, 
                      996, 1000, 1017, 1020, 1053, 1062, 1065, 1076, 1080, 1098,
                      1112, 1137, 1145, 1168, 1173, 1193, 1222, 1224, 1269, 1304, 
                      1337, 1344, 1354, 1367, 1395, 1398, 1401, 1410, 1412, 1416, 
                      1424, 1432, 1443, 1451, 1456, 1461, 1473, 1505, 1535, 1536,
                      1538, 1562, 1571, 1575, 1582, 1583, 1598, 1628, 1633, 1643, 
                      1651, 1652, 1654, 1666, 1668, 1694, 1708, 1725, 1736, 1738, 
                      1761, 1763, 1769, 1809, 1812, 1816, 1825, 1827, 1828, 1832, 
                      1839, 1860, 1862, 1879, 1896, 1907, 1936, 1937, 1944, 1954,
                      1962, 1964, 1966, 1980, 1993, 1999)
session_data_ids <- as.integer(session_data_ids)
length(session_data_ids)

#   TODO: Asked Alex/Allie on 1/18/22 to confirm that this is a coaching account
setdiff(session_data_ids, HR_COACH_ids) == 1112
setdiff(session_data_ids, completion_record_ids) == 1112





# TODO: Compute number in "HR_COACH" who engaged and did not engage with coach at 
# least once (i.e., completed at least one coaching phone call or text messaging 
# session). Consider "Coach Session Tracking" table variables "Is initial call 
# complete?" and "Is call/texting complete?" [for first follow-up coaching call,
# second follow-up coaching call, and final coaching call]. If at least one of
# these is "2 - complete", then consider participant to have engaged with coach.
# I think these variables are "s1complete", "s2complete", "s3complete", and
# "s4complete" in the table "MT_Coaching_Documentation_6.7.20.xlsx". Alternatively, 
# is it better to see which have at least one entry in "R01_coach_sessiondata.csv" 
# table where "coaching_successful" is "Yes". Or "coaching_completion" value of 1
# or 4 in "R01_coach_completion_record.csv". Asked Alex/Allie to confirm on 1/18/22.





# TODO: Compute number who completed S2-S4 training and assessment by condition





# TODO: Compute number who completed S5 training by condition





# TODO: Compute number of S5 training completers in "HR_COACH" who engaged and
# did not engage with a coach at least once





# TODO: Compute number who completed S5 and 2-month FU assessments by condition





# TODO: Identify analysis exclusions by condition





# TODO: Identify S1 assessment completer and S5 training completer samples





# TODO: Also consider the following

# Dropout Risk (https://github.com/jwe4ec/MT-Data-CalmThinkingStudy#dropout-risk)
# Condition Switching (https://github.com/jwe4ec/MT-Data-CalmThinkingStudy#condition-switching)
# Participant Flow and Analysis Exclusions (https://github.com/jwe4ec/MT-Data-CalmThinkingStudy#participant-flow-and-analysis-exclusions)
# Unexpected Mulitple Entries (https://github.com/jwe4ec/MT-Data-CalmThinkingStudy#unexpected-multiple-entries)
# Consider coaching-related data (https://github.com/jwe4ec/MT-Data-CalmThinkingStudy#coaching-related-data-on-uva-box)





# ---------------------------------------------------------------------------- #
# Conduct further cleaning ----
# ---------------------------------------------------------------------------- #

# TODO: Collect potential cleaning tasks

# Handle "prefer not to answer"
# Check response ranges






# ---------------------------------------------------------------------------- #
# Further Clean Readiness Rulers Check Data
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
# Extract and clean Readiness Rulers Check data ----
# ---------------------------------------------------------------------------- #

# Extract data

readiness <- dat2$angular_training[dat2$angular_training$stimulus_name == "readiness_rulers", ]

# Confirm all rows are at "firstSession"

all(readiness$session_and_task_info == "firstSession")

# Inspect condition (though analyses will not use condition from this table)

table(readiness$conditioning, useNA = "always")

#   Participants 185 and 402 have blank condition. Per centralized data cleaning
#   script "4_clean_data.R" (see Line 1473), "Dan Funk said on 1/4/2021 that pressing 
#   the "Continue" button (i.e., "button_pressed" == "continue", which has a high 
#   prevalence in these cases) does not always contain a condition. He said that he 
#   believes these participants had a session timeout of some kind and likely received 
#   a red-error bar saying "you are not logged in" and prompting them to go back to 
#   the main site; however, they can ignore the prompt and continue anyway." No issue.

readiness[readiness$conditioning == "", "participant_id"] == c(185, 402)

#   Participants 382, 396, 390, and 403 have "NONE" condition

#     Per centralized data cleaning script "4_clean_data.R" (see Line 1586), 
#     participant 382 did "CBM-I" at "firstSession" and then switched to "CONTROL" 
#     and did psychoeducation from "secondSession" through "fifthSession"; this 
#     participant will be  excluded from the ITT sample.

#     Per centralized data cleaning script "4_clean_data.R" (see Lines 1677-1689), 
#     participants 396, 390, and 403 did CBM-I at "firstSession" and then were 
#     in "LR_TRAINING" starting at "secondSession." No issue.

readiness[readiness$conditioning == "NONE", "participant_id"] == c(382, 396, 390, 403)

# Data exist for only 665 participants

length(readiness$participant_id)         == 665
length(unique(readiness$participant_id)) == 665

#   Inspect date range. Henry Behan said via email on 3/21/2022 that although for 
#   the TET study, the "stimulus_name" values for Readiness Rulers Check data were 
#   updated to "readiness_question" and "readiness_rulers_control", for the Calm
#   Thinking study the value was "readiness_rulers" throughout. He confirmed that
#   data seem to be missing for "TRAINING" participants after 8/2019 (specifically
#   8/26/2019; except for Participant 1373, who has data on 11/12/2020). He suspects 
#   that the addition of a videos page "in between the readiness check" in the CBM-I 
#   condition somehow interfered with collection of these data (he stated that the 
#   videos page was not included in the psychoeducation condition).

range(readiness[readiness$conditioning == 
                  "TRAINING", ]$date_as_POSIXct) # "2019-03-20 20:51:18 EST" "2020-11-12 15:20:51 EST"
range(readiness[readiness$conditioning == 
                  "CONTROL",  ]$date_as_POSIXct) # "2019-03-20 19:52:55 EST" "2020-03-19 16:48:23 EST"
range(readiness[readiness$conditioning == 
                  "",         ]$date_as_POSIXct) # "2019-04-02 12:52:17 EST" "2019-05-29 13:51:44 EST"
range(readiness[readiness$conditioning == 
                  "NONE",     ]$date_as_POSIXct) # "2019-05-13 13:07:22 EST" "2019-05-21 18:11:15 EST"

hist(readiness[readiness$conditioning == "TRAINING", ]$date_as_POSIXct, breaks = "months")
hist(readiness[readiness$conditioning == "CONTROL",  ]$date_as_POSIXct, breaks = "months")

# View(readiness[readiness$conditioning == "TRAINING" & 
#                  readiness$date_as_POSIXct >= "2019-08-26 00:00:00 EST", ])
                                                 # Participant 1009 on 2019-08-26 08:41:32 EST
                                                 # Participant 1373 on 2020-11-12 15:20:51 EST

nrow(readiness[readiness$conditioning == "TRAINING" & 
                 readiness$date_as_POSIXct > "2019-08-27 00:00:00 EST" &
                 readiness$date_as_POSIXct < "2020-11-12 00:00:00 EST", ]) == 0

# Recode response options

readiness$button_pressed[readiness$button_pressed == "Not at all"]           <- 0
readiness$button_pressed[readiness$button_pressed == "Slightly"]             <- 1
readiness$button_pressed[readiness$button_pressed == "Somewhat"]             <- 2
readiness$button_pressed[readiness$button_pressed == "Mostly"]               <- 3
readiness$button_pressed[readiness$button_pressed == "Very"]                 <- 4
readiness$button_pressed[readiness$button_pressed == "Prefer not to answer"] <- NA

readiness$button_pressed <- as.numeric(readiness$button_pressed)

# Restrict to relevant columns

exclude_cols <- c("time_on_page", "correct", "device", "rt", "rt_first_react",
                  "session_counter", "session_index", "session_title", 
                  "step_index", "step_title", "stimulus", "time_elapsed", "trial_type")

readiness <- readiness[, !(names(readiness) %in% exclude_cols)]

# Rename Readiness Rulers Check item

names(readiness)[names(readiness) == "button_pressed"] <- "confident_program"

# ---------------------------------------------------------------------------- #
# Save cleaned data ----
# ---------------------------------------------------------------------------- #

save(readiness, file = "./data/intermediate_clean_further/readiness.RData")
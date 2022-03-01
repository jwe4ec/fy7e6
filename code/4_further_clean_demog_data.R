# ---------------------------------------------------------------------------- #
# Further Clean Demographic Data and Create Table
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

load("./data/intermediate_clean_further/dat.RData")

# ---------------------------------------------------------------------------- #
# Recode "Prefer not to answer" in categorical and ordinal variables ----
# ---------------------------------------------------------------------------- #

pna <- "Prefer not to answer"

# Recode 555 as "Prefer not to answer". 555 in "birth_year" (integer variable)
# is recoded after "age" is computed below.

target_vars <- c("education", "employment_stat", "ethnicity", "gender", "income",
                 "marital_stat")
dat$demographics[, target_vars][dat$demographics[, target_vars] == 555] <- pna

dat$demographics_race$race[dat$demographics_race$race == 555] <- pna

# Recode "NoAnswer" as "Prefer not to answer"

dat$demographics$country[dat$demographics$country == "NoAnswer"] <- pna

# ---------------------------------------------------------------------------- #
# Compute age ----
# ---------------------------------------------------------------------------- #

# Compute age (use NA where "birth_year" is 555 ["prefer not to answer"])

dat$demographics$age <- NA

for (i in 1:nrow(dat$demographics)) {
  if (dat$demographics$birth_year[i] != 555) {
    dat$demographics$age[i] <- 
      as.integer(format(dat$demographics$date_as_POSIXct[i], "%Y")) - 
      dat$demographics$birth_year[i]
  }
}

# Participants range from 18 to 75 years of age, which is reasonable

range(dat$demographics$age, na.rm = TRUE)

# All NAs in "age" are due to "birth_year" of 555

all(dat$demographics$birth_year[is.na(dat$demographics$age)] == 555)

# Recode 555 in "birth_year" to "Prefer not to answer"

dat$demographics$birth_year[dat$demographics$birth_year == 555] <- pna

# ---------------------------------------------------------------------------- #
# Clean gender ----
# ---------------------------------------------------------------------------- #

# Reorder levels and add "Transgender Female". Codebook indicates that on 8/5/2019, 
# "Transgender" option was replaced with "Transgender Male" and "Transgender Female".

dat$demographics$gender <- 
  factor(dat$demographics$gender,
         levels = c("Female", "Male", 
                    "Transgender", "Transgender Female", "Transgender Male",
                    "Other", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean ethnicity ----
# ---------------------------------------------------------------------------- #

# Reorder levels

dat$demographics$ethnicity <- 
  factor(dat$demographics$ethnicity,
         levels = c("Hispanic or Latino", "Not Hispanic or Latino", 
                    "Unknown", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean employment status ----
# ---------------------------------------------------------------------------- #

# Recode "Homemaker"

dat$demographics$employment_stat[dat$demographics$employment_stat ==
                                   "Homemaker/keeping house or raising children full-time"] <-
  "Homemaker"

# Reorder levels

dat$demographics$employment_stat <- 
  factor(dat$demographics$employment_stat,
         levels = c("Student", "Homemaker", "Unemployed or laid off", "Looking for work",
                    "Working part-time", "Working full-time", "Retired", "Other",
                    "Unknown", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean income ----
# ---------------------------------------------------------------------------- #

# Recode "Don't know"

dat$demographics$income[dat$demographics$income == "Don't know"] <- "Unknown"

# Reorder levels

dat$demographics$income <-
  factor(dat$demographics$income,
         levels = c("Less than $5,000", "$5,000 through $11,999", 
                    "$12,000 through $15,999", "$16,000 through $24,999", 
                    "$25,000 through $34,999", "$35,000 through $49,999",
                    "$50,000 through $74,999", "$75,000 through $99,999",
                    "$100,000 through $149,999", "$150,000 through $199,999",
                    "$200,000 through $249,999", "$250,000 or greater",
                    "Unknown", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean marital status ----
# ---------------------------------------------------------------------------- #

# Reorder levels

dat$demographics$marital_stat <-
  factor(dat$demographics$marital_stat,
         levels = c("Single", "Single,dating", "Single,engaged", "Single,marriagelike",
                    "Married", "civilunion", "Separated", "Divorced", "Widow/widower",
                    "Other", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean race ----
# ---------------------------------------------------------------------------- #

# No duplicated responses exist

nrow(dat$demographics_race[duplicated(dat$demographics_race[, c("participant_id", 
                                                                "race")]), ]) == 0

# Compute number of responses per "participant_id"

race_ag <- aggregate(race ~ participant_id,
                     dat$demographics_race,
                     length)
names(race_ag)[names(race_ag) == "race"] <- "race_cnt"

dat$demographics_race <- merge(dat$demographics_race, race_ag,
                               by = "participant_id", all.x = TRUE)

# Define "race_col", collapsing cases of more than once race

dat$demographics_race$race_col <- NA

for (i in 1:nrow(dat$demographics_race)) {
  if (dat$demographics_race$race_cnt[i] == 1) {
    dat$demographics_race$race_col[i] <- dat$demographics_race$race[i]
  } else if (dat$demographics_race$race_cnt[i] > 1) {
    dat$demographics_race$race_col[i] <- "More than one race"
  }
}

# Add "race_col" to "demographics" table

dat$demographics <- merge(dat$demographics,
                          dat$demographics_race[, c("participant_id", "race_col")],
                          by = "participant_id",
                          all.x = TRUE)

# Reorder levels

dat$demographics$race_col <-
  factor(dat$demographics$race_col,
         levels = c("American Indian/Alaska Native", "Black/African origin",
                    "East Asian", "Native Hawaiian/Pacific Islander", "South Asian",
                    "White/European origin", "More than one race", "Other or Unknown", 
                    "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean country ----
# ---------------------------------------------------------------------------- #

# Recode "Ã…land Islands"

dat$demographics$country[dat$demographics$country == "Ã…land Islands"] <- "Åland Islands"

# Define desired levels order (decreasing frequency ending with "prefer not to answer")

country_levels <- unlist(attr(sort(table(dat$demographics$country), decreasing = TRUE), 
                              "dimnames"))
country_levels <- c(country_levels[country_levels != pna], pna)

# Reorder levels of "country"

dat$demographics$country <- factor(dat$demographics$country, levels = country_levels)

# Define "country_col", collapsing countries not in top 5 into "Other"

top_countries <- head(country_levels, 5)
all(top_countries == c("United States", "Australia", "Canada", "United Kingdom", "Germany"))

dat$demographics$country_col <- NA

for (i in 1:nrow(dat$demographics)) {
  if (as.character(dat$demographics$country)[i] %in% top_countries) {
    dat$demographics$country_col[i] <- as.character(dat$demographics$country)[i]
  } else if (as.character(dat$demographics$country)[i] == pna) {
    dat$demographics$country_col[i] <- pna
  } else {
    dat$demographics$country_col[i] <- "Other"
  }
}

# Reorder levels of "country_col"

dat$demographics$country_col <-
  factor(dat$demographics$country_col,
         levels = c(top_countries, "Other", pna))

# ---------------------------------------------------------------------------- #
# Create table ----
# ---------------------------------------------------------------------------- #

dem_tbl <- dat$demographics

# Add condition

dem_tbl <- merge(dem_tbl, dat$study[, c("participant_id", "conditioning")],
                 by = "participant_id", all.x = TRUE)

# TODO: Order condition levels (consider different samples)





# TODO: Restrict analysis samples





# TODO: Compute descriptives overall and in each condition





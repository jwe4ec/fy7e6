# ---------------------------------------------------------------------------- #
# Evaluate Attrition Algorithm
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

groundhog_day <- version_control_gran()

# Load packages

groundhog.library("caret", groundhog_day, tolerate.R.version = "4.1.2")

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Note: "dat3" is cleaned further than "dat2", but not in ways that matter for
# evaluating attrition algorithm, and "dat2" has additional tables

load("./data/intermediate_clean_further/dat2.RData")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Build table with relevant columns based on "completion" table

alg_eval <- dat2$completion

target_cols <- c("participant_id", "risk_classification_method", "exclude_analysis", 
                 "itt_anlys", "s5_train_compl_anlys_uncorrected_c1", 
                 "class_meas_compl_anlys", "s5_train_compl_anlys_c2_4")

alg_eval <- merge(alg_eval, dat2$participant[, target_cols], 
                  by = "participant_id", all.x = TRUE)

alg_eval <- merge(alg_eval, dat2$study[, c("participant_id", "conditioning")],
                  by = "participant_id", all.x = TRUE)

alg_eval <- merge(alg_eval, dat2$attrition_prediction[, c("participant_id", "confidence")],
                  by = "participant_id", all.x = TRUE)

# Restrict to ITT participants

alg_eval_itt <- alg_eval[alg_eval$itt_anlys == 1, ]

# Note: 1 participant in "CONTROL" received an attrition score (unclear why). All 
# others with attrition score are in "LR_TRAINING", "HR_NO_COACH", or "HR_COACH".

table(alg_eval_itt[!is.na(alg_eval_itt$confidence), "conditioning"])/8

# Restrict to CBM-I conditions that received attrition score. All of these are
# in the classification measure completer sample.

classified_conds <- c("LR_TRAINING", "HR_NO_COACH", "HR_COACH")

alg_eval_cbm_class <- alg_eval_itt[alg_eval_itt$conditioning %in% classified_conds, ]

all(alg_eval_cbm_class$class_meas_compl_anlys == 1)

# Note: 10 participants were classified manually (vs. with algorithm) to account 
# for a drop in attrition scores over the prior week

table(alg_eval_cbm_class$conditioning[alg_eval_cbm_class$risk_classification_method == "manual"],
      useNA = "always")/8

# Restrict to Session 2 and exclude "HR_COACH" condition (because being assigned
# a coach could influence attrition)

alg_eval_cbm_pure <- alg_eval_cbm_class[alg_eval_cbm_class$session_only == "secondSession" &
                                          alg_eval_cbm_class$conditioning != "HR_COACH", ]

# Compute predicted and truth labels, where 0 is "low risk" and 1 is "high risk"
# with respect to completing all Session 2 training and assessment tasks (given
# that training and assessment are done in series, we can focus on assessment)

alg_eval_cbm_pure$pred_label <- NA
alg_eval_cbm_pure$pred_label[alg_eval_cbm_pure$conditioning %in% "HR_NO_COACH"] <- "high_risk"
alg_eval_cbm_pure$pred_label[alg_eval_cbm_pure$conditioning %in% "LR_TRAINING"] <- "low_risk"


alg_eval_cbm_pure$truth_label <- NA
alg_eval_cbm_pure$truth_label[alg_eval_cbm_pure$compl_session_assess == 0] <- "high_risk"
alg_eval_cbm_pure$truth_label[alg_eval_cbm_pure$compl_session_assess == 1] <- "low_risk"

# Export data

write.csv(alg_eval_cbm_pure, "./data/temp/alg_eval_cbm_pure.csv", row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Evaluate algorithm performance ----
# ---------------------------------------------------------------------------- #

# Import data

alg_eval_cbm_pure <- read.csv("./data/temp/alg_eval_cbm_pure.csv")

# Convert predicted and truth labels to factors

lvs <- c("high_risk", "low_risk")

alg_eval_cbm_pure$pred_label  <- factor(alg_eval_cbm_pure$pred_label,  levels = lvs)
alg_eval_cbm_pure$truth_label <- factor(alg_eval_cbm_pure$truth_label, levels = lvs)

# Compute confusion matrix, accuracy, and F1-score

mat <- confusionMatrix(alg_eval_cbm_pure$pred_label, alg_eval_cbm_pure$truth_label)

mat
mat$byClass[c("Precision", "Recall", "F1")]

# ---------------------------------------------------------------------------- #
# Run Models in Parallel
# Authors: Jeremy W. Eberle and Jackie Huband
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script runs models in parallel via separate Slurm scripts that specify a 
# job array on Rivanna. Packages must first be installed via the prior script.

# Before running script, restart R and set working directory to "code" folder

# ---------------------------------------------------------------------------- #
# Load packages ----
# ---------------------------------------------------------------------------- #

# Load custom functions

source("./12a_define_parallel_analysis_functions.R")

# Load groundhog and use it to load other packages per groundhog_day defined
# in load_pkgs_via_groundhog() function (Slurm script specifies R version)

load_pkgs_via_groundhog()

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Set up cores for parallel code ----
# ---------------------------------------------------------------------------- #

# Obtain command-line arguments provided by Slurm script

cmdArgs <- commandArgs(trailingOnly = TRUE)

# Identify job array run number for index into parameter table

myNum <- as.integer(cmdArgs[1])
cat("\nmyNum =", myNum, "\n")

# Set up cores for parallel code (one core is reserved for manager)

numCores <- as.integer(cmdArgs[2]) - 1
cat("numCores =", numCores, "\n\n")

registerDoParallel(cores = numCores)

# Prevent job array from simultaneously reading files

Sys.sleep(myNum)

# ---------------------------------------------------------------------------- #
# Import data and initial values ----
# ---------------------------------------------------------------------------- #

load("../data/final_clean/wd_c1_corr_itt.RData")
load("../data/final_clean/wd_c1_corr_s5_train_compl.RData")

load("../data/final_clean/wd_c2_4_class_meas_compl.RData")
load("../data/final_clean/wd_c2_4_s5_train_compl.RData")

load("../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData")
load("../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData")

# Store initial values and data in lists

inits_all <- list(efficacy = inits_efficacy,
                  dropout  = inits_dropout)

dat_all <- list(c1_corr_itt            = wd_c1_corr_itt,
                c1_corr_s5_train_compl = wd_c1_corr_s5_train_compl,
                c2_4_class_meas_compl  = wd_c2_4_class_meas_compl,
                c2_4_s5_train_compl    = wd_c2_4_s5_train_compl)

# ---------------------------------------------------------------------------- #
# Run analyses ----
# ---------------------------------------------------------------------------- #

parameter_table <- create_parameter_table()

analysis_type <- parameter_table$analysis_type[myNum]
a_contrast <- parameter_table$a_contrast[myNum]
analysis_sample <- parameter_table$analysis_sample[myNum]
y_var <- parameter_table$y_var[myNum]

dat <- dat_all[[analysis_sample]]
inits <- inits_all[[analysis_type]]

total_iterations <- 20000

run_analysis(dat, analysis_type, inits, analysis_sample, a_contrast, y_var, total_iterations)

# TODO: Test efficacy and dropout models for "a1" by running "a1" Slurm script





# TODO: Test efficacy and dropout models for "a2_1", "a2_2", and "a2_3" by running
# "a2" Slurm script




# TODO: Remove unused variables from data (to resolve warnings)





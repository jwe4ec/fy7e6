# ---------------------------------------------------------------------------- #
# Run Models in Parallel on Standard Partition
# Authors: Jeremy W. Eberle and Jackie Huband
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script runs models in parallel via separate Slurm scripts that specify a 
# job array on Rivanna's standard partition (i.e., one node with multiple cores).
# Packages must first be installed via the prior script.

# Before running script, restart R and set working directory to "code" folder

# ---------------------------------------------------------------------------- #
# Load packages ----
# ---------------------------------------------------------------------------- #

# Load custom functions

source("./12a_define_functions_std_partition.R")

# Check R version

check_r_version()

# Check package versions installed

check_installed_pkg_ver("iterators", "1.0.14")
check_installed_pkg_ver("foreach", "1.5.2")
check_installed_pkg_ver("doParallel", "1.0.17")
check_installed_pkg_ver("fastDummies", "1.6.3")
check_installed_pkg_ver("rjags", "4-13")

# Load packages

library(iterators)
library(foreach)
library(doParallel)
library(fastDummies)
library(rjags)

# Print sessionInfo

sessionInfo()

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Set up cores for parallel analysis ----
# ---------------------------------------------------------------------------- #

# Obtain command-line arguments provided by Slurm script

cmdArgs <- commandArgs(trailingOnly = TRUE)

# Identify which model to run (job array run number indexes into parameter table)

myNum <- as.integer(cmdArgs[1])
cat("\nmyNum =", myNum, "\n")

# Set up cores for parallel analysis (one core is reserved for manager)

numCores <- as.integer(cmdArgs[2]) - 1
cat("numCores =", numCores, "\n\n")

registerDoParallel(cores = numCores)

# Prevent job array from simultaneously reading files

Sys.sleep(myNum)

# ---------------------------------------------------------------------------- #
# Import data and initial values ----
# ---------------------------------------------------------------------------- #

load("../data/final_clean/wd_c1_corr_itt.RData")
load("../data/final_clean/wd_c1_corr_itt_6800.RData")

load("../data/final_clean/wd_c1_corr_s5_train_compl.RData")
load("../data/final_clean/wd_c1_corr_s5_train_compl_6800.RData")

load("../data/final_clean/wd_c2_4_class_meas_compl.RData")
load("../data/final_clean/wd_c2_4_s5_train_compl.RData")

load("../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData")
load("../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData")

# Store initial values and data in lists

inits_all <- list(efficacy = inits_efficacy,
                  dropout  = inits_dropout)

dat_all <- list(c1_corr_itt                 = wd_c1_corr_itt,
                c1_corr_itt_6800            = wd_c1_corr_itt_6800,
                c1_corr_s5_train_compl      = wd_c1_corr_s5_train_compl,
                c1_corr_s5_train_compl_6800 = wd_c1_corr_s5_train_compl_6800,
                c2_4_class_meas_compl       = wd_c2_4_class_meas_compl,
                c2_4_s5_train_compl         = wd_c2_4_s5_train_compl)

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

if (a_contrast == "a1") {
  total_iterations <- 20000
} else if (a_contrast %in% c("a2_1", "a2_2", "a2_3")) {
  total_iterations <- 1000000
}

run_analysis(dat, analysis_type, inits, analysis_sample, a_contrast, y_var, total_iterations)
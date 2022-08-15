# ---------------------------------------------------------------------------- #
# Run Models in Parallel
# Authors: Jeremy W. Eberle and Jackie Huband
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to "code" folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Create directory for console output of each job in job array

dir.create("../jobs")

# Identify run number for index into parameter table

myNum <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("\nmyNum =", myNum, "\n")

Sys.sleep(myNum) # Prevents arrays jobs from simultaneously reading files

# Load custom functions

source("./01_define_functions.R")
source("./12_define_parallel_analysis_functions.R")

# TODO: Check correct R version, load groundhog package, and specify groundhog_day

# groundhog_day <- version_control()





# TODO: Load packages

# pkgs <- c("fastDummies", "rjags")
# groundhog.library(pkgs, groundhog_day)





library(fastDummies)
library(rjags)

library(parallel)
library(iterators)
library(foreach, quietly = TRUE)
library(doParallel)

# Set up cores for parallel code

numCores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
registerDoParallel(cores = numCores)

# Set seed

set.seed(1234)

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

# TODO: Test efficacy and dropout models for "a1" by running "a1" Slurm script

parameter_table <- create_parameter_table()

analysis_type <- parameter_table$analysis_type[myNum]
a_contrast <- parameter_table$a_contrast[myNum]
analysis_sample <- parameter_table$analysis_sample[myNum]
y_var <- parameter_table$y_var[myNum]

dat <- dat_all[[analysis_sample]]
inits <- inits_all[[analysis_type]]

total_iterations <- 10 # TODO: Change to 20000 for final run

run_analysis(dat, analysis_type, inits, analysis_sample, a_contrast, y_var, total_iterations)





# TODO: Test efficacy and dropout models for "a2_1", "a2_2", and "a2_3" by running
# "a2" Slurm script





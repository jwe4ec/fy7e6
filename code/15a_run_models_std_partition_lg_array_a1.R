# ---------------------------------------------------------------------------- #
# Run "a1" Contrast Models in Parallel on Standard Partition Using Large Array
# Authors: Jeremy W. Eberle and Jackie Huband
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script runs "a1" contrast models with a large number of bootstrap samples 
# in parallel via a separate Slurm script that specifies a large job array on 
# Rivanna's standard partition (i.e., one node with multiple cores). But rather 
# than using a job array to submit all "a1" models at once (and many cores/node 
# to run model across bootstrap samples) as in "13a_run_models_std_partition.R",
# the present script runs one "a1" model at a time (specified by "myNum" in Slurm 
# script) and uses a larger job array to run the model across bootstrap samples 
# (with each node analyzing only one bootstrap sample). Once a given model has
# run, "myNum" in Slurm script must be manually updated to run the next model.

# "a2" contrast models, which involve no bootstrapping, were run on the standard 
# partition using "13a_run_models_std_partition.R"

# Before running script, restart R and set working directory to "code" folder

# ---------------------------------------------------------------------------- #
# Check correct R version and load packages ----
# ---------------------------------------------------------------------------- #

# Load custom functions

source("./12a_define_functions_std_partition.R")

# Check R version

check_r_version()

# Check package versions installed

check_installed_pkg_ver("fastDummies", "1.6.3")
check_installed_pkg_ver("rjags", "4-13")

# Load packages

library(fastDummies)
library(rjags)

# Print sessionInfo

sessionInfo()

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Set up node for analysis
# ---------------------------------------------------------------------------- #

# Obtain command-line arguments provided by Slurm script

cmdArgs <- commandArgs(trailingOnly = TRUE)

# Identify which model to run (manually specified number indexes into parameter table)

myNum <- as.integer(cmdArgs[1])

# Identify which bootstrap sample to analyze (job array run number)

bs_sample_num <- as.integer(cmdArgs[2])

# ---------------------------------------------------------------------------- #
# If needed, create directory for output files
# ---------------------------------------------------------------------------- #

out_path <- paste0("./results_for_model_", myNum)

if (!dir.exists(out_path)) {
  dir.create(out_path)
}

# ---------------------------------------------------------------------------- #
# Import data and initial values ----
# ---------------------------------------------------------------------------- #

# Note: Only data based on 2000 bootstrap samples for "a1" models are loaded; we no longer 
# load data based on 500 bootstrap samples for "a1" models or data for "a2" models.

load(paste0("../data/final_clean/wd_c1_corr_itt_2000.RData_", bs_sample_num))
load(paste0("../data/final_clean/wd_c1_corr_s5_train_compl_2000.RData_", bs_sample_num))

load(paste0("../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData_", bs_sample_num))
load(paste0("../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData_", bs_sample_num))

# Store initial values and data in lists

inits_all <- list(efficacy = inits_efficacy,
                  dropout  = inits_dropout)

dat_all <- list(c1_corr_itt_2000            = wd_c1_corr_itt_2000,
                c1_corr_s5_train_compl_2000 = wd_c1_corr_s5_train_compl_2000)

# ---------------------------------------------------------------------------- #
# Run "a1" contrast analyses ----
# ---------------------------------------------------------------------------- #

# Create parameter table and select values for given row based on "myNum"

parameter_table <- create_parameter_table()

analysis_type   <- parameter_table$analysis_type[myNum]
a_contrast      <- parameter_table$a_contrast[myNum]
analysis_sample <- parameter_table$analysis_sample[myNum]
y_var           <- parameter_table$y_var[myNum]

dat <- dat_all[[analysis_sample]]
inits <- inits_all[[analysis_type]]

# Set total number of MCMC iterations

if (a_contrast == "a1") {
  total_iterations <- 20000
} else if (a_contrast %in% c("a2_1", "a2_2", "a2_3")) {
  total_iterations <- 1000000
}

# Run analysis based on "a_contrast"

if (a_contrast == "a1") {
  # Specify "jags_dat" and run JAGS model for each bootstrap sample in parallel
  
  jags_dat <- specify_jags_dat(dat[[bs_sample_num]], a_contrast, y_var)
  
  results_list <- list(num = run_jags_model(analysis_type, bs_sample = bs_sample_num, analysis_sample,
                                            jags_dat, inits,
                                            a_contrast, y_var, total_iterations))
  names(results_list) <- bs_sample_num
  
  # Save results list for a given bootstrap sample. Results across bootstrap
  # samples are concatenated in a separate script.
  
  outfile <- paste0(out_path, "/results_for_bs_sample_", bs_sample_num, ".RData")
  save(results_list, file = outfile)
  
} else if (a_contrast %in% c("a2_1", "a2_2", "a2_3")) {
  
  stop('This script is only for running "a1" contrast models that require a large
       job array due to a large number of bootstrap samples. "a2" contrast models, 
       which involve no bootstrapping, were run in a separate script.')
}

cat("\nAll done\n")
# ---------------------------------------------------------------------------- #
# Trim Results
# Authors: Jeremy W. Eberle
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
# Transfer jobs and results from Rivanna ----
# ---------------------------------------------------------------------------- #

# Use Globus to transfer "jobs/" and "results/out/" folders for "a1" and "a2"
# models from "scratch/" directory on Rivanna to local computer

# ---------------------------------------------------------------------------- #
# Trim results ----
# ---------------------------------------------------------------------------- #

# Define function to remove "jags_model", "model_samples", and "model_res" from 
# "results.RData" so outputted "results_trim.RData" is small enough to load for 
# many models at once

trim_results <- function(anlys_path_pattern) {
  res_dir <- "./results/bayesian"
  
  res_filenames <- list.files(res_dir, pattern = "results.RData", 
                              recursive = TRUE, full.names = FALSE)
  res_filenames <- res_filenames[grep(anlys_path_pattern, res_filenames)]
  
  objects_to_remove <- c("jags_model", "model_samples", "model_res")
  
  for (i in 1:length(res_filenames)) {
    cat("On file", i, "\n")
    
    load(paste0(res_dir, "/", res_filenames[i]))
    
    results_trim <- results
    
    if (anlys_path_pattern %in% c("dropout/out/c1_", "efficacy/out/c1_")) {
      for (j in 1:length(results_trim$per_bs_smp)) {
        cat("On sample", j, "\n")
        
        results_trim$per_bs_smp[[j]] <-
          results_trim$per_bs_smp[[j]][!(names(results_trim$per_bs_smp[[j]]) %in% objects_to_remove)]
      }
    } else if (anlys_path_pattern %in% c("dropout/out/c2_4_", "efficacy/out/c2_4_")) {
        results_trim <- results_trim[!(names(results_trim) %in% objects_to_remove)]
    }
    
    results_trim_filename <- sub("results.RData", "results_trim.RData", res_filenames[i])
    
    cat("Saving trimmed file", i, "\n")
    
    save(results_trim, file = paste0(res_dir, "/", results_trim_filename))
    
    rm(results, results_trim)
  }
}

# Run function for each analysis type for "a1" ("c1_") and "a2" ("c2_4_") models

trim_results("dropout/out/c1_")
trim_results("dropout/out/c2_4_")
trim_results("efficacy/out/c1_")
trim_results("efficacy/out/c2_4_")
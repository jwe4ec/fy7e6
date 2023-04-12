# ---------------------------------------------------------------------------- #
# Concatenate Results for "a1" Contrast Models Run in Large Job Array
# Authors: Jackie Huband and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script uses a job array to concatenate each "a1" model's "results_list" 
# files across bootstrap samples into one results list for that model. It is run 
# on Rivanna's standard partition via a separate Slurm script.

# Before running script, restart R, set working directory to "code" folder, and
# move the "results_for_model_<myNum>" folders containing the models' results for 
# each bootstrap sample to a folder "a1_outputs" alongside the "code" folder.

# The concatenated results list "results_for_model_<myNum>.RData" will be saved
# in the "code" folder.

# ---------------------------------------------------------------------------- #
# Concatenate "results_list" files into one list for the model
# ---------------------------------------------------------------------------- #

myNum <- as.integer(commandArgs(trailingOnly = TRUE))

results_path <- paste0("../a1_outputs/results_for_model_", myNum)

cat("\nProcessing files in ", results_path, "\n")

if (file.exists(results_path)) {
  # Concatenate files into list
  
  files <- list.files(results_path, pattern = ".RData", full.names = TRUE)

  combined_list <- NULL
  
  for (myfile in files) {
   load(myfile)
   combined_list <- c(combined_list, results_list)
   rm(results_list)
  }
  
  # Check length of list (should equal number of bootstrap samples)
  
  print(length(combined_list))
 
  # Create and save results object
  
  results <- list(per_bs_smp = combined_list)
  
  outfile <- paste0("results_for_model_", myNum, ".RData")
  save(results, file = outfile)
}
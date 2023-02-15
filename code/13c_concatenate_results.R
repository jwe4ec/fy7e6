# ---------------------------------------------------------------------------- #
# Concatenate Results for "a1" Contrast Models Run in Large Job Array
# Authors: Jackie Huband and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script concatenates "results_list" files for a given model. It is run on
# the standard partition via a separate Slurm script.

# Before running script, restart R and set working directory to "code" folder

# ---------------------------------------------------------------------------- #
# TODO (check this): Check number of "results_list" files for a model
# ---------------------------------------------------------------------------- #

test_num <- as.integer(commandArgs(trailingOnly = TRUE))
results_path <- paste0("../a1_outputs/test_results_", test_num)
cat("\nProcessing files in ", results_path, "\n")
if (file.exists(results_path)) {
  files <- list.files(results_path, pattern = ".RData", full.names = TRUE)

   # ---------------------------------------------------------------------------- #
   # TODO (check this): Concatenate "results_list" files into one list for the model
   # ---------------------------------------------------------------------------- #

   combined_list <- NULL

   for (myfile in files) {
     load(myfile)
     combined_list <- c(combined_list, results_list)
     rm(results_list)
   }

   print(length(combined_list))
 
   # ---------------------------------------------------------------------------- #
   # TODO (check this): Complete work that was in the original code
   # ---------------------------------------------------------------------------- #
   
   model_results_path_stem <- combined_list[[1]]$model_results_path_stem

   # Create results object

   results <- list(per_bs_smp = combined_list)
  
   outfile <- paste0("results_", test_num, ".RData")
   save(results, file = outfile)
}
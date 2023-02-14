# ---------------------------------------------------------------------------- #
# Concatenate Results for "a1" Contrast Models Run in Parallel on Parallel Partition
# Authors: Jackie Huband and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script concatenates "results_list" files for a given model. It is run on
# the standard partition via a separate Slurm script.

# Before running script, restart R and set working directory to "code" folder

# ---------------------------------------------------------------------------- #
# Check number of "results_list" files for a model
# ---------------------------------------------------------------------------- #

# TODO: Seems this won't work now that we have "test_results_1", "test_results_2",
# etc. now for different models, correct?





files <- list.files("./test_results", pattern = ".RData", full.names = TRUE)

cat("\nNumber of files: ", length(files), "\n")

# ---------------------------------------------------------------------------- #
# Concatenate "results_list" files into one list for the model
# ---------------------------------------------------------------------------- #

combined_list <- NULL

for (myfile in files) {
  load(myfile)
  print(names(results_list))
  combined_list <- c(combined_list, results_list)
  rm(results_list)
}

print(length(combined_list))
print(names(combined_list))

save(combined_list, file = "combined_list.RData")
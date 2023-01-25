# TODO: Is this script needed? Except for the section "Check for any missing
# bootstrap values", it seems redundant with "14d_test_concatenate_results.R"





# Get a list of the output files
files <- list.files("./test_results", pattern=".RData", full.names=TRUE)
cat("\nNumber of files: ", length(files), "\n")

# Read each of the output files and get the bootstrap number
combined_list <- NULL
for (myfile in files){
   load(myfile)
   #print(names(results_list))
   combined_list <- c(combined_list, names(results_list))
   rm(results_list)
}

print(length(combined_list))

# Check for any missing bootstrap values
combined_list <- sort(as.integer(combined_list))
full_list <- 1:6800
print(setdiff(full_list, combined_list))
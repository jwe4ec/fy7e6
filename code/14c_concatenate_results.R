
files <- list.files("./test_results", pattern=".RData", full.names=TRUE)

cat("\nNumber of files: ", length(files), "\n")

combined_list <- NULL
for (myfile in files){
   load(myfile)
   print(names(results_list))
   combined_list <- c(combined_list, results_list)
   rm(results_list)
}

print(length(combined_list))
print(names(combined_list))

save(combined_list, file="combined_list.RData")


# ---------------------------------------------------------------------------- #
# Document data filenames ----
# ---------------------------------------------------------------------------- #

# TODO: Output file names to TXT

dir.create("./docs")

sink(file = "./docs/data_filenames.txt")
print(list.files("./data", recursive = TRUE, full.names = FALSE), width = 80)
sink()
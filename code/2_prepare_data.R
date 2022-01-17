# ---------------------------------------------------------------------------- #
# Prepare Data
# Author: Jeremy W. Eberle
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

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import intermediate clean data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of intermediate clean CSV data files

int_cln_data_dir <- paste0(wd_dir, "/data/intermediate_clean")
filenames <- list.files(int_cln_data_dir, pattern = "*.csv", full.names = FALSE)

# Output file names to TXT

dir.create("./docs")

sink(file = "./docs/data_filenames.txt")

cat("In './data/intermediate_clean'", "\n")
cat("\n")
print(filenames)

sink()

# Import tables into list and name tables

dat <- lapply(paste0(int_cln_data_dir, "/", filenames), read.csv)
names(dat) <- sub(".csv", "", filenames)

# Convert system-generated timestamps to POSIXct data types

dat <- convert_POSIXct(dat)

# ---------------------------------------------------------------------------- #
# TODO ----
# ---------------------------------------------------------------------------- #






# ---------------------------------------------------------------------------- #
# Search for Auxiliary Variables
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
# Import data ----
# ---------------------------------------------------------------------------- #

completion <- read.csv("./data/temp/completion.csv")
load("./data/intermediate_clean_further/dat2.RData")




# ---------------------------------------------------------------------------- #
# Search for time-invariant categorical and ordinal auxiliary variables ----
# ---------------------------------------------------------------------------- #

# TODO: Per consultation with Cynthia Tong on 2/22/22, compute proportion of missing 
# data across time points within each level (treat ordinal variables as categorical)





# ---------------------------------------------------------------------------- #
# Search for time-invariant continuous auxiliary variables ----
# ---------------------------------------------------------------------------- #

# TODO: Per consultation with Cynthia Tong on 2/22/22, compute correlation with 
# proportion of missing data across time points





# ---------------------------------------------------------------------------- #
# Search for time-varying categorical auxiliary variables ----
# ---------------------------------------------------------------------------- #

# TODO: Per consultation with Cynthia Tong on 2/22/22, compute proportion of missing
# data within each level at each time point





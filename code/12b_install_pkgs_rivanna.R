# ---------------------------------------------------------------------------- #
# Install Packages on Rivanna
# Authors: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script installs packages on the Rivanna supercomputer at the University
# of Virginia before models are run in parallel. It should be run to completion
# via the separate Slurm script before running models in parallel.

# Before running script, restart R and set working directory to "code" folder

# ---------------------------------------------------------------------------- #
# Install packages ----
# ---------------------------------------------------------------------------- #

# Load custom functions

source("./12a_define_parallel_analysis_functions.R")

# Install groundhog and use it to install other packages per groundhog_day defined
# in load_pkgs_via_groundhog() function (Slurm script specifies R version)

install.packages("groundhog", repos = "https://cran.case.edu")
load_pkgs_via_groundhog()
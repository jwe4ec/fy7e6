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

source("./12a_define_functions_std_partition.R")

# Check R version

check_r_version()

# Install packages (if needed)

parallel_pkgs <- c("iterators", "foreach", "doParallel")
anlys_pkgs <- c("fastDummies", "rjags")

pkgs <- c(parallel_pkgs, anlys_pkgs)

for (i in 1:length(pkgs)) {
  if (!require(pkgs[i], quietly = TRUE)) {
    install.packages(pkgs[i], repos = "https://cran.case.edu")
  }
}
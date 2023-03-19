# ---------------------------------------------------------------------------- #
# Create Plots
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

source("./code/01_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control_tables_plots()

# Load packages

groundhog.library(ggplot2, groundhog_day, tolerate.R.version = "4.1.2")

# ---------------------------------------------------------------------------- #
# Import results and parameter labels ----
# ---------------------------------------------------------------------------- #

load("./results/bayesian/pooled/w_marg_effects/res_trm_eff_c1_2000bs.RData")

# ---------------------------------------------------------------------------- #
# Plot time-specific means by treatment arm for "a1" models ----
# ---------------------------------------------------------------------------- #

# TODO: Define function to INSERT

res_trm_eff_c1_2000bs$efficacy_c1_corr_itt_2000_a1_bbsiq_ben_m$marg_tbl



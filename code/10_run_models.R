# ---------------------------------------------------------------------------- #
# Run Models
# Authors: Jeremy W. Eberle
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

groundhog_day <- version_control()

# Load packages

groundhog.library(rjags, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data and initial values ----
# ---------------------------------------------------------------------------- #

load("./data/final_clean/wd_c1_corr_itt.RData")
load("./data/final_clean/wd_c1_corr_s5_train_compl.RData")

load("./data/final_clean/wd_c2_4_class_meas_compl.RData")
load("./data/final_clean/wd_c2_4_s5_train_compl.RData")

load("./results/bayesian/model_and_initial_values/inits.RData")

# ---------------------------------------------------------------------------- #
# Define "specify_jags_dat()" ----
# ---------------------------------------------------------------------------- #

# Define function to specify JAGS data for non-OASIS (4 time points) and OASIS
# (7 time points) outcomes for a given sample and contrast

specify_jags_dat <- function(df, a_contrast, y_var) {
  if (y_var %in% c("rr_neg_threat_m", "rr_pos_threat_m",
                   "bbsiq_neg_m", "bbsiq_ben_m", "dass21_as_m")) {
    assessed_at_j <- c(1, 4, 6, 7)
    t1            <- c(0, 3, 5, 5)
    t2            <- c(0, 0, 0, 1)
  } else if (y_var == "oa_m") {
    assessed_at_j <- c(1, 2, 3, 4, 5, 6, 7)
    t1            <- c(0, 1, 2, 3, 4, 5, 5)
    t2            <- c(0, 0, 0, 0, 0, 0, 1)
  }
  
  y_mat <- as.matrix(df[, paste0(y_var, ".", assessed_at_j)])
  
  jags_dat <- list(N                   = length(df[, "participant_id"]),
                   J                   = length(assessed_at_j),
                   income              = df[, "income_dollar_ctr"],
                   age                 = df[, "age_ctr"],
                   employment_stat_col = df[, "employment_stat_col"],
                   marital_stat_col    = df[, "marital_stat_col"],
                   confident_online    = df[, "confident_online"],
                   important           = df[, "important"],
                   gender_col          = df[, "gender_col"],
                   device_col_bin      = df[, "device_col_bin"],
                   a                   = df[, a_contrast],
                   t1                  = t1, 
                   t2                  = t2,
                   y                   = y_mat)
}

# ---------------------------------------------------------------------------- #
# Define "run_jags_model()" ----
# ---------------------------------------------------------------------------- #

# Define function to run JAGS model

run_jags_model <- function(sample, jags_dat, inits,
                           a_contrast, y_var, total_iterations) {
  # Create JAGS model
  
  jags_model <- jags.model(file = "./results/bayesian/model_and_initial_values/model_string.txt",
                           data = jags_dat, inits = inits, n.chains = 1)

  # Prevent printing in scientific notation

  options(scipen = 999)
  
  # Specify total iterations and burn-in period (typically half)
  
  total_iterations     <- total_iterations                
  burn_iterations      <- total_iterations/2
  remaining_iterations <- total_iterations - burn_iterations
  
  # Run model, tracking elapsed time

  time0 <- proc.time()
  update(jags_model, n.iter = burn_iterations)
  (burn_in_time <- proc.time() - time0)

  time0 <- proc.time()
  model_samples <- coda.samples(jags_model,
                                "para",
                                n.iter = remaining_iterations)
  (sampling_time <- proc.time() - time0)

  # Save posterior samples, reload them, and create MCMC object

  path1 <- paste0("./results/bayesian/output/", sample, "_", a_contrast, "_", y_var)
  dir.create(path1, recursive = TRUE)

  path2 <- paste0("/burn_", burn_iterations,
                  "_total_", total_iterations)
  dir.create(paste0(path1, path2))

  save(model_samples,
       file = paste0(path1, path2, "/model_samples.RData"))
  load(paste0(path1, path2, "/model_samples.RData"))

  model_res <- as.mcmc(do.call(rbind, model_samples))

  # Save plots

  pdf(file = paste0(path1, path2, "/plots.pdf"))
  par(mfrow=c(4,2))
  plot(model_res)
  dev.off()

  # Save results and diagnostics

  sink(file = paste0(path1, path2, "/results and diagnostics.txt"))
  
  print(paste0("Sample: ", sample))
  print(paste0("Contrast: ", a_contrast))
  print(paste0("Outcome: ", y_var))
  cat("\n")

  print(paste0("Total Iterations: ", total_iterations))
  print(paste0("Burn Iterations: ", burn_iterations))
  print(paste0("Remaining Iterations: ", remaining_iterations))
  cat("\n")

  print("Burn-In Time:")
  print(burn_in_time)
  cat("\n")
  print("Sampling Time:")
  print(sampling_time)
  cat("\n")

  print("Summary:")
  print(summary(model_res))

  print("HPD Credible Intervals:")
  cat("\n")
  print(HPDinterval(model_res))
  cat("\n")

  print("Geweke's convergence diagnostic:")
  geweke <- geweke.diag(model_res)
  print(geweke)
  print("Parameters in (-1.96, 1.96):")
  cat("\n")
  print(geweke$z > -1.96 & geweke$z < 1.96)
  cat("\n")
  print(paste0("All parameters in (-1.96, 1.96): ",
               all(geweke$z > -1.96 & geweke$z < 1.96)))
  
  sink()
}

# ---------------------------------------------------------------------------- #
# Run models ----
# ---------------------------------------------------------------------------- #

# TODO: Test individual models and potentially combine functions

jags_dat <- specify_jags_dat(wd_c1_corr_itt[[1]], "a1", "bbsiq_neg_m")
run_jags_model("c1_corr_itt", jags_dat, inits, "a1", "bbsiq_neg_m", 10)

jags_dat <- specify_jags_dat(wd_c1_corr_itt[[1]], "a1", "oa_m")
run_jags_model("c1_corr_itt", jags_dat, inits, "a1", "oa_m", 10)





# TODO: Pool results across 500 bootstrap samples for models that converge





# TODO: Iterate across all samples, contrasts, and outcomes

samples <- c("c1_corr_itt", "c1_corr_s5_train_compl",
             "c2_4_class_meas_compl", "c2_4_s5_train_compl")

a_contrasts <- c("a1", "a2_1", "a2_2", "a2_3")

y_vars <- c("rr_neg_threat_m", "rr_pos_threat_m",
            "bbsiq_neg_m", "bbsiq_ben_m", 
            "oa", "dass21_as_m")





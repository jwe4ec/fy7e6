# ---------------------------------------------------------------------------- #
# Run Models in Series
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

pkgs <- c("fastDummies", "rjags")
groundhog.library(pkgs, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import initial values and data ----
# ---------------------------------------------------------------------------- #

load("./results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData")
load("./results/bayesian/dropout/model_and_initial_values/inits_dropout.RData")

load("./data/final_clean/wd_c1_corr_itt.RData")
load("./data/final_clean/wd_c1_corr_s5_train_compl.RData")
load("./data/final_clean/wd_c2_4_class_meas_compl.RData")
load("./data/final_clean/wd_c2_4_s5_train_compl.RData")

# Store initial values and data in lists

inits_all <- list(efficacy = inits_efficacy,
                  dropout  = inits_dropout)

dat_all <- list(c1_corr_itt            = wd_c1_corr_itt,
                c1_corr_s5_train_compl = wd_c1_corr_s5_train_compl,
                c2_4_class_meas_compl  = wd_c2_4_class_meas_compl,
                c2_4_s5_train_compl    = wd_c2_4_s5_train_compl)

# ---------------------------------------------------------------------------- #
# Define "impute_mcar_nominal()" ----
# ---------------------------------------------------------------------------- #

# Define function to impute missing values for nominal variables we assume are
# MCAR by randomly assigning levels to NAs in accordance with the distribution 
# of observed levels

impute_mcar_nominal <- function(df, mcar_nominal_vars) {
  for (i in 1:length(mcar_nominal_vars)) {
    nominal_var <- mcar_nominal_vars[i]
    
    imp_nominal_var <- paste0(nominal_var, "_imp")
    
    # Get numeric levels of nominal variable, excluding NA
    
    numeric_levels <- as.numeric(df[, nominal_var][!is.na(df[, nominal_var])])
    numeric_levels <- sort(unique(numeric_levels))
    
    # Compute number of NAs in nominal variable and proportions of observed levels
    
    number_of_nas <- sum(is.na(df[, nominal_var]))
    level_probabilities <- prop.table(table(df[, nominal_var]))
    
    # Randomly assign levels to NAs weighted by proportions of observed levels
    
    weighted_rnd_numeric_levels <- sample(numeric_levels, size = number_of_nas, 
                                          replace = TRUE, prob = level_probabilities)
    weighted_rnd_character_levels <- levels(df[, nominal_var])[weighted_rnd_numeric_levels]
    
    df[, imp_nominal_var] <- df[, nominal_var]
    df[, imp_nominal_var][is.na(df[, imp_nominal_var])] <- weighted_rnd_character_levels
  }
  
  return(df)
}

# ---------------------------------------------------------------------------- #
# Define "dummy_code()" ----
# ---------------------------------------------------------------------------- #

# Define function to dummy code certain nominal variables. Make the most frequent 
# level the reference group.

dummy_code <- function(df, nominal_vars) {
  df <- dummy_cols(df, 
                   select_columns = nominal_vars,
                   remove_most_frequent_dummy = TRUE,
                   ignore_na = TRUE)
  
  names(df) <- gsub(",", "",  names(df))
  names(df) <- gsub("/", "_", names(df))
  names(df) <- gsub(" ", "_", names(df))
  
  return(df)
}

# ---------------------------------------------------------------------------- #
# Define "specify_jags_dat()" ----
# ---------------------------------------------------------------------------- #

# Define function to specify JAGS data for time-varying efficacy (non-OASIS = 4 
# time points, OASIS = 7 time points) and time-invariant treatment dropout outcomes 
# for given sample and contrast

specify_jags_dat <- function(df, a_contrast, y_var) {
  # Restrict to groups involved in "a1" or "a2" contrast (i.e., where contrast
  # code is 1 or -1)
  
  df <- df[df[, a_contrast] %in% c(-1, 1), ]

  # Impute missing values for nominal variables we assume are MCAR

  mcar_nominal_vars <- c("employment_stat_col", "marital_stat_col", "gender_col")
  df <- impute_mcar_nominal(df, mcar_nominal_vars)
  
  # Dummy code nominal variables given that JAGS does not recognize them
  
  nominal_vars <- c("employment_stat_col_imp", "marital_stat_col_imp", "gender_col_imp", 
                    "device_col_bin")
  df <- dummy_code(df, nominal_vars)
  
  # Compute mean and precision for age and confident_online based on observed data
  
  mu_age <- mean(df$age, na.rm = TRUE)
  mu_confident_online <- mean(df$confident_online, na.rm = TRUE)
  
  inv_sig_age              <- 1/var(df$age, na.rm = TRUE)
  inv_sig_confident_online <- 1/var(df$confident_online, na.rm = TRUE)
  
  # Combine time-invariant elements (except y) into list
  
  jags_dat <- list(N                        = length(df[, "participant_id"]),
                   
                   income                   = df[, "income_dollar"],
                   age                      = df[, "age"],
                   
                   employment_stat_col      = df[, "employment_stat_col"],
                   marital_stat_col         = df[, "marital_stat_col"],
                   gender_col               = df[, "gender_col"],
                   
                   employment_stat_col_imp  = df[, "employment_stat_col_imp"],
                   marital_stat_col_imp     = df[, "marital_stat_col_imp"],
                   gender_col_imp           = df[, "gender_col_imp"],
                   
                   device_col_bin           = df[, "device_col_bin"],
                   
                   empl_Student             = df[, "employment_stat_col_imp_Student"],
                   empl_Other_or_Unknown    = df[, "employment_stat_col_imp_Other_or_Unknown"],
                   mrtl_Single_or_Dating    = df[, "marital_stat_col_imp_Single_or_Dating"],
                   mrtl_Sep_Div_or_Wid      = df[, "marital_stat_col_imp_Separated_Divorced_or_Widowed"],
                   mrtl_Other               = df[, "marital_stat_col_imp_Other"],
                   gndr_Male                = df[, "gender_col_imp_Male"],
                   gndr_Trans_or_Other      = df[, "gender_col_imp_Transgender_Other"],
                   
                   dvce_multiple_types      = df[, "device_col_bin_multiple_types"],
                   
                   confident_online         = df[, "confident_online"],
                   important                = df[, "important"],
                   
                   mu_age                   = mu_age,
                   mu_confident_online      = mu_confident_online,
                   inv_sig_age              = inv_sig_age,
                   inv_sig_confident_online = inv_sig_confident_online,
                   
                   a                        = df[, a_contrast])
  
  # Revise list for specific outcome
  
  if (y_var == "miss_session_train_sum") {
    # For time-invariant outcome
    
      # Add time-invariant y to list
    
    jags_dat_time_invariant <- list(y = df[, y_var])
    
    jags_dat <- append(jags_dat, jags_dat_time_invariant)
    
      # Remove unneeded elements
    
    rm_elements <- c("gender_col", "gender_col_imp", "device_col_bin", 
                     "gndr_Male", "gndr_Trans_or_Other", "dvce_multiple_types")
    
    jags_dat[rm_elements] <- NULL
    
  } else {
    # For time-varying outcome
    
      # Define piecewise linear time variables
    
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
    
      # Create matrix for outcome 
    
    y_mat <- as.matrix(df[, paste0(y_var, ".", assessed_at_j)])
    
      # Create missing data indicator matrix for outcome (0 = present, 1 = missing)
    
    r_mat <- is.na(y_mat)
    
      # Add time-varying elements to list
    
    jags_dat_time_varying <- list(J  = length(assessed_at_j),
                                  t1 = t1,
                                  t2 = t2,
                                  y  = y_mat,
                                  r  = r_mat)
    
    jags_dat <- append(jags_dat, jags_dat_time_varying)
  }
  
  return(jags_dat)
}

# ---------------------------------------------------------------------------- #
# Define "run_jags_model()" ----
# ---------------------------------------------------------------------------- #

# Define function to run JAGS model

run_jags_model <- function(analysis_type, bs_sample, analysis_sample, 
                           jags_dat, inits,
                           a_contrast, y_var, total_iterations) {
  # Create JAGS model
  
  model_string_path <- paste0("./results/bayesian/", analysis_type, 
                              "/model_and_initial_values/model_string.txt")
  
  time0 <- proc.time()
  jags_model <- jags.model(file = model_string_path,
                           data = jags_dat, inits = inits, n.chains = 1)
  create_model_time <- proc.time() - time0

  # Prevent printing in scientific notation

  options(scipen = 999)
  
  # Specify total iterations and burn-in period (typically half)
  
  total_iterations     <- total_iterations                
  burn_iterations      <- total_iterations/2
  remaining_iterations <- total_iterations - burn_iterations
  
  # Run model, tracking elapsed time

  time0 <- proc.time()
  update(jags_model, n.iter = burn_iterations)
  burn_in_time <- proc.time() - time0

  time0 <- proc.time()
  model_samples <- coda.samples(jags_model,
                                c("beta", "gamma", "para"),
                                n.iter = remaining_iterations)
  sampling_time <- proc.time() - time0
  
  total_time <- create_model_time + burn_in_time + sampling_time

  # Create directory for saving model results, including results per bootstrap 
  # sample (if applicable)
  
  path1 <- paste0("./results/bayesian/", analysis_type, "/out/",
                  analysis_sample, "_", a_contrast, "_", y_var)
  path2 <- paste0("/burn_", burn_iterations,
                  "_total_", total_iterations)
  model_results_path_stem <- paste0(path1, path2)
  
  path3 <- switch(is.null(bs_sample) + 1, paste0("/per_bs_smp/", bs_sample), NULL)
  model_results_path_specific <- paste0(model_results_path_stem, path3)

  dir.create(model_results_path_specific, recursive = TRUE)
  
  # Save posterior samples, reload them, and create MCMC object

  save(model_samples,
       file = paste0(model_results_path_specific, "/model_samples",
                     switch(is.null(bs_sample) + 1, paste0("_", bs_sample), NULL),
                     ".RData"))
  
  model_res <- as.mcmc(do.call(rbind, model_samples))

  # Save plots

  pdf(file = paste0(model_results_path_specific, "/plots",
                    switch(is.null(bs_sample) + 1, paste0("_", bs_sample), NULL),
                    ".pdf"))
  par(mfrow=c(4,2))
  plot(model_res)
  dev.off()

  # Save results and diagnostics
  
  sink(file = paste0(model_results_path_specific, "/results_and_dx",
                     switch(is.null(bs_sample) + 1, paste0("_", bs_sample), NULL),
                     ".txt"))
  
  print(paste0("Analysis Type: ", analysis_type))
  cat("\n")
  
  if (!is.null(bs_sample)) {
    print(paste0("Bootstrap Sample: ", bs_sample))
    cat("\n")
  }
  
  print(paste0("Analysis Sample: ", analysis_sample))
  print(paste0("Contrast: ", a_contrast))
  print(paste0("Outcome: ", y_var))
  cat("\n")

  print(paste0("Total Iterations: ", total_iterations))
  print(paste0("Burn Iterations: ", burn_iterations))
  print(paste0("Remaining Iterations: ", remaining_iterations))
  cat("\n")
  
  print("Model Creation Time (seconds):")
  print(create_model_time)
  cat("\n")
  print("Burn-In Time (seconds):")
  print(burn_in_time)
  cat("\n")
  print("Sampling Time (seconds):")
  print(sampling_time)
  cat("\n")
  print("Total Time (seconds):")
  print(total_time)
  cat("\n")

  print("Summary:")
  summary <- summary(model_res)
  print(summary)

  print("HPD Credible Intervals:")
  cat("\n")
  hpd_interval <- HPDinterval(model_res)
  print(hpd_interval)
  cat("\n")

  print("Geweke's convergence diagnostic:")
  geweke <- geweke.diag(model_res)
  print(geweke)
  print("Parameters in (-1.96, 1.96):")
  cat("\n")
  geweke_converge_each <- geweke$z > -1.96 & geweke$z < 1.96
  print(geweke_converge_each)
  cat("\n")
  geweke_converge_all <- all(geweke_converge_each)
  print(paste0("All parameters in (-1.96, 1.96): ", geweke_converge_all))
  
  sink()
  
  # Save results in list
  
  results <- list(analysis_type = analysis_type,
                  bs_sample = bs_sample,
                  analysis_sample = analysis_sample,
                  a_contrast = a_contrast,
                  y_var = y_var,
                  model_results_path_stem = model_results_path_stem,
                  model_results_path_specific = model_results_path_stem,
                  jags_model = jags_model,
                  total_iterations = total_iterations,
                  burn_iterations = burn_iterations,
                  remaining_iterations = remaining_iterations,
                  create_model_time = create_model_time,
                  burn_in_time = burn_in_time,
                  sampling_time = sampling_time,
                  total_time = total_time,
                  model_samples = model_samples,
                  model_res = model_res,
                  summary = summary,
                  hpd_interval = hpd_interval,
                  geweke = geweke,
                  geweke_converge_each = geweke_converge_each,
                  geweke_converge_all = geweke_converge_all)
  
  return(results)
}

# ---------------------------------------------------------------------------- #
# Define "pool_results()" ----
# ---------------------------------------------------------------------------- #

# Define function to pool results across bootstrap samples for converged models

pool_results <- function(results_list) {
  # Restrict to converged models (models in which all parameters pass Geweke's test)
  
  # TODO: For testing, temporarily pool across models that *didn't* converge
  
  
  
  
  
  results_list_converged <- Filter(function(x) x$geweke_converge_all == FALSE, results_list)
  
  number_converged <- length(results_list_converged)
  
  # Extract means and SDs of estimated parameters for each model
  
  results_list_converged_stats <- lapply(results_list_converged, function(x) x[["summary"]]$statistics)
  
  results_list_converged_stats_mean <- lapply(results_list_converged_stats, function(x) x[, "Mean"])
  results_list_converged_stats_sd   <- lapply(results_list_converged_stats, function(x) x[, "SD"])
  
  # Pool results across models by computing (a) average of estimated means, (b) 
  # percentile bootstrap 95% CIs for average of estimated means (2.5th and 97.5th
  # percentiles of estimated means), (c) empirical SD (SD of estimated means), and 
  # (d) average SD (mean of estimated SDs)
  
  pooled_mean   <- apply(simplify2array(results_list_converged_stats_mean), 1, mean)
  pooled_pctl_bs_ci_ll <- apply(simplify2array(results_list_converged_stats_mean), 1, 
                                quantile, probs = .025)
  pooled_pctl_bs_ci_ul <- apply(simplify2array(results_list_converged_stats_mean), 1, 
                                quantile, probs = .975)
  pooled_emp_sd <- apply(simplify2array(results_list_converged_stats_mean), 1, sd)
  
  pooled_avg_sd <- apply(simplify2array(results_list_converged_stats_sd), 1, mean)
  
  # Combine pooled results in data frame
  
  pooled_results <- data.frame(mean = pooled_mean,
                               pctl_bs_ci_ll = pooled_pctl_bs_ci_ll,
                               pctl_bs_ci_ul = pooled_pctl_bs_ci_ul,
                               emp_sd = pooled_emp_sd,
                               avg_sd = pooled_avg_sd)
  
  # Combine number of converged models and pooled results in list
  
  pooled <- list(number_converged = number_converged,
                 results = pooled_results)
  
  return(pooled)
}

# ---------------------------------------------------------------------------- #
# Define "run_analyses()" ----
# ---------------------------------------------------------------------------- #

# Define function for running analyses and pooling results

run_analysis <- function(inits_all, analysis_type, dat_all, analysis_sample, 
                         a_contrast, y_var, total_iterations) {
  # Select "inits" and "dat" based on "analysis_type" and "analysis_sample"
  
  inits <- inits_all[[analysis_type]]
  dat <- dat_all[[analysis_sample]]
  
  # Run analysis based on "a_contrast"
  
  if (a_contrast == "a1") {
    # TODO: Test with only 2 bootstrap samples for now
    
    dat <- dat[1:2]
    
    
    
    
    
    # Specify "jags_dat" and run JAGS model for each bootstrap sample
    
    results_list <- vector("list", length(dat))
    
    for (i in 1:length(dat)) {
      jags_dat <- specify_jags_dat(dat[[i]], a_contrast, y_var)
      results_list[[i]] <- run_jags_model(analysis_type, i, analysis_sample,
                                          jags_dat, inits,
                                          a_contrast, y_var, total_iterations)
    }
    
    names(results_list) <- 1:length(results_list)
    
    # TODO: Pool results across 500 bootstrap samples for models that converge. For
    # testing, temporarily pool across models that *didn't* converge.
    
    pooled <- pool_results(results_list)
    
    
    
    
    
    # Obtain path for saving results
    
    model_results_path_stem <- results_list[[1]]$model_results_path_stem
    
    # Create results object
    
    results <- list(per_bs_smp = results_list,
                    pooled = pooled)
  } else if (a_contrast %in% c("a2_1", "a2_2", "a2_3")) {
    # Specify "jags_dat" and run JAGS model
    
    jags_dat <- specify_jags_dat(dat, a_contrast, y_var)
    
    results <- run_jags_model(analysis_type, NULL, analysis_sample,
                              jags_dat, inits,
                              a_contrast, y_var, total_iterations)
    
    # Obtain path for saving results
    
    model_results_path_stem <- results$model_results_path_stem
  }
    
  # Save and return results
  
  save(results, file = paste0(model_results_path_stem, "/results.RData"))
  
  return(results)
}

# ---------------------------------------------------------------------------- #
# Run analyses ----
# ---------------------------------------------------------------------------- #

# TODO: Revise pooling to accommodate visual inspection of plots





# TODO: Test individual efficacy and dropout models for "a1". Test with only
# 2 bootstrap samples for now (see "run_analysis" above).

results_eff_a1_a <- run_analysis(inits_all, "efficacy", dat_all, "c1_corr_itt", 
                                 "a1", "bbsiq_neg_m", 10)
results_eff_a1_b <- run_analysis(inits_all, "efficacy", dat_all, "c1_corr_itt", 
                                 "a1", "bbsiq_neg_m", 20000)

results_drp_a1_a <- run_analysis(inits_all, "dropout", dat_all, "c1_corr_itt", 
                                 "a1", "miss_session_train_sum", 10)
results_drp_a1_b <- run_analysis(inits_all, "dropout", dat_all, "c1_corr_itt", 
                                 "a1", "miss_session_train_sum", 20000)





# TODO: Test for "a2" model not requiring pooling

results_eff_a2_1_a <- run_analysis(inits_all, "efficacy", dat_all, "c2_4_class_meas_compl", 
                                   "a2_1", "bbsiq_neg_m", 10)





# Iterate across all samples, contrasts, and outcomes

analysis_samples <- c("c1_corr_itt", "c1_corr_s5_train_compl",
                      "c2_4_class_meas_compl", "c2_4_s5_train_compl")

a_contrasts <- c("a1", "a2_1", "a2_2", "a2_3")

eff_y_vars <- c("rr_neg_threat_m", "rr_pos_threat_m",
                "bbsiq_neg_m", "bbsiq_ben_m", 
                "oa", "dass21_as_m")

total_iterations <- 10 # TODO: Update after testing





n_analysis_samples <- length(analysis_samples)
n_a_contrasts      <- length(a_contrasts)
n_eff_y_vars       <- length(eff_y_vars)

# TODO: Parallelize (a) "a1" contrast analyses in 500 bootstrap samples (see if
# statement on "a1" contrast in "run_analysis()" above) and (b) for loops below

  # Run "efficacy" models

for (i in 1:n_analysis_samples) {
  for (j in 1:n_a_contrasts) {
    for (k in 1:n_eff_y_vars) {
      run_analysis(inits_all, "efficacy", dat_all, analysis_samples[i], 
                   a_contrasts[j], eff_y_vars[k], total_iterations)
    }                                       # End for loop on "eff_y_vars"
  }                                         # End for loop on "a_contrasts"
}                                           # End for loop on "analysis_samples"

  # Run "dropout" models

for (i in 1:n_analysis_samples) {
  for (j in 1:n_a_contrasts) {
    run_analysis(inits_all, "dropout", dat_all, analysis_samples[i], 
                 a_contrasts[j], "miss_session_train_sum", total_iterations)
  }                                         # End for loop on "a_contrasts"
}                                           # End for loop on "analysis_samples"





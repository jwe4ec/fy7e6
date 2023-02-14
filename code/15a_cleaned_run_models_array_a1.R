# ---------------------------------------------------------------------------- #
# Run "a1" Contrast Models in Parallel on Parallel Partition
# Authors: Jeremy W. Eberle and Jackie Huband
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script runs "a1" contrast models that require multiple nodes due to a large
# number of bootstrap samples in parallel via a separate Slurm script that specifies 
# a job on Rivanna's parallel partition (multiple nodes with multiple cores/node).
# "a2" contrast models, which involve no bootstrapping and thus use only one node,
# are run on the standard partition (see separate script).



# Before running script, restart R and set working directory to "code" folder


# ---------------------------------------------------------------------------- #
# Obtain row of "parameter_table" to run
# ---------------------------------------------------------------------------- #

# Obtain "myNum", "boot_start", and "boot_stop" from command line (passed there by 
# Slurm script, which obtains them from "param_row", "c", and "bs_stop" of Bash script
# used to run Slurm script) to be used as index into "parameter_table" created below

cmdArgs <- commandArgs(trailingOnly = TRUE)
myNum <- as.integer(cmdArgs[1])
bs_sample_num <- as.integer(cmdArgs[2])


# ---------------------------------------------------------------------------- #
# If needed, create directory for output files
# ---------------------------------------------------------------------------- #

out_path <- paste0("./test_results_", myNum)

if (!dir.exists(out_path)) {
    dir.create(out_path)
}

# ---------------------------------------------------------------------------- #
# Define parallel analysis functions ----
# ---------------------------------------------------------------------------- #

# We cannot source custom functions defined in a separate script when using the 
# parallel partition; functions must be defined in the present script. Functions
# below are identical to those of "12a_define_functions_standard_partition.R" used
# for models run on the standard partition, with three exceptions:

# 1. Script 12a's "run_analysis()" function is no longer defined below; its
# contents, revised, appear outside any function in Section "Run Analyses" below.

# 2. A new function "broadcast_large_object()", which is needed only when 
# running models on the parallel partition, is defined below.

# 3. In "run_jags_model()", we no longer save posterior samples ("model_samples") 
# or plots of the MCMC object "model_res". We also no longer include the model 
# ("jags_model") or "model_res" in the returned list of results ("results").

# 4. In "create_parameter_table()", for "a1" analyses we use 2000 bootstrap samples
# instead of 6800 bootstrap samples.

# TODO (is this needed?): 5. "library(fastDummies)" and "library(rjags)" have 
# been added to the "dummy_code()" and "run_jags_model()" functions below.





# Note: Some of these functions refer to "a2" contrasts, but this script is not
# used for running "a2" models. Nevertheless, we retain the code relevant to "a2"
# models so that these functions remain nearly identical to those in Script 12a.

# ---------------------------------------------------------------------------- #
# 1. Define "load_pkgs_via_groundhog()" ----
# ---------------------------------------------------------------------------- #

# Define function to load packages via groundhog package. If packages have not
# been installed yet, groundhog will install them.

load_pkgs_via_groundhog <- function() {
  library(groundhog)
  meta.groundhog("2022-01-01")
  groundhog_day <- "2022-01-01"

  #parallel_pkgs <- c("iterators", "foreach", "doParallel")
  anlys_pkgs <- c("fastDummies", "rjags")

  groundhog.library(c(parallel_pkgs, anlys_pkgs), groundhog_day)

  cat("\ngroundhog_day =", groundhog_day, "\n\n")
}

# ---------------------------------------------------------------------------- #
# 2. Define "impute_mcar_nominal()" ----
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
# 3. Define "dummy_code()" ----
# ---------------------------------------------------------------------------- #

# Define function to dummy code certain nominal variables. Make the most frequent
# level the reference group.

dummy_code <- function(df, nominal_vars) {
  library(fastDummies)
  
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
# 4. Define "specify_jags_dat()" ----
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
# 5. Define "run_jags_model()" ----
# ---------------------------------------------------------------------------- #

# Define function to run JAGS model

run_jags_model <- function(analysis_type, bs_sample, analysis_sample,
                           jags_dat, inits,
                           a_contrast, y_var, total_iterations) {
  library(rjags)
  
  # Create JAGS model

  model_string_path <- paste0("../results/bayesian/", analysis_type,
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

  path1 <- paste0("../results/bayesian/", analysis_type, "/out/",
                  analysis_sample, "_", a_contrast, "_", y_var)
  path2 <- paste0("/burn_", burn_iterations,
                  "_total_", total_iterations)
  model_results_path_stem <- paste0(path1, path2)

  path3 <- switch(is.null(bs_sample) + 1, paste0("/per_bs_smp/", bs_sample), NULL)
  model_results_path_specific <- paste0(model_results_path_stem, path3)

  dir.create(model_results_path_specific, recursive = TRUE)

  # Create MCMC object

  model_res <- as.mcmc(do.call(rbind, model_samples))

  # Save results and diagnostics

  sink(file = paste0(model_results_path_specific, "/results_and_dx",
                     switch(is.null(bs_sample) + 1, paste0("_", bs_sample), NULL),
                     ".txt"))

  #print(paste0("Analysis Type: ", analysis_type))
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
                  model_results_path_specific = model_results_path_specific,
                  total_iterations = total_iterations,
                  burn_iterations = burn_iterations,
                  remaining_iterations = remaining_iterations,
                  create_model_time = create_model_time,
                  burn_in_time = burn_in_time,
                  sampling_time = sampling_time,
                  total_time = total_time,
                  summary = summary,
                  hpd_interval = hpd_interval,
                  geweke = geweke,
                  geweke_converge_each = geweke_converge_each,
                  geweke_converge_all = geweke_converge_all)

  return(results)
}

# ---------------------------------------------------------------------------- #
# 6. Define "create_parameter_table()" ----
# ---------------------------------------------------------------------------- #

# Define function to create table for nested combinations of model input parameters

create_parameter_table <- function() {
  # Specify input elements

  c1_a_contrasts   <- "a1"
  c2_4_a_contrasts <- c("a2_1", "a2_2", "a2_3")

  c1_eff_analysis_samples   <- c("c1_corr_itt_2000", "c1_corr_s5_train_compl_2000")
  c1_drp_analysis_samples   <- "c1_corr_itt_2000"

  c2_4_eff_analysis_samples <- c("c2_4_class_meas_compl", "c2_4_s5_train_compl")
  c2_4_drp_analysis_samples <- "c2_4_class_meas_compl"

  eff_y_vars <- c("rr_neg_threat_m", "rr_pos_threat_m",
                  "bbsiq_neg_m", "bbsiq_ben_m",
                  "oa_m", "dass21_as_m")
  drp_y_vars <- "miss_session_train_sum"

  # Compute lengths of input elements

  n_c1_a_contrasts        <- length(c1_a_contrasts)
  n_c2_4_a_contrasts      <- length(c2_4_a_contrasts)

  n_c1_eff_analysis_samples   <- length(c1_eff_analysis_samples)
  n_c1_drp_analysis_samples   <- length(c1_drp_analysis_samples)

  n_c2_4_eff_analysis_samples <- length(c2_4_eff_analysis_samples)
  n_c2_4_drp_analysis_samples <- length(c2_4_drp_analysis_samples)

  n_eff_y_vars       <- length(eff_y_vars)
  n_drp_y_vars       <- length(drp_y_vars)

  n_eff_contrasts_across_analysis_samples <- n_c1_a_contrasts*n_c1_eff_analysis_samples +
                                             n_c2_4_a_contrasts*n_c2_4_eff_analysis_samples
  n_drp_contrasts_across_analysis_samples <- n_c1_a_contrasts*n_c1_drp_analysis_samples +
                                             n_c2_4_a_contrasts*n_c2_4_drp_analysis_samples

  n_eff_combos <- (n_eff_contrasts_across_analysis_samples)*n_eff_y_vars
  n_drp_combos <- (n_drp_contrasts_across_analysis_samples)*n_drp_y_vars

  # Specify analysis samples nested within contrasts

  eff_contrasts_across_analysis_samples <-
    data.frame(a_contrast      = c(rep(c1_a_contrasts,
                                       each = n_c1_eff_analysis_samples),
                                   rep(c2_4_a_contrasts,
                                       each = n_c2_4_eff_analysis_samples)),
               analysis_sample = c(rep(c1_eff_analysis_samples,
                                       times = n_c1_a_contrasts),
                                   rep(c2_4_eff_analysis_samples,
                                       times = n_c2_4_a_contrasts)))

  drp_contrasts_across_analysis_samples <-
    data.frame(a_contrast      = c(rep(c1_a_contrasts,
                                       each = n_c1_drp_analysis_samples),
                                   rep(c2_4_a_contrasts,
                                       each = n_c2_4_drp_analysis_samples)),
               analysis_sample = c(rep(c1_drp_analysis_samples,
                                       times = n_c1_a_contrasts),
                                   rep(c2_4_drp_analysis_samples,
                                       times = n_c2_4_a_contrasts)))

  # Expand to specify outcomes nested within efficacy and dropout models

  eff_combos <- data.frame(analysis_type = "efficacy",
                           data.frame(lapply(eff_contrasts_across_analysis_samples, rep, n_eff_y_vars)),
                           y_var         = rep(eff_y_vars,
                                               each = n_eff_contrasts_across_analysis_samples))
  eff_combos <- eff_combos[order(eff_combos$a_contrast, eff_combos$analysis_sample), ]

  drp_combos <- data.frame(analysis_type = "dropout",
                           data.frame(lapply(drp_contrasts_across_analysis_samples, rep, n_drp_y_vars)),
                           y_var         = rep(drp_y_vars,
                                               each = n_drp_contrasts_across_analysis_samples))
  drp_combos <- drp_combos[order(drp_combos$a_contrast, drp_combos$analysis_sample), ]

  all_combos <- rbind(eff_combos, drp_combos)
  row.names(all_combos) <- 1:nrow(all_combos)

  return(all_combos)
}


# ---------------------------------------------------------------------------- #
# Read the data, using filenames for the giving bootstrap number
# ---------------------------------------------------------------------------- #

#(Note:
# Only data based on 2000 bootstrap samples for "a1" models are loaded; we no longer 
# load data based on 500 bootstrap samples for "a1" models or data for "a2" models.)

filename <- paste0("../data/final_clean/wd_c1_corr_itt_2000.RData_",bs_sample_num)
load(filename)

filename <- paste0("../data/final_clean/wd_c1_corr_s5_train_compl_2000.RData_", bs_sample_num)
load(filename)
  
filename <- paste0("../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData_", bs_sample_num)
load(filename)

filename <- paste0("../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData_", bs_sample_num)
load(filename)
  
# Store initial values in list

inits_all <- list(efficacy = inits_efficacy,
                    dropout  = inits_dropout)


# Store data in list
dat_all <- list(c1_corr_itt_2000            = wd_c1_corr_itt_2000,
                c1_corr_s5_train_compl_2000 = wd_c1_corr_s5_train_compl_2000)

# ---------------------------------------------------------------------------- #
# Run "a1" contrast analyses ----
# ---------------------------------------------------------------------------- #

# Create parameter table and select values for given row based on "myNum"

parameter_table <- create_parameter_table()

analysis_type <- parameter_table$analysis_type[myNum]
a_contrast <- parameter_table$a_contrast[myNum]
analysis_sample <- parameter_table$analysis_sample[myNum]
y_var <- parameter_table$y_var[myNum]

dat <- dat_all[[analysis_sample]]
inits <- inits_all[[analysis_type]]

# Set total number of MCMC iterations

if (a_contrast == "a1") {
  total_iterations <- 20000
} else if (a_contrast %in% c("a2_1", "a2_2", "a2_3")) {
  total_iterations <- 1000000
}

# Run analysis based on "a_contrast". Each worker must analyze multiple bootstrap
# samples because we need to analyze all bootstrap samples with <= 1000 cores (due
# to the limit on the number of cores per job on Rivanna's parallel partition)

if (a_contrast == "a1") {
  
  # Specify "jags_dat" and run JAGS model for each bootstrap sample in parallel

  jags_dat <- specify_jags_dat(dat[[bs_sample_num]], a_contrast, y_var)

  results_list <- list(num=run_jags_model(analysis_type, bs_sample = bs_sample_num, analysis_sample,
                     jags_dat, inits,
                     a_contrast, y_var, total_iterations) )
  

  names(results_list) <- bs_sample_num

  # ************************************************************************** #
  # TODO: Revise this section as needed to save results outside "/code" folder
  
  jmh <- FALSE
  if (jmh) {
  # Obtain path for saving results

  model_results_path_stem <- results_list[[1]]$model_results_path_stem

  # Create results object

  results <- list(per_bs_smp = results_list)
  } # End jmh
  
  # ************************************************************************** #
  
  # Save results list from a given array task. The names of the list elements are the 
  # bootstrap samples analyzed by the worker. The file name distinguishes results 
  # lists generated by the workers for different sets of bootstrap samples. These
  # results lists are concatenated in a separate script.
  
  outfile <- paste0(out_path, "/results_", bs_sample_num, ".RData")
  save(results_list, file = outfile)

} else if (a_contrast %in% c("a2_1", "a2_2", "a2_3")) {
  
  # TODO: Is it OK to have an error message here? Or is there a better approach?
  
  stop('This script is only for running "a1" contrast models that require multiple
       nodes on the parallel partition due to a large number of bootstrap samples.
       "a2" contrast models, which involve no bootstrapping, cannot be run on the
       parallel partition because they require only one node; rather, they are
       run on the standard partition (see separate script).')
}

cat("\nAll Done\n")

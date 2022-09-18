# ---------------------------------------------------------------------------- #
# Specify Efficacy Model and Initial Values
# Authors: Katharine E. Daniel and Jeremy W. Eberle
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

# No packages loaded

# ---------------------------------------------------------------------------- #
# Specify model ----
# ---------------------------------------------------------------------------- #

# Specify JAGS model with likelihood, priors, and parameters of interest

model_string = "model {
  # Specify likelihood for piecewise linear multilevel model and selection model
  
  for (i in 1:N) {
    # Grand-mean-center continuous covariates and auxiliary variables, rescaling
    # income to avoid Geweke's convergence diagnostic values of Inf
    
    income_ctr[i] <- (income[i] - mean(income))/10000
    age_ctr[i] <- age[i] - mean(age)
    
    confident_online_ctr[i] <- confident_online[i] - mean(confident_online)
    important_ctr[i] <- important[i] - mean(important)
    
    # Level 2 equations of multilevel model

    RE[i, 1:3] ~ dmnorm(mu_RE[i, 1:3], inv_cov[1:3, 1:3])

      # Mean random intercept B0i

    mu_RE[i, 1] <- beta[1] + beta[2]*a[i] + beta[3]*income_ctr[i] + beta[4]*age_ctr[i] +
                   beta[5]*a[i]*income_ctr[i] + beta[6]*a[i]*age_ctr[i] +
                   beta[7]*income_ctr[i]*age_ctr[i] +
                   beta[8]*a[i]*income_ctr[i]*age_ctr[i]
                   
      # Mean random slope B1i (baseline to Session 5)

    mu_RE[i, 2] <- beta[9] + beta[10]*a[i] + beta[11]*income_ctr[i] + beta[12]*age_ctr[i] +
                   beta[13]*a[i]*income_ctr[i] + beta[14]*a[i]*age_ctr[i] +
                   beta[15]*income_ctr[i]*age_ctr[i] +
                   beta[16]*a[i]*income_ctr[i]*age_ctr[i]

      # Mean random slope B2i (Session 5 to follow-up)

    mu_RE[i, 3] <- beta[17] + beta[18]*a[i] + beta[19]*income_ctr[i] + beta[20]*age_ctr[i] +
                   beta[21]*a[i]*income_ctr[i] + beta[22]*a[i]*age_ctr[i] +
                   beta[23]*income_ctr[i]*age_ctr[i] +
                   beta[24]*a[i]*income_ctr[i]*age_ctr[i]
    
    for (j in 1:J) {
      # Level 1 equation of multilevel model
      
      y[i, j] ~ dnorm(mu_Y[i, j], inv_sig_e2)

        # Expected outcome for each participant at each time point

      mu_Y[i, j] <- RE[i, 1] + RE[i, 2]*t1[j] + RE[i, 3]*t2[j]
      
      # Selection model to handle missing data in y[i, j]. Assume MAR conditioned 
      # on auxiliary variables below.

      r[i, j] ~ dbern(prob_r[i])
    }
    
    # Selection model (continued)

    logit(prob_r[i]) <- gamma[1] +
                        gamma[2]*gndr_Male[i] + gamma[3]*gndr_Trans_or_Other[i] +
                        gamma[4]*dvce_multiple_types[i]

    # Other missing data handling
    
      # Note: Auxiliary variables important and device are completely observed
      
      # Assume MCAR for employment status, marital status, and gender auxiliary 
      # variables given few missing values. Because these are nominal, singly 
      # impute data for them separately by randomly assigning levels to missing 
      # values based on the distribution of observed levels.
      
      # Assume MCAR for age covariate and confident_online auxiliary variable given 
      # few missing values. Because these are continuous, assign distributions below
      # based on observed means and precisions computed separately.
      
    age[i] ~ dnorm(mu_age, inv_sig_age)
    confident_online[i] ~ dnorm(mu_confident_online, inv_sig_confident_online)
    
      # Assume MAR for income covariate conditioned on auxiliary variables below

    income[i] ~ dnorm(mu_income[i], inv_sig_income)

    mu_income[i] <- gamma[5] +
                    gamma[6]*empl_Student[i] +
                    gamma[7]*empl_Other_or_Unknown[i] +
                    gamma[8]*mrtl_Single_or_Dating[i] +
                    gamma[9]*mrtl_Sep_Div_or_Wid[i] +
                    gamma[10]*mrtl_Other[i] +
                    gamma[11]*confident_online_ctr[i] +
                    gamma[12]*important_ctr[i]
  }

  # Specify priors for model parameters (all uninformative)

  for (i in 1:24) {               # All betas have same normal distribution
    beta[i] ~ dnorm(0, 1.0E-6)
  }
  
  for (i in 1:12) {               # All gammas have same normal distribution
    gamma[i] ~ dnorm(0, 1.0E-6)
  }
  
  inv_sig_income ~ dgamma(.001, .001)       # Precision for income covariate

  inv_cov[1:3, 1:3] ~ dwish(R[1:3, 1:3], 3) # Precision matrix for random effects
  R[1, 1] <- 1
  R[2, 2] <- 1
  R[3, 3] <- 1
  R[2, 1] <- 0
  R[1, 2] <- R[2, 1]
  R[3, 1] <- 0
  R[1, 3] <- R[3, 1]
  R[3, 2] <- 0
  R[2, 3] <- R[3, 2]

  cov[1:3, 1:3] <- inverse(inv_cov[1:3, 1:3]) # Var-cov matrix for random effects

  sig_RI  <- cov[1, 1]    # Variance for random intercept B0i
  sig_RS1 <- cov[2, 2]    # Variance for random slope B1i
  sig_RS2 <- cov[3, 3]    # Variance for random slope B2i

  cov_1_2 <- cov[1, 2]    # Covariance of random intercept and random slope B1i
  cov_1_3 <- cov[1, 3]    # Covariance of random intercept and random slope B2i
  cov_2_3 <- cov[2, 3]    # Covariance of random slope B1i and random slope B2i

  rho_1_2 <- cov[1, 2]/sqrt(cov[1, 1]*cov[2, 2])  # Correlation of above
  rho_1_3 <- cov[1, 3]/sqrt(cov[1, 1]*cov[3, 3])  # Correlation of above
  rho_2_3 <- cov[2, 3]/sqrt(cov[2, 2]*cov[3, 3])  # Correlation of above
  
  inv_sig_e2 ~ dgamma(.001, .001)             # Precision for Level 1 residual
  sig_e2 <- 1/inv_sig_e2  # Level 1 residual (measurement error) variance
  
  # Store parameters of interest into list called para so that we can get HPD
  # credible intervals for them all
  
  para[1] <- 2*beta[2] + 10*beta[10]               # Contrast difference in outcome at Session 5
  para[2] <- 2*beta[2] + 10*beta[10] + 2*beta[18]  # Contrast difference in outcome at follow-up

  para[3] <- 2*beta[10]               # Contrast difference in t1 slope (baseline to Session 5)
  para[4] <- 2*beta[18]               # Contrast difference in t2 slope (Session 5 to follow-up)
  
  para[5] <- sig_RI
  para[6] <- sig_RS1
  para[7] <- sig_RS2

  para[8]  <- cov_1_2
  para[9]  <- cov_1_3
  para[10] <- cov_2_3

  para[11] <- rho_1_2
  para[12] <- rho_1_3
  para[13] <- rho_2_3
  
  para[14] <- sig_e2
}
"

# ---------------------------------------------------------------------------- #
# Specify initial values ----
# ---------------------------------------------------------------------------- #

inits_efficacy <- list(beta = rep(0, 24), gamma = rep(0, 12), inv_sig_e2 = 1, inv_cov = diag(3),
                       .RNG.name = "base::Wichmann-Hill", .RNG.seed = 1234)

# ---------------------------------------------------------------------------- #
# Save model and initial values ----
# ---------------------------------------------------------------------------- #

path <- "./results/bayesian/efficacy/model_and_initial_values/"
dir.create(path, recursive = TRUE)

writeLines(model_string, con = paste0(path, "model_string.txt"))
save(inits_efficacy, file = paste0(path, "inits_efficacy.RData"))
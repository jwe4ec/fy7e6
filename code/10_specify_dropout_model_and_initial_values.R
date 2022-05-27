# ---------------------------------------------------------------------------- #
# Specify Treatment Dropout Model and Initial Values
# Authors: Jeremy W. Eberle and Katharine E. Daniel
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
  # Specify likelihood for linear regression
  
  for (i in 1:N) {
    # Grand-mean-center continuous covariates and auxiliary variables
    
    income_ctr[i] <- income[i] - mean(income)
    age_ctr[i] <- age[i] - mean(age)
    
    confident_online_ctr[i] <- confident_online[i] - mean(confident_online)
    important_ctr[i] <- important[i] - mean(important)
    
    # TODO: Use Poisson or Tobit regression instead of linear regression given
    # count data? Seems Tobit is appropriate given that some participants have 
    # 0 missing training sessions.
    
    
    
    
    
    y[i] ~ dnorm(mu_Y[i], inv_sig_e2)

      # Expected outcome for each participant

    mu_Y[i] <- beta[1] + beta[2]*a[i] + beta[3]*income_ctr[i] + beta[4]*age_ctr[i] +
               beta[5]*a[i]*income_ctr[i] + beta[6]*a[i]*age_ctr[i] +
               beta[7]*income_ctr[i]*age_ctr[i] +
               beta[8]*a[i]*income_ctr[i]*age_ctr[i]
    
    # Missing data handling
    
      # Note: Outcome y and auxiliary variable important are completely observed
      
      # Assume MCAR for employment status and marital status auxiliary variables 
      # given few missing values. Because these are nominal, singly impute data 
      # for them separately by randomly assigning levels to missing values based 
      # on the distribution of observed levels.
      
      # Assume MCAR for age covariate and confident_online auxiliary variable given 
      # few missing values. Because these are continuous, assign distributions below
      # based on observed means and precisions computed separately.
      
    age[i] ~ dnorm(mu_age, inv_sig_age)
    confident_online[i] ~ dnorm(mu_confident_online, inv_sig_confident_online)
    
      # Assume MAR for income covariate conditioned on auxiliary variables below

    income[i] ~ dnorm(mu_income[i], inv_sig_income)

    mu_income[i] <- gamma[1] +
                    gamma[2]*empl_Student[i] +
                    gamma[3]*empl_Other_or_Unknown[i] +
                    gamma[4]*mrtl_Single_or_Dating[i] +
                    gamma[5]*mrtl_Sep_Div_or_Wid[i] +
                    gamma[6]*mrtl_Other[i] +
                    gamma[7]*confident_online_ctr[i] +
                    gamma[8]*important_ctr[i]
  }

  # Specify priors for model parameters (all uninformative)

  for (i in 1:8) {               # All betas have same normal distribution
    beta[i] ~ dnorm(0, 1.0E-6)
  }
  
  for (i in 1:8) {               # All gammas have same normal distribution
    gamma[i] ~ dnorm(0, 1.0E-6)
  }
  
  inv_sig_income ~ dgamma(.001, .001)  # Precision for income covariate

  inv_sig_e2 ~ dgamma(.001, .001)      # Precision for residual
  sig_e2 <- 1/inv_sig_e2               # Residual (measurement error) variance
  
  # Store parameters of interest into list called para so that we can get HPD
  # credible intervals for them all
  
  para[1] <- 2*beta[2]                 # Contrast difference in outcome

  para[2] <- sig_e2
}
"

# ---------------------------------------------------------------------------- #
# Specify initial values ----
# ---------------------------------------------------------------------------- #

inits_dropout <- list(beta = rep(0, 8), gamma = rep(0, 8), inv_sig_e2 = 1,
                      .RNG.name = "base::Wichmann-Hill", .RNG.seed = 1234)

# ---------------------------------------------------------------------------- #
# Save model and initial values ----
# ---------------------------------------------------------------------------- #

path <- "./results/bayesian/dropout/model_and_initial_values/"
dir.create(path, recursive = TRUE)

writeLines(model_string, con = paste0(path, "model_string.txt"))
save(inits_dropout, file = paste0(path, "inits_dropout.RData"))
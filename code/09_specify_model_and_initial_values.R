# ---------------------------------------------------------------------------- #
# Specify Model and Initial Values
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

# TODO: Add auxiliary variables (see code from Class 24 as example)





# Specify JAGS model with likelihood, priors, and parameters of interest

model_string = "model {
  # Specify likelihood for piecewise linear multilevel model

  for (i in 1:N) {

    # Random effects

    RE[i, 1:3] ~ dmnorm(mu_RE[i, 1:3], Inv_cov[1:3, 1:3])

    # Mean random intercept B0i

    mu_RE[i, 1] <- beta[1] + beta[2]*income[i] + beta[3]*age[i]
                   + beta[4]*income[i]*age[i]

    # Mean random slope B1i (baseline to Session 5)

    mu_RE[i, 2] <- beta[5] + beta[6]*a[i] + beta[7]*income[i] + beta[8]*age[i]
                   + beta[9]*a[i]*income[i] + beta[10]*a[i]*age[i]
                   + beta[11]*income[i]*age[i]
                   + beta[12]*a[i]*income[i]*age[i]

    # Mean random slope B2i (Session 5 to follow-up)

    mu_RE[i, 3] <- beta[13] + beta[14]*a[i] + beta[15]*income[i] + beta[16]*age[i] 
                   + beta[17]*a[i]*income[i] + beta[18]*a[i]*age[i]
                   + beta[19]*income[i]*age[i] 
                   + beta[20]*a[i]*income[i]*age[i]

    # Because we have missing data on the age and income predictors

    age[i] ~ dnorm(mu_age, Inv_Sig_age)
    income[i] ~ dnorm(mu_income, Inv_Sig_income)

    for (j in 1:J) {
      y[i, j] ~ dnorm(mu_Y[i, j], Inv_Sig_e2)

      # Expected outcome for each participant at each time point

      mu_Y[i, j] <- RE[i, 1] + RE[i, 2]*t1[j] + RE[i, 3]*t2[j]
    }
  }

  # Specify priors for model parameters (all uninformative)

  for (i in 1:20) {               # All 20 betas have same normal distribution
    beta[i] ~ dnorm(0, 1.0E-6)
  }

  mu_age ~ dnorm(0, .0000001)
  Inv_Sig_age ~ dgamma(.001, .001)

  mu_income ~ dnorm(0, .0000001)
  Inv_Sig_income ~ dgamma(.001, .001)

  Inv_cov[1:3, 1:3] ~ dwish(R[1:3, 1:3], 3) # Precision matrix for random effects
  R[1, 1] <- 1
  R[2, 2] <- 1
  R[3, 3] <- 1
  R[1, 2] <- R[2, 1]
  R[2, 1] <- 0
  R[1, 3] <- R[3, 1]
  R[3, 1] <- 0
  R[2, 3] <- R[3, 2]
  R[3, 2] <- 0

  Cov[1:3, 1:3] <- inverse(Inv_cov[1:3, 1:3]) # Var-cov matrix of random effects

  Inv_Sig_e2 ~ dgamma(.001, .001)             # Precision for level 1 residual
  Sig_e2 <- 1/Inv_Sig_e2  # Level 1 residual (measurement error) variance

  Sig_RI  <- Cov[1, 1]    # Variance for random intercept B0i
  Sig_RS1 <- Cov[2, 2]    # Variance for random slope B1i
  Sig_RS2 <- Cov[3, 3]    # Variance for random slope B2i

  Cov_1_2 <- Cov[1, 2]    # Covariance of random intercept and random slope B1i
  Cov_1_3 <- Cov[1, 3]    # Covariance of random intercept and random slope B2i
  Cov_2_3 <- Cov[2, 3]    # Covariance of random slope B1i and random slope B2i

  rho_1_2 <- Cov[1, 2]/sqrt(Cov[1, 1]*Cov[2, 2])  # Correlation of above
  rho_1_3 <- Cov[1, 3]/sqrt(Cov[1, 1]*Cov[3, 3])  # Correlation of above
  rho_2_3 <- Cov[2, 3]/sqrt(Cov[2, 2]*Cov[3, 3])  # Correlation of above
  
  # Store parameters of interest into list called para so that we can get HPD
  # credible intervals for them all

  para[1] <- 10*beta[6]               # Condition difference in outcome at Session 5
  para[2] <- 10*beta[6] + 2*beta[14]  # Condition difference in outcome at follow-up

  para[3] <- 2*beta[6]                # Condition difference in t1 slope (baseline to Session 5)
  para[4] <- 2*beta[14]               # Condition difference in t2 slope (Session 5 to follow-up)

  # TODO: ADD AUC PARAMETER
  




  para[5] <- Sig_e2  
  para[6] <- Sig_RI
  para[7] <- Sig_RS1
  para[8] <- Sig_RS2

  para[9]  <- Cov_1_2
  para[10] <- Cov_1_3
  para[11] <- Cov_2_3

  para[12] <- rho_1_2
  para[13] <- rho_1_3
  para[14] <- rho_2_3
}
"

# ---------------------------------------------------------------------------- #
# Specify initial values ----
# ---------------------------------------------------------------------------- #

inits <- list(beta = rep(0, 20), Inv_Sig_e2 = 1,
              Inv_cov = diag(3),
              .RNG.name = "base::Wichmann-Hill", .RNG.seed = 11)

# ---------------------------------------------------------------------------- #
# Save model and initial values ----
# ---------------------------------------------------------------------------- #

path <- "./results/bayesian/model_and_initial_values/"
dir.create(path, recursive = TRUE)

writeLines(model_string, con = paste0(path, "model_string.txt"))
save(inits, file = paste0(path, "inits.RData"))
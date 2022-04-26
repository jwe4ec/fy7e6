# ---------------------------------------------------------------------------- #
# RJAGS Models for Negative BBSIQ and OASIS: CBM vs. Psychoeducation v3.1
# Authors: Katharine E. Daniel and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Changes to script reflect changes to analysis plan where a2 is removed
# for CBM-I and psychoeducation comparisons
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

# Load packages

groundhog.library(rjags, groundhog_day)

# ---------------------------------------------------------------------------- #
# TODO: Add auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Add any auxiliary variables to this code (see code from Class 24 as example)





# ---------------------------------------------------------------------------- #
# Negative BBSIQ for session 1 assessment completers ----
# ---------------------------------------------------------------------------- #

# Load data ----

bbsiq_dat <- read.csv("./data/temp/bbsiq_session_1_ax_completers_v2.0.csv") #KD: we'll change this to reflect updated data

# Extract data in wide format for RJAGS ----

# This outcome is measured at 4 time points, so make matrix with 4 columns.

neg_bias <- matrix(NA, nrow = length(unique(bbsiq_dat$participant_id)),
                   ncol = 4) # 4 time points

for (i in 1:length(unique(bbsiq_dat$participant_id))) {
  neg_bias[i, 1] <-
    bbsiq_dat$neg_bias[which(bbsiq_dat$participant_id ==
                               unique(bbsiq_dat$participant_id)[i])][1]
  neg_bias[i, 2] <-
    bbsiq_dat$neg_bias[which(bbsiq_dat$participant_id ==
                               unique(bbsiq_dat$participant_id)[i])][2]
  neg_bias[i, 3] <-
    bbsiq_dat$neg_bias[which(bbsiq_dat$participant_id ==
                               unique(bbsiq_dat$participant_id)[i])][3]
  neg_bias[i, 4] <-
    bbsiq_dat$neg_bias[which(bbsiq_dat$participant_id ==
                               unique(bbsiq_dat$participant_id)[i])][4]
}


# Because these values are same for all subjects over time, just create vector.

S1 <- c(0, 3, 5, 5) 
S2 <- c(0, 0, 0, 1)


# Because these values do not change over time within a participant, just take
# the first value for each participant

SES <- rep(NA, length(unique(bbsiq_dat$participant_id)))
Age <- rep(NA, length(unique(bbsiq_dat$participant_id)))
a1 <- rep(NA, length(unique(bbsiq_dat$participant_id))) 

for (i in 1:length(unique(bbsiq_dat$participant_id))) {
  SES[i] <-
    bbsiq_dat$income_dollar_centered[which(bbsiq_dat$participant_id ==
                                             unique(bbsiq_dat$participant_id)[i])][1]
  Age[i] <-
    bbsiq_dat$age_centered[which(bbsiq_dat$participant_id ==
                                   unique(bbsiq_dat$participant_id)[i])][1]
  a1[i] <-
    bbsiq_dat$a1[which(bbsiq_dat$participant_id ==
                         unique(bbsiq_dat$participant_id)[i])][1]
}

# Specify JAGS model with likelihood, priors, and parameters of interest ----

modelString_bbsiq_neg_a1 = "model {

  # Specify likelihood for piecewise linear multilevel model

  for (i in 1:N) {

    # Random effects

    LS[i, 1:3] ~ dmnorm(muLS[i, 1:3], Inv_cov[1:3, 1:3])

    # Mean random intercept B0i

    muLS[i, 1] <- beta[1] + beta[2]*income[i] + beta[3]*Age[i]
                  + beta[4]*income[i]*Age[i]

    # Mean random slope B1i (pre-intervention to session 5)

    muLS[i, 2] <- beta[5] + beta[6]*a1[i] + beta[7]*income[i] + beta[8]*Age[i]
                  + beta[9]*a1[i]*income[i] + beta[10]*a1[i]*Age[i]
                  + beta[11]*income[i]*Age[i]
                  + beta[12]*a1[i]*income[i]*Age[i]

    # Mean random slope B2i (session 5 to follow up)

    muLS[i, 3] <- beta[13] + beta[14]*a1[i] + beta[15]*income[i]
                  + beta[16]*Age[i] + beta[17]*a1[i]*income[i] 
                  + beta[18]*a1[i]*Age[i] + beta[19]*income[i]*Age[i] 
                  + beta[20]*a1[i]*income[i]*Age[i]

    # Because we have missing data on the Age and income predictors

    Age[i] ~ dnorm(muAge, Inv_Sig_Age)
    income[i] ~ dnorm(muincome, Inv_Sig_income)

    for (j in 1:J) {
      y[i, j] ~ dnorm(muY[i, j], Inv_Sig_e2)

      # Expected outcome for each participant at each time point

      muY[i, j] <- LS[i, 1] + LS[i, 2]*S1[j] + LS[i, 3]*S2[j]
    }
  }

  # Specify priors for model parameters (all uninformative)

  for (i in 1:20) {               # All 20 betas have same normal distribution
    beta[i] ~ dnorm(0, 1.0E-6)
  }

  muAge ~ dnorm(0, .0000001)
  Inv_Sig_Age ~ dgamma(.001, .001)

  muSES ~ dnorm(0, .0000001)
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

  Sig_L1 <- Cov[1, 1]     # Variance for random intercept B0i
  Sig_S1 <- Cov[2, 2]     # Variance for random slope B1i
  Sig_S2 <- Cov[3, 3]     # Variance for random slope B2i

  Cov_1_2 <- Cov[1, 2]    # Covariance of random intercept and random slope B1i
  Cov_1_3 <- Cov[1, 3]    # Covariance of random intercept and random slope B2i
  Cov_2_3 <- Cov[2, 3]    # Covariance of random slope B1i and random slope B2i

  rho_1_2 <- Cov[1, 2]/sqrt(Cov[1, 1]*Cov[2, 2])  # Correlation of above
  rho_1_3 <- Cov[1, 3]/sqrt(Cov[1, 1]*Cov[3, 3])  # Correlation of above
  rho_2_3 <- Cov[2, 3]/sqrt(Cov[2, 2]*Cov[3, 3])  # Correlation of above
  
  # Store parameters of interest into list called para so that we can get HPD
  # credible intervals for them all

  para[1] <- 2*beta[6]*S1[j]                      # CBM vs psychoed time-specific outcome at session 5
  para[2] <- 2(beta[6]*S1[j] + beta[14]*S2[j])    # CBM vs psychoed time-specific outcome at follow-up

  para[3] <- 2*beta[6]                            # CBM vs. psychoed slope at S1 (pre-intervention to session 5)
  para[4] <- 2*beta[14]                           # CBM vs. psychoed slope at S2 (session 5 to follow-up)

  #ADD IN AUC PARAMETER

  para[6] <- Sig_e2  
  para[7] <- Sig_L1
  para[8] <- Sig_S2
  para[9] <- Sig_S3

  para[10] <- Cov_1_2
  para[11] <- Cov_1_3
  para[12] <- Cov_2_3

  para[13] <- rho_1_2
  para[14] <- rho_1_3
  para[15] <- rho_2_3

}
"

# Save model to directory ----

outcome_name <- "neg_bias"                      # Specify outcome name
sample_name <- "session_1_ax_completers"        # Specify sample name
a1_name <- "a1"                                 # Specify a1 contrast name

dir.create("./results")
path1 <- paste0("./results/", outcome_name, "_", sample_name, "_", a1_name, "_")
dir.create(path1)

writeLines(modelString_bbsiq_neg_a1,
           con = paste0(path1, "/model_bbsiq_neg_a1.txt"))

# Specify initial values ----

inits <- list(beta = rep(0, 20), Inv_Sig_e2 = 1,
              Inv_cov = diag(3),
              .RNG.name = "base::Wichmann-Hill", .RNG.seed = 11)

# Specify data ----

jagsdata <- list(N = length(unique(bbsiq_dat$participant_id)),
                 J = 4,
                 SES = SES,
                 Age = Age,
                 a1 = a1,
                 S1 = S1, S2 = S2,
                 y = neg_bias)

# Run model in JAGS ----

model_bbsiq_neg_a1 <-
  jags.model(file = paste0(path1, "/model_bbsiq_neg_a1.txt"),
             data = jagsdata, inits = inits, n.chains = 1)

options(scipen = 999)                 # Prevent printing in scientific notation
total_iterations <- 10                # Specify total number of iterations
burn_iterations <- total_iterations/2 # Specify burn-in period (typically half)
remaining_iterations <- total_iterations - burn_iterations

time0 <- proc.time()
update(model_bbsiq_neg_a1, n.iter = burn_iterations)
(burn_in_time <- proc.time() - time0)

time0 <- proc.time()
model.samples_bbsiq_neg_a1 <-
  coda.samples(model_bbsiq_neg_a1,
               c("para"),
               n.iter = remaining_iterations)
(sampling_time <- proc.time() - time0)

# Save posterior samples to directory, reload them, and create MCMC object ----

path2 <- paste0("/burn_", burn_iterations,
                "_total_", total_iterations)
dir.create(paste0(path1, path2))

save(model.samples_bbsiq_neg_a1,
     file = paste0(path1, path2, "/model.samples_bbsiq_neg_a1.RData"))
load(paste0(path1, path2, "/model.samples_bbsiq_neg_a1.RData"))

model.res_bbsiq_neg_a1 <-
  as.mcmc(do.call(rbind, model.samples_bbsiq_neg_a1))

# Save plots to directory ----

pdf(file = paste0(path1, path2, "/plots.pdf"))
par(mfrow=c(4,2))
plot(model.res_bbsiq_neg_a1)
dev.off()

# Save results and diagnostics to directory ----

sink(file = paste0(path1, path2, "/results and diagnostics.txt"))
print(paste0("Outcome: ", outcome_name))
print(paste0("Sample: ", sample_name))
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
summary(model.res_bbsiq_neg_a1)

print("HPD Credible Intervals:")
cat("\n")
HPDinterval(model.res_bbsiq_neg_a1)
cat("\n")

print("Geweke's convergence diagnostic:")
(geweke <- geweke.diag(model.res_bbsiq_neg_a1))
print("Parameters in (-1.96, 1.96):")
cat("\n")
print(geweke$z > -1.96 & geweke$z < 1.96)
cat("\n")
print(paste0("All parameters in (-1.96, 1.96): ",
             all(geweke$z > -1.96 & geweke$z < 1.96)))
sink()




# ---------------------------------------------------------------------------- #
# OASIS for session 1 assessment completers ----
# ---------------------------------------------------------------------------- #


# Load data ----

oasis_dat <- read.csv("./data/temp/oa_session_1_ax_completers_v2.0.csv") #KD: we'll change this to reflect clean data file

# Extract data in wide format for RJAGS ----

# This outcome is measured at 7 time points, so make matrix with 7 columns.

anxiety_scale <- matrix(NA, nrow = length(unique(oasis_dat$participant_id)),
                        ncol = 7) # 7 time points

for (i in 1:length(unique(oasis_dat$participant_id))) {
  anxiety_scale[i, 1] <-
    oasis_dat$anxiety_scale[which(oasis_dat$participant_id ==
                                    unique(oasis_dat$participant_id)[i])][1]
  anxiety_scale[i, 2] <-
    oasis_dat$anxiety_scale[which(oasis_dat$participant_id ==
                                    unique(oasis_dat$participant_id)[i])][2]
  anxiety_scale[i, 3] <-
    oasis_dat$anxiety_scale[which(oasis_dat$participant_id ==
                                    unique(oasis_dat$participant_id)[i])][3]
  anxiety_scale[i, 4] <-
    oasis_dat$anxiety_scale[which(oasis_dat$participant_id ==
                                    unique(oasis_dat$participant_id)[i])][4]
  anxiety_scale[i, 5] <-
    oasis_dat$anxiety_scale[which(oasis_dat$participant_id ==
                                    unique(oasis_dat$participant_id)[i])][5]
  anxiety_scale[i, 6] <-
    oasis_dat$anxiety_scale[which(oasis_dat$participant_id ==
                                    unique(oasis_dat$participant_id)[i])][6]
  anxiety_scale[i, 7] <-
    oasis_dat$anxiety_scale[which(oasis_dat$participant_id ==
                                    unique(oasis_dat$participant_id)[i])][7]
}

# Because these values are same for all subjects over time, just create vector.

S1 <- c(0, 1, 2, 3, 4, 5, 5)
S2 <- c(0, 0, 0, 0, 0, 0, 1)  


# Because these values do not change over time within a participant, just take
# the first value for each participant

income <- rep(NA, length(unique(oasis_dat$participant_id)))
Age <- rep(NA, length(unique(oasis_dat$participant_id)))
a1 <- rep(NA, length(unique(oasis_dat$participant_id))) 

for (i in 1:length(unique(oasis_dat$participant_id))) {
  income[i] <-
    oasis_dat$income_dollar_centered[which(oasis_dat$participant_id ==
                                             unique(oasis_dat$participant_id)[i])][1]
  Age[i] <-
    oasis_dat$age_centered[which(oasis_dat$participant_id ==
                                   unique(oasis_dat$participant_id)[i])][1]
  a1[i] <-
    oasis_dat$a1[which(oasis_dat$participant_id ==
                         unique(oasis_dat$participant_id)[i])][1]
}

# Specify JAGS model with likelihood, priors, and parameters of interest ----

modelString_oasis_a1 = "model {

  # Specify likelihood for piecewise linear multilevel model

  for (i in 1:N) {

    # Random effects

    LS[i, 1:3] ~ dmnorm(muLS[i, 1:3], Inv_cov[1:3, 1:3])

    # Mean random intercept B0i

    muLS[i, 1] <- beta[1] + beta[2]*income[i] + beta[3]*Age[i]
                  + beta[4]*income[i]*Age[i]

    # Mean random slope B1i

    muLS[i, 2] <- beta[5] + beta[6]*a1[i] + beta[7]*income[i] + beta[8]*Age[i]
                  + beta[9]*a1[i]*income[i] + beta[10]*a1[i]*Age[i]
                  + beta[11]*income[i]*Age[i]
                  + beta[12]*a1[i]*income[i]*Age[i]

    # Mean random slope B2i

    muLS[i, 3] <- beta[13] + beta[14]*a1[i] + beta[15]*income[i]
                  + beta[16]*Age[i] + beta[17]*a1[i]*income[i] + beta[18]*a1[i]*Age[i]
                  + beta[19]*income[i]*Age[i] +  beta[20]*a1[i]*income[i]*Age[i]

    # Because we have missing data on the Age and income predictors

    Age[i] ~ dnorm(muAge, Inv_Sig_Age)
    income[i] ~ dnorm(muincome, Inv_Sig_income)

    for (j in 1:J) {
      y[i, j] ~ dnorm(muY[i, j], Inv_Sig_e2)

      # Expected outcome for each participant at each time point

      muY[i, j] <- LS[i, 1] + LS[i, 2]*S1[j] + LS[i, 3]*S2[j] 
    }
  }

  # Specify priors for model parameters (all uninformative)

  for (i in 1:20) {               # All 20 betas have same normal distribution
    beta[i] ~ dnorm(0, 1.0E-6)
  }

  muAge ~ dnorm(0, .0000001)
  Inv_Sig_Age ~ dgamma(.001, .001)

  muSES ~ dnorm(0, .0000001)
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

  Inv_Sig_e2 ~ dgamma(.001, .001)     # Precision for level 1 residual
  Sig_e2 <- 1/Inv_Sig_e2              # Level 1 residual (measurement error) variance

  Sig_L1 <- Cov[1, 1]     # Variance for random intercept B0i
  Sig_S2 <- Cov[2, 2]     # Variance for random slope B1i
  Sig_S3 <- Cov[3, 3]     # Variance for random slope B2i

  Cov_1_2 <- Cov[1, 2]    # Covariance of random intercept and random slope B1i
  Cov_1_3 <- Cov[1, 3]    # Covariance of random intercept and random slope B2i
  Cov_2_3 <- Cov[2, 3]    # Covariance of random slope B1i and random slope B2i
  
  rho_1_2 <- Cov[1, 2]/sqrt(Cov[1, 1]*Cov[2, 2])  # Correlation of above
  rho_1_3 <- Cov[1, 3]/sqrt(Cov[1, 1]*Cov[3, 3])  # Correlation of above
  rho_2_3 <- Cov[2, 3]/sqrt(Cov[2, 2]*Cov[3, 3])  # Correlation of above

  # Store parameters of interest into list called para so that we can get HPD
  # credible intervals for them all

  para[1] <- 2*beta[6]*S1[j]                        # CBM vs psychoed time-specific outcome at session 5
  para[2] <- 2(beta[6]*S1[j] + beta[14]*S2[j])      # CBM vs psychoed time-specific outcome at follow-up
    
  para[3] <- 2*beta[6]                              # CBM vs. psychoed slope at S1 (pre-intervention to session 5)
  para[4] <- 2*beta[14]                             # CBM vs. psychoed slope at S2 (session 5 to follow-up)

  #ADD IN AUC PARAMETER
  
  para[6] <- Sig_e2
  para[7] <- Sig_L1
  para[8] <- Sig_S2
  para[9] <- Sig_S3
  para[10] <- Cov_1_2
  para[11] <- Cov_1_3
  para[12] <- Cov_2_3
  para[13] <- rho_1_2
  para[14] <- rho_1_3
  para[15] <- rho_2_3

}
"

# Save model to directory ----

outcome_name <- "anxiety_scale"                 # Specify outcome name
sample_name <- "session_1_ax_completers"        # Specify sample name
a1_name <- "a1"                                 # Specify a1 contrast name

dir.create("./results")
path1 <- paste0("./results/", outcome_name, "_", sample_name, "_", a1_name)
dir.create(path1)

writeLines(modelString_oasis_a1,
           con = paste0(path1, "/modelString_oasis_a1.txt"))

# Specify initial values ----

inits <- list(beta = rep(0, 20), Inv_Sig_e2 = 1,
              Inv_cov = diag(3),
              .RNG.name = "base::Wichmann-Hill", .RNG.seed = 11)

# Specify data ----

jagsdata <- list(N = length(unique(oasis_dat$participant_id)),
                 J = 7,
                 income = income,
                 Age = Age,
                 a1 = a1,
                 S1 = S1, S2 = S2,
                 y = anxiety_scale)

# Run model in JAGS ----

model_oasis_a1 <-
  jags.model(file = paste0(path1, "/modelString_oasis_a1.txt"),
             data = jagsdata, inits = inits, n.chains = 1)

options(scipen = 999)                 # Prevent printing in scientific notation
total_iterations <- 10                # Specify total number of iterations
burn_iterations <- total_iterations/2 # Specify burn-in period (typically half)
remaining_iterations <- total_iterations - burn_iterations

time0 <- proc.time()
update(model_oasis_a1, n.iter = burn_iterations)
(burn_in_time <- proc.time() - time0)

time0 <- proc.time()
model.samples_oasis_a1 <-
  coda.samples(model_oasis_a1,
               c("para"),
               n.iter = remaining_iterations)
(sampling_time <- proc.time() - time0)

# Save posterior samples to directory, reload them, and create MCMC object ----

path2 <- paste0("/burn_", burn_iterations,
                "_total_", total_iterations)
dir.create(paste0(path1, path2))

save(model.samples_oasis_a1,
     file = paste0(path1, path2, "/model.samples_oasis_a1.RData"))
load(paste0(path1, path2, "/model.samples_oasis_a1.RData"))

model.res_oasis_a1 <-
  as.mcmc(do.call(rbind, model.samples_oasis_a1))

# Save plots to directory ----

pdf(file = paste0(path1, path2, "/plots.pdf"))
par(mfrow=c(4,2))
plot(model.res_oasis_a1_1)
dev.off()

# Save results and diagnostics to directory ----

sink(file = paste0(path1, path2, "/results and diagnostics.txt"))
print(paste0("Outcome: ", outcome_name))
print(paste0("Sample: ", sample_name))
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
summary(model.res_oasis_a1)

print("HPD Credible Intervals:")
cat("\n")
HPDinterval(model.res_oasis_a1)
cat("\n")

print("Geweke's convergence diagnostic:")
(geweke <- geweke.diag(model.res_oasis_a1))
print("Parameters in (-1.96, 1.96):")
cat("\n")
print(geweke$z > -1.96 & geweke$z < 1.96)
cat("\n")
print(paste0("All parameters in (-1.96, 1.96): ",
             all(geweke$z > -1.96 & geweke$z < 1.96)))
sink()
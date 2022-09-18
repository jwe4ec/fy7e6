# ---------------------------------------------------------------------------- #
# RJAGS Model for Treatment Dropout: High Risk CBM + Telecoaching vs.
#   High Risk CBM without Telecoaching v1.12
# Authors: Jeremy W. Eberle and Katharine E. Daniel
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# 1. TODO: Test MCAR to identify auxiliary variables ----
# ---------------------------------------------------------------------------- #

# For main outcomes paper (but not for PSYC 7505 paper), run in a separate
# script correlations between all measured variables and their missing data
# indicators. If correlation between a measured variable and a missing data
# indicator exceeds .4, then add the variable as auxiliary in the following
# code using code from Class 24 as an example.

# Note: The dependent variable of treatment dropout is completely observed.
# However, identifying auxiliary variables related to missingness on the
# incomplete predictors age and SES is still required.

# ---------------------------------------------------------------------------- #
# 2. Load packages and set working directory ----
# ---------------------------------------------------------------------------- #

library(rjags)

setwd("INSERT")

# ---------------------------------------------------------------------------- #
# 3. Treatment dropout for session 1 assessment completers ----
# ---------------------------------------------------------------------------- #

# Load data ----

tx_dropout_dat <- read.csv("./Data/tx_dropout_session_1_ax_completers_v1.0.csv")

# Extract data in wide format for RJAGS ----

# This outcome is modeled with 6 time points, so make matrix with 6 columns.

session_tx_not_complete <-
  matrix(NA, nrow = length(unique(tx_dropout_dat$participantID)),
         ncol = 6) # 6 time points

for (i in 1:length(unique(tx_dropout_dat$participantID))) {
  session_tx_not_complete[i, 1] <-
    tx_dropout_dat$session_tx_not_complete[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][1]
  session_tx_not_complete[i, 2] <-
    tx_dropout_dat$session_tx_not_complete[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][2]
  session_tx_not_complete[i, 3] <-
    tx_dropout_dat$session_tx_not_complete[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][3]
  session_tx_not_complete[i, 4] <-
    tx_dropout_dat$session_tx_not_complete[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][4]
  session_tx_not_complete[i, 5] <-
    tx_dropout_dat$session_tx_not_complete[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][5]
  session_tx_not_complete[i, 6] <-
    tx_dropout_dat$session_tx_not_complete[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][6]
}

# Because these values are same for all subjects over time, just create vector.

S1 <- c(0, 1, 1, 1, 1, 1)
S2 <- c(0, 0, 1, 2, 3, 4)

# Because these values do not change over time within a participant, just take
# the first value for each participant

SES <- rep(NA, length(unique(tx_dropout_dat$participantID)))
Age <- rep(NA, length(unique(tx_dropout_dat$participantID)))
a1 <- rep(NA, length(unique(tx_dropout_dat$participantID)))
a2_2 <- rep(NA, length(unique(tx_dropout_dat$participantID)))

for (i in 1:length(unique(tx_dropout_dat$participantID))) {
  SES[i] <-
    tx_dropout_dat$income_dollar_centered[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][1]
  Age[i] <-
    tx_dropout_dat$age_centered[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][1]
  a1[i] <-
    tx_dropout_dat$a1[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][1]
  a2_2[i] <-
    tx_dropout_dat$a2_2[which(tx_dropout_dat$participantID ==
                                    unique(tx_dropout_dat$participantID)[i])][1]
}

# Specify JAGS model with likelihood, priors, and parameters of interest ----

modelString_tx_dropout_a2_2 = "model {

  # Specify likelihood for piecewise logistic linear multilevel model

  for (i in 1:N) {

    # Random effects

    LS[i, 1:3] ~ dmnorm(muLS[i, 1:3], Inv_cov[1:3, 1:3])

    # Mean random intercept B0i

    muLS[i, 1] <- beta[1] + beta[2]*SES[i] + beta[3]*Age[i]
                  + beta[4]*SES[i]*Age[i]

    # Mean random slope B1i

    muLS[i, 2] <- beta[5] + beta[6]*a1[i] + beta[7]*SES[i] + beta[8]*Age[i]
                  + beta[9]*a1[i]*SES[i] + beta[10]*a1[i]*Age[i]
                  + beta[11]*SES[i]*Age[i]
                  + beta[12]*a1[i]*SES[i]*Age[i]

    # Mean random slope B2i

    muLS[i, 3] <- beta[13] + beta[14]*a1[i] + beta[15]*a2[i] + beta[16]*SES[i]
                  + beta[17]*Age[i] + beta[18]*a1[i]*a2[i]
                  + beta[19]*a1[i]*SES[i] + beta[20]*a1[i]*Age[i]
                  + beta[21]*a2[i]*SES[i] + beta[22]*a2[i]*Age[i]
                  + beta[23]*SES[i]*Age[i] + beta[24]*a1[i]*a2[i]*SES[i]
                  + beta[25]*a1[i]*a2[i]*Age[i] + beta[26]*a1[i]*SES[i]*Age[i]
                  + beta[27]*a2[i]*SES[i]*Age[i]
                  + beta[28]*a1[i]*a2[i]*SES[i]*Age[i]

    # Because we have missing data on the Age and SES predictors

    Age[i] ~ dnorm(muAge, Inv_Sig_Age)
    SES[i] ~ dnorm(muSES, Inv_Sig_SES)

    for (j in 1:J) {
      y[i, j] ~ dbern(p[i, j])
      
      # Expected outcome for each participant at each time point
      
      logit(p[i, j]) <- LS[i, 1] + LS[i, 2]*S1[j] + LS[i, 3]*S2[j]
    }
  }

  # Specify priors for model parameters (all uninformative)

  for (i in 1:28) {               # All 28 betas have same normal distribution
    beta[i] ~ dnorm(0, 1.0E-6)
  }

  muAge ~ dnorm(0, .0000001)
  Inv_Sig_Age ~ dgamma(.001, .001)

  muSES ~ dnorm(0, .0000001)
  Inv_Sig_SES ~ dgamma(.001, .001)

  Inv_cov[1:3, 1:3] ~ dwish(R[1:3, 1:3], 3) # Precision matrix for random effects
  R[1, 1] <- 1
  R[2, 2] <- 1
  R[3, 3] <- 1
  R[2, 1] <- 0
  R[1, 2] <- R[2, 1]
  R[3, 1] <- 0
  R[1, 3] <- R[3, 1]
  R[3, 2] <- 0
  R[2, 3] <- R[3, 2]

  Cov[1:3, 1:3] <- inverse(Inv_cov[1:3, 1:3]) # Var-cov matrix of random effects

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

  para[1] <- 2*(beta[15] + beta[18])       # HR+T vs. HR+0 slope at S2

  para[2] <- Sig_L1
  para[3] <- Sig_S2
  para[4] <- Sig_S3
  para[5] <- Cov_1_2
  para[6] <- Cov_1_3
  para[7] <- Cov_2_3
  para[8] <- rho_1_2
  para[9] <- rho_1_3
  para[10] <- rho_2_3
}
"

# Save model to directory ----

outcome_name <- "session_tx_not_complete"       # Specify outcome name
sample_name <- "session_1_ax_completers"        # Specify sample name
a2_name <- "a2_2"                               # Specify a2 contrast name

dir.create("./Results")
path1 <- paste0("./Results/", outcome_name, "_", sample_name, "_", a2_name)
dir.create(path1)

writeLines(modelString_tx_dropout_a2_2,
           con = paste0(path1, "/modelString_tx_dropout_a2_2.txt"))

# Specify initial values ----

inits <- list(beta = rep(0, 28),
              Inv_cov = diag(3),
              .RNG.name = "base::Wichmann-Hill", .RNG.seed = 11)

# Specify data ----

jagsdata <- list(N = length(unique(tx_dropout_dat$participantID)),
                 J = 6,
                 SES = SES,
                 Age = Age,
                 a1 = a1,
                 a2 = a2_2,
                 S1 = S1, S2 = S2,
                 y = session_tx_not_complete)

# Run model in JAGS ----

model_tx_dropout_a2_2 <-
  jags.model(file = paste0(path1, "/modelString_tx_dropout_a2_2.txt"),
             data = jagsdata, inits = inits, n.chains = 1)

options(scipen = 999)                 # Prevent printing in scientific notation
total_iterations <- 10                # Specify total number of iterations
burn_iterations <- total_iterations/2 # Specify burn-in period (typically half)
remaining_iterations <- total_iterations - burn_iterations

time0 <- proc.time()
update(model_tx_dropout_a2_2, n.iter = burn_iterations)
(burn_in_time <- proc.time() - time0)

time0 <- proc.time()
model.samples_tx_dropout_a2_2 <-
  coda.samples(model_tx_dropout_a2_2,
               c("para"),
               n.iter = remaining_iterations)
(sampling_time <- proc.time() - time0)

# Save posterior samples to directory, reload them, and create MCMC object ----

path2 <- paste0("/burn_", burn_iterations,
                "_total_", total_iterations)
dir.create(paste0(path1, path2))

save(model.samples_tx_dropout_a2_2,
     file = paste0(path1, path2, "/model.samples_tx_dropout_a2_2.RData"))
load(paste0(path1, path2, "/model.samples_tx_dropout_a2_2.RData"))

model.res_tx_dropout_a2_2 <-
  as.mcmc(do.call(rbind, model.samples_tx_dropout_a2_2))

# Save plots to directory ----

pdf(file = paste0(path1, path2, "/plots.pdf"))
par(mfrow=c(4,2))
plot(model.res_tx_dropout_a2_2)
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
summary(model.res_tx_dropout_a2_2)

print("HPD Credible Intervals:")
cat("\n")
HPDinterval(model.res_tx_dropout_a2_2)
cat("\n")

print("Geweke's convergence diagnostic:")
(geweke <- geweke.diag(model.res_tx_dropout_a2_2))
print("Parameters in (-1.96, 1.96):")
cat("\n")
print(geweke$z > -1.96 & geweke$z < 1.96)
cat("\n")
print(paste0("All parameters in (-1.96, 1.96): ",
             all(geweke$z > -1.96 & geweke$z < 1.96)))
sink()
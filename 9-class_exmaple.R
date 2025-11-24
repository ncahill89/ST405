# Load tidyverse for data manipulation and visualization
library(tidyverse)

# Read in running data (Cherry Blossom sample: age vs. net running time per runner)
running_dat <- read_csv("cherry_blossom_sample.csv")
running_dat

# Quick exploratory plot:
# Scatterplots of age vs. net time, faceted by runner, each with its own scale
ggplot(running_dat, aes(age, net)) +
  geom_point() +
  facet_wrap(~ runner, scales = "free")


# ----------------------------
# BAYESIAN HIERARCHICAL MODEL
# ----------------------------

# JAGS model written as a string
bhm =
  "
model{

  # Likelihood: one observation per row in dataset
  for(i in 1:n)
  {
    # Observation model: net time y.i is Normal(mu.i, sigma^2)
    y.i[i] ~ dnorm(mu.i[i], sigma^-2)

    # Runner-specific regression line
    # Each runner j has its own intercept alpha.j[j] and slope beta.j[j]
    mu.i[i] <- alpha.j[runner_index[i]] + beta.j[runner_index[i]] * (x.i[i] - 50)
  }

  # Priors for each runner's intercept and slope
  for(j in 1:n_runner)
  {
    alpha.j[j] ~ dnorm(mu_alpha, sigma_alpha^-2)  # intercepts vary around population mean mu_alpha
    beta.j[j]  ~ dnorm(mu_beta,  sigma_beta^-2)   # slopes vary around population mean mu_beta
  }

  # Hyperpriors for population-level regression parameters
  mu_alpha ~ dnorm(100, 30^-2)   # population mean intercept
  mu_beta  ~ dnorm(0, 10^-2)     # population mean slope

  # Hyperpriors for variance between runners
  sigma_alpha ~ dt(0, 20^-2, 1)T(0,)  # half-Student-t prior (positive only)
  sigma_beta  ~ dt(0,  5^-2, 1)T(0,)  # half-Student-t prior

  # Residual error SD for individual times
  sigma ~ dunif(0,50)

  # Predictions for new age values for each runner
  for(k in 1:n_pred)
  {
    mupred.k[k]     <- alpha.j[runner_index_pred[k]] + beta.j[runner_index_pred[k]] * (xpred.k[k] - 50)
    muoverall.k[k]  <- mu_alpha + mu_beta * (xpred.k[k] - 50)
  }
}
"

# Parameters we want JAGS to save
parnames <- c("alpha.j", "beta.j", "mu_alpha", "mu_beta", "mupred.k", "muoverall.k")


# ----------------------------
# DATA SENT TO JAGS
# ----------------------------
jags.data <- list(
  y.i = running_dat$net,                # observed net times
  x.i = running_dat$age,                # observed ages
  runner_index = running_dat$runner,    # runner index for hierarchical grouping
  n = nrow(running_dat),                # number of observations
  n_runner = running_dat$runner %>% unique() %>% length(),  # number of runners
  
  # Prediction grid: ages 50–60, repeated for each runner
  xpred.k = rep(50:60, 36),
  runner_index_pred = rep(1:36, each = 11),
  n_pred = 11*36                        # number of prediction rows
)

# Load JAGS libraries
library(rjags)
library(R2jags)

# Fit the hierarchical model
mod <- jags(
  data = jags.data,
  parameters.to.save = parnames,
  model.file = textConnection(bhm),
  n.iter = 20000,    # total iterations
  n.burnin = 2000,   # burn-in
  n.thin = 10        # thinning interval
)

# Diagnostic plots (trace + density for each parameter)
plot(mod)

# Matrix of posterior samples
m <- mod$BUGSoutput$sims.matrix


# ----------------------------
# SUMMARIZE PREDICTIONS
# ----------------------------

# Convert posterior samples of predictions into tidy format
pred_summary <- m %>%
  gather_rvars(mupred.k[pred_ind]) %>%
  median_qi(.value) %>%                 # compute median + 95% CI
  mutate(
    x = jags.data$xpred.k,
    runner = jags.data$runner_index_pred
  )

# Plot predicted fits for each runner
ggplot(pred_summary, aes(x = x, y = .value)) +
  geom_line() +
  geom_point(data = running_dat, aes(x = age, y = net)) +
  facet_wrap(~ runner)


# ----------------------------
# SUMMARIZE PARAMETERS
# ----------------------------

par_summary <- m %>%
  gather_rvars(alpha.j[1:36], beta.j[1:36], mu_alpha, mu_beta) %>%
  median_qi(.value)

# Print all parameter summaries
par_summary %>% print(n = 74)

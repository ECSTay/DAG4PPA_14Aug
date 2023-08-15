
## AVS Causal Model Prior Predictive Checks

rm(list=ls())
library(data.table)
library(ggplot2)

N_sim <- 10000 ## number of simulations
N_A <- 2       ## number of age groups (<50/>=50)
N_R <- 2       ## number of response categories (no/yes)
N_S <- 2       ## number of severity categories (no/yes)
N_M <- 2       ## number of medical attendance categories (no/yes)
N_D <- 2       ## number of reported medical attendance categories (no/yes)

## setup matrices to hold sampled parameters

alpha <- matrix(NA, nrow = N_sim, ncol = N_A*N_S)
beta <- matrix(NA, nrow = N_sim, ncol = N_A)
gamma <- matrix(NA, nrow = N_sim, ncol = N_A*N_S)
delta <- matrix(NA, nrow = N_sim, ncol = N_M)

## sample from prior distributions

alpha[] <- rnorm(n = N_sim*N_A*N_S, mean = 0, sd = 2)
beta[] <- rnorm(n = N_sim*N_A, mean = 0, sd = 2)
gamma[] <- rnorm(n = N_sim*N_A*N_S, mean = -4.59, sd = 2)
delta[,1] <- rnorm(n = N_sim, mean = -5, sd = 1)
delta[,2] <- rnorm(n = N_sim, mean = 5, sd = 1)

## transform parameters via linear predictors

p <- plogis(alpha)
q <- plogis(beta)
g <- plogis(gamma)
h <- plogis(delta)

## setup matrices to hold sampled data

R <- matrix(NA, nrow = N_sim, ncol = N_A*N_S)
S <- matrix(NA, nrow = N_sim, ncol = N_A)
M <- matrix(NA, nrow = N_sim, ncol = N_A*N_S)
D <- matrix(NA, nrow = N_sim, ncol = N_M)

## push sampled parameters through the likelihoods

R[] <- rbinom(n = N_sim*N_A*N_S, size = 1, prob = p)
S[] <- rbinom(n = N_sim*N_A, size = 1, prob = q)
M[] <- rbinom(n = N_sim*N_A*N_S, size = 1, prob = g)
D[] <- rbinom(n = N_sim*N_M, size = 1, prob = h)

## visualise prior predictive distributions

age_lvls <- c(paste0("\u2265", "50 years"),"<50 years")
sev_lvls <- c("High severity", "Low severity")
age_sev_lvls <- levels(interaction(expand.grid(age_lvls, sev_lvls), sep = ", "))
med_lvls <- c("No MA", "MA")

d_vis <- data.table(var = factor(rep(c("R", "S", "M", "D"), times = N_sim*c(N_A*N_S, N_A, N_A*N_S, N_M)), levels = c("R", "S", "M", "D")),
                    lvl = factor(rep(c(age_sev_lvls, age_lvls, age_sev_lvls, med_lvls), each = N_sim),
                                 levels = c(age_sev_lvls, age_lvls, med_lvls)),
                    val = c(as.vector(R), as.vector(S), as.vector(M), as.vector(D)))
d_vis <- d_vis[, .(prop = mean(val)), by = .(var, lvl)]
d_vis

ggplot(d_vis, aes(y = lvl, x = prop)) +
  facet_grid(~ var) +
  geom_col()

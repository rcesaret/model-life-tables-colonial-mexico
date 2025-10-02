# R/55_sparsebins.R
suppressPackageStartupMessages({ library(dplyr); library(purrr); library(tidyr); library(tibble) })

# Parametric bootstrap p-value for G2 when bins are few/small (multinomial).  B defaults small.
g2_bootstrap_p <- function(O, p_hat, B = 499L){
  N <- sum(O); K <- length(O)
  if(N <= 0) return(NA_real_)
  sims <- replicate(B, {
    sim <- as.vector(rmultinom(1, size = N, prob = p_hat))
    gs <- 2 * sum(ifelse(sim > 0, sim * log(sim / (N*p_hat)), 0))
    gs
  })
  G2_obs <- 2 * sum(ifelse(O > 0, O * log(O / (N*p_hat)), 0))
  mean(sims >= G2_obs)
}

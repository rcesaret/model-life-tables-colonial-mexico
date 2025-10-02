# R/00_utils.R
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(stringr); library(tibble); library(glue)
})

# Silence NSE notes for lintr/R CMD check
utils::globalVariables(c(
  "model_id","source","family","level","r_hat","AIC_weight",
  "G2","X2","Hellinger","TV","Wasserstein1","JS",
  "age_lower","age_upper","sex","Lx","age","schema_id","schema_label",
  "Name"
))

require_pkgs <- function(pkgs){
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if(length(miss)) stop(glue("Missing packages: {paste(miss, collapse=', ')}. Install them via install.packages()."), call. = FALSE)
}

# Robust CSV reader (UTF-8 / Latin-1 fallback)
read_csv_safely <- function(path){
  tryCatch(readr::read_csv(path, show_col_types = FALSE),
           error = function(e) readr::read_csv(path, locale = readr::locale(encoding = "Latin1"), show_col_types = FALSE))
}

# Build a compact unit_id
make_unit_id <- function(dataset_id, geography = NA_character_, sex = NA_character_, status = NA_character_){
  parts <- c(dataset_id, geography, sex, status)
  paste0(na.omit(parts), collapse = "_")
}

# Compute midpoints of closed intervals
age_midpoint <- function(lower, upper){
  ifelse(is.finite(upper), lower + (upper - lower)/2, lower + 5) # crude for open; 5y offset
}

# Distances on discrete probability vectors
hellinger_dist <- function(p, q){
  (1/sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
}
tv_dist <- function(p, q){
  0.5 * sum(abs(p - q))
}

# Wasserstein distance (requires transport)
wasserstein1d_safe <- function(x_support, p, q){
  if(!requireNamespace("transport", quietly = TRUE)) return(NA_real_)
  transport::wasserstein1d(x_support, p, q, p = 1)
}

# Jensen–Shannon divergence (base e). Returns finite, symmetric, bounded in [0, log(2)].
js_divergence <- function(p, q, eps = 1e-12){
  p <- pmax(p, eps); q <- pmax(q, eps)
  p <- p / sum(p); q <- q / sum(q)
  m <- 0.5 * (p + q)
  kl <- function(a, b){ sum(a * log(a / b)) }
  0.5 * kl(p, m) + 0.5 * kl(q, m)
}

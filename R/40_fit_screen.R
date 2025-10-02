# R/40_fit_screen.R
suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(tibble); library(glue)
})

# Compute predicted age shares p_i(r):
# Prefer single-year Lx inside each bin: sum_{a in bin} Lx(a) * exp(-r*a), then normalize.
# If only 5y Lx present, approximate using midpoint. (We project models to target bins beforehand.)
stable_pred <- function(Lx_by_bin, r, use_midpoints = FALSE){
  if(!use_midpoints){
    # we assume Lx_by_bin already refers to single-year Lx expanded then aggregated per bin
    # In that case, the "age_lower" are bin lows; we still need a discount within the bin.
    # Use midpoint at bin level as a practical compromise (after 1y aggregation, bias is small).
    xmid <- age_midpoint(Lx_by_bin$age_lower, Lx_by_bin$age_upper)
    w <- Lx_by_bin$Lx * exp(-r * xmid)
  } else {
    xmid <- age_midpoint(Lx_by_bin$age_lower, Lx_by_bin$age_upper)
    w <- Lx_by_bin$Lx * exp(-r * xmid)
  }
  p <- w / sum(w, na.rm = TRUE)
  p
}

loglik_multinom <- function(O, p){
  # O = observed counts, p = predicted shares
  # safe guard
  p <- pmax(p, 1e-12); p <- p / sum(p)
  sum(O * log(p))
}

gof_stats <- function(O, p){
  p <- pmax(p, 1e-12); p <- p / sum(p)
  N <- sum(O)
  E <- N * p
  G2 <- 2 * sum(ifelse(O > 0, O * log(O / E), 0))
  X2 <- sum((O - E)^2 / E)
  list(G2 = G2, X2 = X2, E = E, p = p)
}

profile_r <- function(O, Lx_by_bin, r_grid){
  rr <- seq(r_grid$default$r_min, r_grid$default$r_max, by = r_grid$default$r_step)
  ll <- vapply(rr, function(r) loglik_multinom(O, stable_pred(Lx_by_bin, r)), numeric(1))
  i_best <- which.max(ll)
  r_hat <- rr[i_best]
  # refine
  lo <- max(r_hat - r_grid$default$refine_window, min(rr))
  hi <- min(r_hat + r_grid$default$refine_window, max(rr))
  rr2 <- seq(lo, hi, by = r_grid$default$refine_step)
  ll2 <- vapply(rr2, function(r) loglik_multinom(O, stable_pred(Lx_by_bin, r)), numeric(1))
  i_best2 <- which.max(ll2)
  list(r_hat = rr2[i_best2], ll = ll2[i_best2], rr = rr2, ll_profile = ll2)
}

screen_models_for_unit <- function(obs_qc_unit, mlt_proj, r_grid){
  # obs_qc_unit: one geography x sex x dataset_id x schema_id
  # mlt_proj: all models already aggregated to target schema_id bins
  schema_id <- unique(obs_qc_unit$schema_id)
  sex_u     <- unique(obs_qc_unit$sex)

  # observed
  obs <- obs_qc_unit |> arrange(age_lower)
  O   <- obs$count_adj
  ages <- obs |> select(age_lower, age_upper)

  # candidate models (projected to same bins & sex)
  cand <- mlt_proj |> filter(schema_id == schema_id, sex %in% c(sex_u, "both"))
  models <- unique(cand$model_id)

  res <- purrr::map_dfr(models, function(m){
    Lx_bin <- cand |> filter(model_id == m) |> arrange(age_lower) |> select(model_id, source, family, level, sex, age_lower, age_upper, Lx)
    # sanity align
    Lx_bin <- Lx_bin |> right_join(ages, by = c("age_lower","age_upper")) |> fill(model_id, source, family, level, sex, .direction="downup")
    prof <- profile_r(O, Lx_bin, r_grid)
    p_hat <- stable_pred(Lx_bin, prof$r_hat)
    gs <- gof_stats(O, p_hat)
    # distances
    Pobs <- O / sum(O)
    hell <- hellinger_dist(Pobs, gs$p)
    tv   <- tv_dist(Pobs, gs$p)
    # optional Wasserstein (age midpoints)
    xmid <- age_midpoint(ages$age_lower, ages$age_upper)
    wass <- tryCatch(wasserstein1d_safe(xmid, Pobs, gs$p), error = function(e) NA_real_)
    tibble(
      unit_id    = make_unit_id(unique(obs_qc_unit$dataset_id), unique(obs_qc_unit$geography), unique(obs_qc_unit$sex)),
      dataset_id = unique(obs_qc_unit$dataset_id),
      geography  = unique(obs_qc_unit$geography),
      schema_id  = schema_id,
      sex        = unique(obs_qc_unit$sex),
      model_id   = m,
      source     = unique(Lx_bin$source),
      family     = unique(Lx_bin$family),
      level      = unique(Lx_bin$level),
      r_hat      = prof$r_hat,
      logLik     = prof$ll,
      G2         = gs$G2,
      X2         = gs$X2,
      AIC        = -2*prof$ll + 2*1,  # k=1 for r
      Hellinger  = hell,
      TV         = tv,
      Wasserstein1 = wass
    )
  })

  # AIC weights
  res <- res |>
    group_by(unit_id) |>
    mutate(delta = AIC - min(AIC, na.rm = TRUE),
           AIC_weight = exp(-0.5*delta) / sum(exp(-0.5*delta), na.rm = TRUE)) |>
    ungroup()
  res
}

rank_and_select_all <- function(screen_all, keep_cd_west = TRUE, top_n = 5){
  screen_all |>
    group_by(unit_id) |>
    arrange(AIC, .by_group = TRUE) |>
    mutate(rank = row_number()) |>
    summarise(
      finalists = list(head(cur_data_all(), top_n)),
      cd_refs   = list(filter(cur_data_all(), grepl("^CD:West:", model_id)) %>% arrange(AIC) %>% head(3)),
      .groups = "drop"
    )
}

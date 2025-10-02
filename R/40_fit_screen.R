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

screen_models_for_unit <- function(obs_qc_input, mlt_proj, r_grid){
  # Accepts a tibble of many rows (multiple geographies/sexes) and screens all units within it.
  empty_df <- tibble(
    unit_id = character(), dataset_id = character(), geography = character(),
    schema_id = character(), sex = character(), model_id = character(),
    source = character(), family = character(), level = character(),
    r_hat = double(), logLik = double(), G2 = double(), X2 = double(),
    AIC = double(), Hellinger = double(), TV = double(), Wasserstein1 = double(),
    delta = double(), AIC_weight = double()
  )

  if(nrow(obs_qc_input) == 0){
    return(empty_df)
  }

  units <- obs_qc_input |>
    distinct(dataset_id, schema_id, geography, sex)

  purrr::map_dfr(seq_len(nrow(units)), function(i){
    u <- units[i,]
    obs_qc_unit <- obs_qc_input |>
      filter(dataset_id == u$dataset_id, schema_id == u$schema_id,
             geography == u$geography, sex == u$sex) |>
      arrange(age_lower)

    # Observed
    O <- obs_qc_unit$count_adj
    ages <- obs_qc_unit |>
      select(age_lower, age_upper)

    # Candidates for this schema; handle sex
    cand_all <- mlt_proj |>
      filter(schema_id == u$schema_id)
    sx <- tolower(u$sex)
    cand <- if (sx %in% c("male","female")) {
      cand_all |> filter(tolower(sex) == sx)
    } else {
      # Build synthetic 'both' by summing male+female Lx
      cand_all |>
        filter(tolower(sex) %in% c("male","female")) |>
        group_by(model_id, source, family, level, age_lower, age_upper, schema_id, schema_label) |>
        summarise(Lx = sum(Lx, na.rm = TRUE), .groups = "drop") |>
        mutate(sex = "both")
    }
    models <- unique(cand$model_id)
    if(length(models) == 0){
      return(empty_df)
    }

    # Screen all models for this unit
    res_unit <- purrr::map_dfr(models, function(m){
      Lx_bin <- cand |>
        filter(model_id == m) |>
        arrange(age_lower) |>
        select(model_id, source, family, level, sex, age_lower, age_upper, Lx)
      Lx_bin <- Lx_bin |>
        right_join(ages, by = c("age_lower")) |>
        fill(model_id, source, family, level, sex, .direction = "downup")

      prof <- profile_r(O, Lx_bin, r_grid)
      p_hat <- stable_pred(Lx_bin, prof$r_hat)
      gs <- gof_stats(O, p_hat)
      Pobs <- O / sum(O)
      hell <- hellinger_dist(Pobs, gs$p)
      tv   <- tv_dist(Pobs, gs$p)
      xmid <- age_midpoint(ages$age_lower, ages$age_upper)
      wass <- tryCatch(wasserstein1d_safe(xmid, Pobs, gs$p), error = function(e) NA_real_)

      tibble(
        unit_id    = paste(u$dataset_id, u$geography, u$sex, sep = "_"),
        dataset_id = u$dataset_id,
        geography  = u$geography,
        schema_id  = u$schema_id,
        sex        = u$sex,
        model_id   = m,
        source     = unique(Lx_bin$source),
        family     = unique(Lx_bin$family),
        level      = unique(Lx_bin$level),
        r_hat      = prof$r_hat,
        logLik     = prof$ll,
        G2         = gs$G2,
        X2         = gs$X2,
        AIC        = -2*prof$ll + 2*1,
        Hellinger  = hell,
        TV         = tv,
        Wasserstein1 = wass
      )
    })

    if(nrow(res_unit) == 0){
      return(empty_df)
    }

    res_unit |>
      group_by(unit_id) |>
      mutate(delta = AIC - min(AIC, na.rm = TRUE),
             AIC_weight = exp(-0.5*delta) / sum(exp(-0.5*delta), na.rm = TRUE)) |>
      ungroup()
  })
}

rank_and_select_all <- function(screen_all, keep_cd_west = TRUE, top_n = 5){
  if (is.null(screen_all) || nrow(screen_all) == 0 || !("unit_id" %in% names(screen_all))){
    return(tibble(unit_id = character(), finalists = vector("list", 0), cd_refs = vector("list", 0)))
  }
  screen_all |>
    group_by(unit_id) |>
    arrange(AIC, .by_group = TRUE) |>
    reframe(
      finalists = list(head(pick(everything()), top_n)),
      cd_refs   = list({
        cd_subset <- pick(everything()) |> filter(grepl("^CD:West:", model_id))
        if(nrow(cd_subset) > 0){
          cd_subset |> arrange(AIC) |> head(3)
        } else {
          cd_subset[0,]  # Empty tibble with same structure
        }
      })
    )
}

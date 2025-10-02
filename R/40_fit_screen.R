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

# Evaluate metrics across a discrete r grid and choose r minimizing JS divergence.
screen_over_r_js <- function(O, predict_fun, r_values, x_support){
  res <- purrr::map_dfr(r_values, function(r){
    p <- predict_fun(r)
    gs <- gof_stats(O, p)
    Pobs <- O / sum(O)
    tibble(r = r,
           logLik = loglik_multinom(O, p),
           G2 = gs$G2,
           X2 = gs$X2,
           Hellinger = hellinger_dist(Pobs, gs$p),
           TV = tv_dist(Pobs, gs$p),
           JS = js_divergence(Pobs, gs$p),
           Wasserstein1 = tryCatch(wasserstein1d_safe(x_support, Pobs, gs$p), error = function(e) NA_real_))
  })
  i_best <- which.min(res$JS)
  list(best = res[i_best,], profile = res)
}

screen_models_for_unit <- function(obs_qc_input, mlt_proj, mlt_1y, r_grid, cfg_shortlist = NULL){
  # Accepts a tibble of many rows (multiple geographies/sexes) and screens all units within it.
  empty_df <- tibble(
    unit_id = character(), dataset_id = character(), geography = character(),
    schema_id = character(), sex = character(), model_id = character(),
    source = character(), family = character(), level = character(),
    r_hat = double(), logLik = double(), G2 = double(), X2 = double(),
    AIC = double(), Hellinger = double(), TV = double(), JS = double(), Wasserstein1 = double(),
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
    xmid <- age_midpoint(ages$age_lower, ages$age_upper)

    # Candidates for this schema; handle sex
    cand_all <- mlt_proj |>
      filter(schema_id == u$schema_id)
    sx <- tolower(u$sex)
    cand <- if (sx %in% c("male","female")) {
      cand_all |> filter(tolower(sex) == sx)
    } else {
      # Build synthetic 'both' by summing male+female Lx across the same model tokens
      cand_all |>
        filter(tolower(sex) %in% c("male","female")) |>
        group_by(source, family, level, age_lower, age_upper, schema_id, schema_label) |>
        summarise(Lx = sum(Lx, na.rm = TRUE), .groups = "drop") |>
        mutate(sex = "both",
               model_id = paste(source, family, level, "both", sep = ":"))
    }

    # Apply shortlist filters if provided
    if(!is.null(cfg_shortlist)){
      families_keep <- cfg_shortlist$families
      fixed_keep <- cfg_shortlist$include_cd_fixed
      # Match by family OR by source:family:level prefix (ignoring sex suffix)
      prefix <- paste(cand$source, cand$family, cand$level, sep = ":")
      cand <- cand |>
        mutate(prefix = prefix) |>
        filter(family %in% families_keep | prefix %in% fixed_keep) |>
        select(-prefix)
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

      # Build single-year data for exact projection if available
      # Extract tokens for filtering mlt_1y
      src  <- unique(Lx_bin$source)
      fam  <- unique(Lx_bin$family)
      lev  <- unique(Lx_bin$level)
      sexm <- unique(Lx_bin$sex)

      # Prepare single-year Lx for the model/sex
      sy_df <- NULL
      if(!missing(mlt_1y) && !is.null(mlt_1y) && nrow(mlt_1y) > 0){
        if(tolower(sexm) %in% c("male","female")){
          sy_df <- mlt_1y |>
            filter(source == src, family == fam, level == lev, tolower(sex) == tolower(sexm)) |>
            arrange(age_lower)
        } else {
          sy_df <- mlt_1y |>
            filter(source == src, family == fam, level == lev, tolower(sex) %in% c("male","female")) |>
            group_by(source, family, level, age_lower, age_upper) |>
            summarise(Lx = sum(Lx, na.rm = TRUE), .groups = "drop") |>
            mutate(sex = "both") |>
            arrange(age_lower)
        }
      }

      # r values for Stage S screening (coarse grid)
      r_values <- tryCatch(r_grid$r_grid_screen$values, error = function(e) NULL)
      if(is.null(r_values)){
        # fallback to legacy range if not configured
        r_values <- seq(r_grid$default$r_min, r_grid$default$r_max, by = r_grid$default$r_step)
      }

      predict_fun <- if(!is.null(sy_df) && nrow(sy_df) > 0){
        function(r) stable_shares_binned_exact(sy_df, r, ages)
      } else {
        function(r) stable_pred(Lx_bin, r)
      }

      scr <- screen_over_r_js(O, predict_fun, r_values, xmid)
      best <- scr$best

      tibble(
        unit_id    = paste(u$dataset_id, u$geography, u$sex, sep = "_"),
        dataset_id = u$dataset_id,
        geography  = u$geography,
        schema_id  = u$schema_id,
        sex        = u$sex,
        model_id   = m,
        source     = src,
        family     = fam,
        level      = lev,
        r_hat      = best$r,
        logLik     = best$logLik,
        G2         = best$G2,
        X2         = best$X2,
        AIC        = -2*best$logLik + 2*1,
        Hellinger  = best$Hellinger,
        TV         = best$TV,
        JS         = best$JS,
        Wasserstein1 = best$Wasserstein1
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

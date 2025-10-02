# R/50_finalist_fit.R
suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(tibble)
  library(glue)
})

# Build fine r grid from cfg
fine_r_seq <- function(cfg_rgrid){
  g <- cfg_rgrid$r_grid_fine
  if (!is.null(g$start) && !is.null(g$stop) && !is.null(g$by)) {
    seq(g$start, g$stop, by = g$by)
  } else if (!is.null(cfg_rgrid$default)) {
    seq(cfg_rgrid$default$r_min, cfg_rgrid$default$r_max,
        by = cfg_rgrid$default$r_step)
  } else {
    seq(0, 0.015, by = 0.0005)
  }
}

# Helper: parse fixed prefixes like "CD:West:Level1"
.parse_fixed <- function(x){
  if (length(x) == 0) return(tibble(source=character(), family=character(),
                                    level=character()))
  parts <- strsplit(x, ":", fixed = TRUE)
  tibble(
    source = vapply(parts, function(p) p[[1]], ""),
    family = vapply(parts, function(p) p[[2]], ""),
    level  = vapply(parts, function(p) p[[3]], "")
  )
}

# Derive finalist model prefixes from Stage S results and config shortlist
# Returns tibble(source,family,level, model_prefix)
derive_finalists <- function(screen_all, cfg_shortlist){
  if (is.null(screen_all) || nrow(screen_all) == 0) {
    return(tibble(source=character(), family=character(), level=character(),
                  model_prefix=character()))
  }
  comb <- screen_all |>
    mutate(model_prefix = paste(source, family, level, sep = ":")) |>
    group_by(model_prefix, source, family, level) |>
    summarise(JS_combined = sum(JS, na.rm = TRUE), .groups = "drop")

  best_per_family <- comb |>
    group_by(family) |>
    slice_min(order_by = JS_combined, n = 1, with_ties = FALSE) |>
    ungroup()

  fixed_tbl <- .parse_fixed(cfg_shortlist$include_cd_fixed) |>
    mutate(model_prefix = paste(source, family, level, sep = ":"))

  finals <- bind_rows(best_per_family, fixed_tbl) |>
    distinct(source, family, level, model_prefix)

  finals
}

# Build single-year Lx for a model prefix and sex context
.get_sy_df <- function(mlt_1y, src, fam, lev, sex){
  sx <- tolower(sex)
  if (sx %in% c("male","female")){
    mlt_1y |>
      filter(source == src, family == fam, level == lev,
             tolower(.data$sex) == sx) |>
      arrange(age_lower)
  } else {
    mlt_1y |>
      filter(source == src, family == fam, level == lev,
             tolower(.data$sex) %in% c("male","female")) |>
      group_by(source, family, level, age_lower, age_upper) |>
      summarise(Lx = sum(Lx, na.rm = TRUE), .groups = "drop") |>
      mutate(sex = "both") |>
      arrange(age_lower)
  }
}

# Profile one finalist for a unit across fine r
.profile_one <- function(finalist, u, obs_u, ages, r_vals, mlt_1y, mlt_proj){
  src <- finalist$source; fam <- finalist$family; lev <- finalist$level
  sy_df <- .get_sy_df(mlt_1y, src, fam, lev, u$sex)

  O <- obs_u$count_adj
  xmid <- age_midpoint(ages$age_lower, ages$age_upper)

  rows <- purrr::map(r_vals, function(r){
    # Prefer exact from single-year; fallback to binned projection using mlt_proj
    if (nrow(sy_df) > 0) {
      p <- stable_shares_binned_exact(sy_df, r, ages)
    } else {
      cand_all <- mlt_proj |>
        filter(schema_id == u$schema_id, source == src,
               family == fam, level == lev)
      sx <- tolower(u$sex)
      Lx_bin <- if (sx %in% c("male","female")){
        cand_all |> filter(tolower(sex) == sx)
      } else {
        cand_all |>
          filter(tolower(sex) %in% c("male","female")) |>
          group_by(source, family, level, age_lower, age_upper,
                   schema_id, schema_label) |>
          summarise(Lx = sum(Lx, na.rm = TRUE), .groups = "drop") |>
          mutate(sex = "both")
      }
      Lx_bin <- Lx_bin |>
        arrange(age_lower) |>
        right_join(ages, by = c("age_lower")) |>
        tidyr::fill(source, family, level, sex, .direction = "downup")
      p <- stable_pred(Lx_bin, r)
    }
    if (any(!is.finite(p))) return(NULL)
    gs <- gof_stats(O, p)
    Pobs <- O / sum(O)
    tibble(
      r = r,
      logLik = loglik_multinom(O, p),
      G2 = gs$G2,
      X2 = gs$X2,
      AIC = -2*logLik + 2*1,
      Hellinger = hellinger_dist(Pobs, gs$p),
      TV = tv_dist(Pobs, gs$p),
      JS = js_divergence(Pobs, gs$p),
      Wasserstein1 = tryCatch(
        wasserstein1d_safe(xmid, Pobs, gs$p),
        error = function(e) NA_real_
      )
    )
  })
  prof <- dplyr::bind_rows(rows)
  if (nrow(prof) == 0) return(NULL)
  i <- which.min(prof$AIC)
  best <- prof[i,]
  list(profile = prof, best = best)
}

# Run profiling for all finalists and units
finalist_profile_run <- function(finalists, obs_qc, mlt_1y, mlt_proj, cfg_rgrid){
  if (nrow(finalists) == 0) {
    return(list(profiles = tibble(), bestfits = tibble()))
  }
  r_vals <- fine_r_seq(cfg_rgrid)

  units <- obs_qc |>
    distinct(dataset_id, schema_id, geography, sex)

  all_prof <- list(); all_best <- list(); k <- 1L; j <- 1L
  for (i in seq_len(nrow(units))){
    u <- units[i,]
    obs_u <- obs_qc |>
      filter(dataset_id == u$dataset_id, schema_id == u$schema_id,
             geography == u$geography, sex == u$sex) |>
      arrange(age_lower)
    ages <- obs_u |>
      select(age_lower, age_upper)

    for (m in seq_len(nrow(finalists))){
      f <- finalists[m,]
      out <- .profile_one(f, u, obs_u, ages, r_vals, mlt_1y, mlt_proj)
      if (is.null(out)) next
      prof <- out$profile |>
        mutate(
          unit_id = paste(u$dataset_id, u$geography, u$sex, sep = "_"),
          dataset_id = u$dataset_id, geography = u$geography,
          schema_id = u$schema_id, sex = u$sex,
          source = f$source, family = f$family, level = f$level,
          model_prefix = f$model_prefix
        )
      all_prof[[k]] <- prof; k <- k + 1L

      best <- out$best |>
        mutate(
          unit_id = paste(u$dataset_id, u$geography, u$sex, sep = "_"),
          dataset_id = u$dataset_id, geography = u$geography,
          schema_id = u$schema_id, sex = u$sex,
          source = f$source, family = f$family, level = f$level,
          model_prefix = f$model_prefix
        )
      all_best[[j]] <- best; j <- j + 1L
    }
  }

  profiles <- dplyr::bind_rows(all_prof)
  bestfits <- dplyr::bind_rows(all_best)

  list(profiles = profiles, bestfits = bestfits)
}

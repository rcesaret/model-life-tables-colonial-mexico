# R/35_projector.R
suppressPackageStartupMessages({ library(dplyr); library(purrr); library(tidyr); library(tibble) })

# Helper: aggregate single-year Lx to arbitrary bins
agg_1y_to_bins <- function(df1y, bins){
  # df1y: model_id, sex, age_lower==age_upper (single), Lx
  # bins: tibble(age_lower, age_upper) with NA upper meaning open interval
  purrr::map_dfr(seq_len(nrow(bins)), function(i){
    lo <- bins$age_lower[i]; up <- bins$age_upper[i]
    if(is.na(up)){
      sl <- df1y |> filter(age_lower >= lo)
    } else {
      sl <- df1y |> filter(age_lower >= lo, age_upper <= up)
    }
    sl |>
      group_by(model_id, source, family, level, sex) |>
      summarise(Lx = sum(Lx, na.rm=TRUE), .groups = "drop") |>
      mutate(age_lower = lo, age_upper = up)
  })
}

# Helper: aggregate abridged Lx to nearby bins (approximate)
agg_abridged_to_bins <- function(df5y, bins){
  # For closed bins we use midpoints ~ ok; for open, we simply sum Lx
  purrr::map_dfr(seq_len(nrow(bins)), function(i){
    lo <- bins$age_lower[i]; up <- bins$age_upper[i]
    if(is.na(up)){
      sl <- df5y |> filter(age_lower >= lo)
    } else {
      sl <- df5y |> filter(age_lower >= lo, age_upper <= up)
    }
    sl |>
      group_by(model_id, source, family, level, sex) |>
      summarise(Lx = sum(Lx, na.rm=TRUE), .groups = "drop") |>
      mutate(age_lower = lo, age_upper = up)
  })
}

# Public: project all models (abridged and/or 1y) to every schema
project_all_models_to_schemas <- function(mlt_abridged, mlt_1y, schemas, prefer_1y = TRUE){
  out <- purrr::imap_dfr(schemas, function(s, sid){
    bins <- s$bins
    # Always aggregate single-year where available
    agg1 <- if (prefer_1y && nrow(mlt_1y)) agg_1y_to_bins(mlt_1y, bins) else tibble()
    # For abridged, exclude any model_ids present in single-year to avoid duplicates
    abr_excl <- mlt_abridged
    if (nrow(agg1)){
      ids_1y <- unique(mlt_1y$model_id)
      abr_excl <- mlt_abridged |> filter(!model_id %in% ids_1y)
    }
    agg5 <- if (nrow(abr_excl)) agg_abridged_to_bins(abr_excl, bins) else tibble()
    bind_rows(agg1, agg5) |> mutate(schema_id = sid, schema_label = s$label)
  })
  out
}

# Exact stable shares from single-year Lx at integer ages.
# single_year_df: tibble with columns age_lower (integer), age_upper (same), Lx.
# bins: tibble(age_lower, age_upper) in desired schema order; open age has NA upper.
# Returns a numeric vector of length nrow(bins) summing to 1.
stable_shares_binned_exact <- function(single_year_df, r, bins){
  stopifnot(is.data.frame(single_year_df), nrow(single_year_df) > 0)
  stopifnot(is.data.frame(bins), nrow(bins) > 0)
  df <- single_year_df |>
    mutate(age = as.integer(round(age_lower))) |>
    filter(!is.na(age), !is.na(Lx)) |>
    mutate(w = Lx * exp(-r * age))

  # Sum weights by target bins
  w_by_bin <- purrr::map_dbl(seq_len(nrow(bins)), function(i){
    lo <- bins$age_lower[i]; up <- bins$age_upper[i]
    if(is.na(up)){
      sum(df$w[df$age >= lo], na.rm = TRUE)
    } else {
      sum(df$w[df$age >= lo & df$age <= up], na.rm = TRUE)
    }
  })
  total <- sum(w_by_bin, na.rm = TRUE)
  if(total <= 0) return(rep(NA_real_, nrow(bins)))
  as.numeric(w_by_bin / total)
}

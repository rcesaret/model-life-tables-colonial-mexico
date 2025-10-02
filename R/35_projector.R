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
    if(prefer_1y && nrow(mlt_1y)){
      # aggregate single-year (more exact for open intervals)
      agg <- agg_1y_to_bins(mlt_1y, bins)
    } else {
      agg <- agg_abridged_to_bins(mlt_abridged, bins)
    }
    agg |> mutate(schema_id = sid, schema_label = s$label)
  })
  out
}

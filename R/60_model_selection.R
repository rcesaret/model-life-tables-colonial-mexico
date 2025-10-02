# R/60_model_selection.R
suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(tibble)
})

# Compute AIC weights within groups
.add_aic_weights <- function(df, group_vars){
  df |>
    group_by(across(all_of(group_vars))) |>
    mutate(delta = AIC - min(AIC, na.rm = TRUE)) |>
    mutate(AIC_weight = exp(-0.5 * delta) /
             sum(exp(-0.5 * delta), na.rm = TRUE)) |>
    ungroup()
}

# Unit-level rankings (bestfits are already AIC-minimized over r)
select_unit_rankings <- function(bestfits){
  if (is.null(bestfits) || nrow(bestfits) == 0) {
    return(tibble())
  }
  out <- bestfits |>
    mutate(model_prefix = paste(source, family, level, sep = ":")) |>
    .add_aic_weights(group_vars = c("unit_id")) |>
    arrange(unit_id, AIC)
  out
}

# Combined rankings across units by summing AIC (independent datasets)
select_combined_rankings <- function(bestfits){
  if (is.null(bestfits) || nrow(bestfits) == 0) {
    return(tibble())
  }
  comb <- bestfits |>
    mutate(model_prefix = paste(source, family, level, sep = ":")) |>
    group_by(model_prefix, source, family, level) |>
    summarise(AIC = sum(AIC, na.rm = TRUE), .groups = "drop") |>
    .add_aic_weights(group_vars = character()) |>
    arrange(AIC)
  comb
}

# Convenience wrapper
model_select <- function(bestfits){
  list(
    unit_rankings = select_unit_rankings(bestfits),
    combined_rankings = select_combined_rankings(bestfits)
  )
}

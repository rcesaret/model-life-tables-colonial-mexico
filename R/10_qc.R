# R/10_qc.R
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(tibble); library(DemoTools) })

# Parsers for your files (add others later if needed)
read_observed <- function(ds){
  reader <- ds$reader
  path   <- ds$path
  if(reader == "oaxaca"){
    df <- read_csv_safely(path)
    # Expect a column like AgeGroup (0_4, 5_9, ... 90_up) and Total, Male, Female
    # Convert labels to numeric bounds
    ages <- df$AgeGroup
    bounds <- strsplit(gsub("_", "-", gsub("up","+", ages, fixed = TRUE)), "-")
    lo <- as.numeric(vapply(bounds, function(x) gsub("\\+","",x[1]), ""))
    up <- vapply(bounds, function(x){
      if(length(x) == 1 || grepl("\\+", x[length(x)])) NA_character_ else x[2]
    }, "")
    up <- as.numeric(up)
    long <- df |>
      mutate(age_lower = lo, age_upper = up) |>
      select(age_lower, age_upper, total = Total, male = Male, female = Female) |>
      tidyr::pivot_longer(c(total, male, female), names_to = "sex", values_to = "count") |>
      mutate(sex = dplyr::recode(sex, total="total", male="male", female="female"),
             dataset_id = ds$dataset_id,
             schema_id  = ds$schema_id,
             geography  = ds$geography,
             year       = ds$year)
    return(long)
  }
  if(reader == "us1790"){
    df <- read_csv_safely(path)
    # We expect columns AgesLower, AgesUpper, Total, Male, Female
    long <- df |>
      rename(age_lower = AgesLower, age_upper = AgesUpper) |>
      select(Name, age_lower, age_upper, Total, Male, Female) |>
      tidyr::pivot_longer(c(Total, Male, Female), names_to = "sex", values_to = "count") |>
      mutate(sex = tolower(sex),
             dataset_id = ds$dataset_id,
             schema_id  = ds$schema_id,
             geography  = Name,
             year       = ds$year)
    return(long)
  }
  stop("Unknown reader specified in datasets.yml")
}

# QC and optional light smoothing (Arriaga / Carrier-Farrag).
qc_and_adjust <- function(obs_long, cfg_qc){
  # We compute Age-Sex Accuracy Index on 5y bins (works on grouped ages).
  thresholds <- cfg_qc$thresholds
  # split by geography x sex or total
  obs_long |>
    group_by(dataset_id, schema_id, geography, sex) |>
    arrange(age_lower, .by_group = TRUE) |>
    reframe(
      age_lower = age_lower,
      age_upper = age_upper,
      count_raw = count,
      count_adj = count,
      asai = NA_real_,
      smoothing_note = "none"
    ) |>
    ungroup() |>
    mutate(unit_id = paste(dataset_id, geography, sex, sep = "_"))
}

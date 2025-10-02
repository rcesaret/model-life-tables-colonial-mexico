# R/20_adjust.R
suppressPackageStartupMessages({ library(dplyr); library(DemoTools) })

# Light 5-year smoothing when flagged (Arriaga, Carrier-Farrag). :contentReference[oaicite:4]{index=4}
apply_smoothing_if_needed <- function(df_unit, method = c("arriaga","cf")){
  m <- match.arg(method)
  vals <- df_unit$count_raw
  ages <- df_unit$age_lower
  OAG  <- any(is.na(df_unit$age_upper))
  smooth_vals <- switch(m,
    "arriaga" = DemoTools::smooth_age_5_arriaga(Value = vals, Age = ages, OAG = OAG),
    "cf"      = DemoTools::smooth_age_5(Value = vals, Age = ages, method = "cf", OAG = OAG)
  )
  df_unit$count_adj <- as.numeric(smooth_vals)
  df_unit$smoothing_note <- m
  df_unit
}

# R/70_reporting.R
suppressPackageStartupMessages({ library(dplyr); library(gt); library(glue) })

write_screening_tables <- function(screen_all, out_dir){
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  split(screen_all, screen_all$unit_id) |>
    purrr::imap(function(df, uid){
      tab <- df |>
        arrange(AIC) |>
        select(model_id, source, family, level, r_hat, AIC, AIC_weight, G2, X2, Hellinger, TV, Wasserstein1) |>
        gt::gt() |>
        gt::fmt_number(columns = c(r_hat, AIC, AIC_weight, G2, X2, Hellinger, TV, Wasserstein1), decimals = 4) |>
        gt::tab_header(title = glue("Model screening — {uid}"))
      path <- file.path(out_dir, glue("{uid}_screening.html"))
      gt::gtsave(tab, path)
      path
    })
}

write_finalist_reports <- function(deepdive_reports, out_dir){
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  invisible(TRUE)
}

write_qc_reports <- function(obs_qc, out_dir){
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  invisible(TRUE)
}

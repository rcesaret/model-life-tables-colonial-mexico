# R/80_final_reports.R
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble)
  library(readr); library(gt); library(glue)
})

# Write core finalist tables (CSV + optional HTML summary)
write_final_tables <- function(bestfits, unit_rankings, combined_rankings,
                               out_dir){
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # CSVs
  readr::write_csv(bestfits, file.path(out_dir, "bestfits.csv"))
  readr::write_csv(unit_rankings, file.path(out_dir, "unit_rankings.csv"))
  readr::write_csv(combined_rankings,
                   file.path(out_dir, "combined_rankings.csv"))

  # Simple HTML table for combined rankings (Top 20)
  top <- combined_rankings |>
    arrange(AIC) |>
    slice_head(n = 20)
  tab <- top |>
    gt::gt() |>
    gt::fmt_number(columns = c(AIC, AIC_weight), decimals = 4) |>
    gt::tab_header(title = "Finalists — Combined AIC Rankings (Top 20)")
  path_html <- file.path(out_dir, "combined_rankings_top20.html")
  gt::gtsave(tab, path_html)

  invisible(list(csv = c("bestfits.csv","unit_rankings.csv",
                         "combined_rankings.csv"),
                 html = path_html))
}

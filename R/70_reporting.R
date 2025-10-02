# R/70_reporting.R
suppressPackageStartupMessages({
  library(dplyr); library(gt); library(glue); library(ggplot2); library(readr)
})

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

# New: CSV outputs and Top-15 JS plot for Stage S
write_screening_csvs <- function(screen_all, out_dir){
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Focus results: keep key columns for inspection
  focus <- screen_all |>
    select(unit_id, model_id, source, family, level, r_hat, AIC, G2, X2,
           Hellinger, TV, JS, Wasserstein1)
  readr::write_csv(focus,
                   file.path(out_dir, "screening_results_focus.csv"))

  # Combined JS across units per model (lower is better)
  combined <- screen_all |>
    group_by(model_id, source, family, level) |>
    summarise(JS_combined = sum(JS, na.rm = TRUE), .groups = "drop")

  # Best per family by combined JS
  best_per_family <- combined |>
    group_by(family) |>
    slice_min(order_by = JS_combined, n = 1, with_ties = FALSE) |>
    ungroup()
  readr::write_csv(best_per_family,
                   file.path(out_dir, "shortlist_best_per_family.csv"))

  invisible(list(focus = focus, combined = combined,
                 best_per_family = best_per_family))
}

plot_topN <- function(screen_all, out_dir, N = 15){
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  combined <- screen_all |>
    group_by(model_id, source, family, level) |>
    summarise(JS_combined = sum(JS, na.rm = TRUE), .groups = "drop") |>
    arrange(JS_combined)
  topN <- head(combined, N)
  p <- ggplot(topN,
              aes(x = reorder(model_id, -JS_combined), y = JS_combined,
                  fill = family)) +
    geom_col() +
    coord_flip() +
    labs(title = glue("Top {N} by combined JS"), x = "model_id",
         y = "JS (lower is better)") +
    theme_minimal(base_size = 10)
  out_path <- file.path(out_dir, "top15_js.png")
  ggsave(out_path, plot = p, width = 7, height = 5, dpi = 300)
  out_path
}

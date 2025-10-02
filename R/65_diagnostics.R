# R/65_diagnostics.R
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(purrr)
  library(ggplot2)
})

# Predict shares helper from single-year Lx
.pred_p_from_sy <- function(mlt_1y, src, fam, lev, sex, r, ages){
  sx <- tolower(sex)
  sy_df <- if (sx %in% c("male","female")){
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
  if (nrow(sy_df) == 0) return(rep(NA_real_, nrow(ages)))
  stable_shares_binned_exact(sy_df, r, ages)
}

# Produce diagnostic figures for finalists
# - AIC vs r profile for each (unit, model_prefix)
# - Overlay observed vs predicted shares at best r
finalist_diagnostics_figs <- function(bestfits, profiles, obs_qc, mlt_1y, out_dir){
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (is.null(bestfits) || nrow(bestfits) == 0) return(character())

  paths <- list()
  units <- bestfits |>
    distinct(unit_id, dataset_id, geography, schema_id, sex)

  for (i in seq_len(nrow(units))){
    u <- units[i,]
    obs_u <- obs_qc |>
      filter(dataset_id == u$dataset_id, schema_id == u$schema_id,
             geography == u$geography, sex == u$sex) |>
      arrange(age_lower)
    ages <- obs_u |>
      select(age_lower, age_upper)
    O <- obs_u$count_adj
    Pobs <- O / sum(O)

    bf_u <- bestfits |>
      filter(unit_id == u$unit_id)

    for (j in seq_len(nrow(bf_u))){
      b <- bf_u[j,]
      prefix <- paste(b$source, b$family, b$level, sep = ":")

      # Profile figure
      prof <- profiles |>
        filter(unit_id == u$unit_id,
               source == b$source, family == b$family, level == b$level)
      if (nrow(prof) > 0){
        p1 <- ggplot(prof, aes(x = r, y = AIC)) +
          geom_line(color = "steelblue") +
          geom_point(data = b, aes(x = r, y = AIC), color = "red", size = 2) +
          theme_minimal(base_size = 10) +
          labs(title = paste("AIC vs r —", u$unit_id, prefix), x = "r", y = "AIC")
        f1 <- file.path(out_dir, paste0(u$unit_id, "_", prefix, "_profile.png"))
        ggsave(f1, plot = p1, width = 6.5, height = 4.2, dpi = 300)
        paths[[length(paths)+1]] <- f1
      }

      # Overlay observed vs predicted at best r
      Pfit <- .pred_p_from_sy(mlt_1y, b$source, b$family, b$level, b$sex,
                              b$r, ages)
      df_plot <- tibble(
        bin = paste0(ages$age_lower, "-", ifelse(is.na(ages$age_upper), "+", ages$age_upper)),
        Observed = as.numeric(Pobs), Predicted = as.numeric(Pfit)
      ) |>
        pivot_longer(c(Observed, Predicted), names_to = "type",
                     values_to = "share")
      p2 <- ggplot(df_plot, aes(x = bin, y = share, fill = type)) +
        geom_col(position = "dodge") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        labs(title = paste("Observed vs Predicted —", u$unit_id, prefix),
             x = "Age bin", y = "Share")
      f2 <- file.path(out_dir, paste0(u$unit_id, "_", prefix, "_overlay.png"))
      ggsave(f2, plot = p2, width = 7.5, height = 4.5, dpi = 300)
      paths[[length(paths)+1]] <- f2
    }
  }
  unlist(paths)
}

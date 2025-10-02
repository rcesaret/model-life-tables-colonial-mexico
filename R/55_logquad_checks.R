# R/55_logquad_checks.R
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble)
})

# Placeholder for MortCast log-quadratic plausibility checks.
# Returns a tibble with unit_id, model_prefix, and a note.
# TODO: Implement full relational log-quad checks when e0/qx is available.
logquad_checks <- function(bestfits, mlt_1y){
  if (is.null(bestfits) || nrow(bestfits) == 0) {
    return(tibble(unit_id = character(), model_prefix = character(),
                  note = character()))
  }
  tibble(
    unit_id = bestfits$unit_id,
    model_prefix = paste(bestfits$source, bestfits$family,
                         bestfits$level, sep = ":"),
    note = "log-quad checks pending full implementation"
  )
}

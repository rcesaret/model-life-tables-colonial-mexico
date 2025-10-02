# R/05_config.R
suppressPackageStartupMessages({ library(yaml); library(dplyr); library(purrr); library(tibble); library(glue) })

load_schema_file <- function(path){
  y <- yaml::read_yaml(path)
  bins <- purrr::map_dfr(y$bins, function(b){
    lo <- as.numeric(b[[1]])
    up <- if(length(b) < 2 || is.null(b[[2]])) NA_real_ else as.numeric(b[[2]])
    tibble::tibble(age_lower = lo, age_upper = up)
  })
  list(schema_id = y$schema_id, label = y$label, bins = bins)
}

load_schema_registry <- function(dir){
  files <- list.files(dir, pattern = "\\.yml$", full.names = TRUE)
  regs <- purrr::map(files, load_schema_file)
  ids <- vapply(regs, `[[`, "", "schema_id")
  names(regs) <- ids
  regs
}

load_dataset_registry <- function(path){
  y <- yaml::read_yaml(path)
  purrr::map_dfr(y$datasets, tibble::as_tibble)
}

load_rgrid <- function(path){
  y <- yaml::read_yaml(path)
  y
}

load_qc <- function(path){
  y <- yaml::read_yaml(path)
  y
}

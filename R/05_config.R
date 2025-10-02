# R/05_config.R
suppressPackageStartupMessages({ library(yaml); library(dplyr); library(purrr); library(tibble); library(glue) })

load_schema_file <- function(path){
  y <- yaml::read_yaml(path)
  bins <- tibble::as_tibble(do.call(rbind, y$bins)) |>
    transmute(age_lower = as.numeric(V1), age_upper = as.numeric(V2))
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
  tibble::as_tibble(y$datasets)
}

load_rgrid <- function(path){
  y <- yaml::read_yaml(path)
  y
}

load_qc <- function(path){
  y <- yaml::read_yaml(path)
  y
}

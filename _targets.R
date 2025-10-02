# _targets.R
library(targets)

tar_option_set(packages = c(
  "yaml","dplyr","tidyr","purrr","readr","stringr","tibble",
  "ggplot2","gt","glue","furrr","DemoTools","ungroup","transport","MortCast"
))

source("R/00_utils.R")
source("R/05_config.R")
source("R/10_qc.R")
source("R/20_adjust.R")
source("R/30_models.R")
source("R/35_projector.R")
source("R/40_fit_screen.R")
source("R/50_deepdive.R")
source("R/55_sparsebins.R")
source("R/70_reporting.R")

plan(multisession)

list(
  # Config
  tar_target(cfg_datasets, load_dataset_registry("configs/datasets.yml")),
  tar_target(cfg_schemas,  load_schema_registry("configs/schemas")),
  tar_target(cfg_rgrid,    load_rgrid("configs/rgrid.yml")),
  tar_target(cfg_qc,       load_qc("configs/qc.yml")),

  # Read & standardize model life tables (UN 2011 + CD West)
  tar_target(un_abr_path, "data_raw/model_tables/unpd_2011_mlt_130_1y_abridged.csv"),
  tar_target(un_1y_path,  "data_raw/model_tables/MLT_UN2011_130_1y_complete.csv"),
  tar_target(cd_west_path,"data_raw/model_tables/WestLifeTables.csv"),

  tar_target(mlt_abridged, read_un_abridged(un_abr_path)),
  tar_target(mlt_1y,       read_un_1y(un_1y_path)),
  tar_target(mlt_cdwest,   read_cd_west(cd_west_path)),
  tar_target(mlt_all,      bind_rows(mlt_abridged, mlt_cdwest)),

  # Project models to each schema (abridged -> rescale; prefer 1y if complex/open bins)
  tar_target(mlt_proj, project_all_models_to_schemas(
    mlt_abridged = mlt_all, mlt_1y = mlt_1y, schemas = cfg_schemas, prefer_1y = TRUE
  )),

  # Read observed data (dynamic per dataset)
  tar_target(obs_raw, read_observed(cfg_datasets), pattern = map(cfg_datasets)),
  tar_target(obs_qc,  qc_and_adjust(obs_raw, cfg_qc), pattern = map(obs_raw)),

  # Screen all models per observed unit (match by schema_id & sex)
  tar_target(screen_unit, screen_models_for_unit(
    obs_qc, mlt_proj, cfg_rgrid
  ), pattern = map(obs_qc)),

  tar_target(screen_all, bind_rows(screen_unit)),

  # Select finalists (top N + CD West 1/3/8 at r=0.05 as reference)
  tar_target(shortlists, rank_and_select_all(screen_all, keep_cd_west = TRUE, top_n = 5)),

  # Deep-dive diagnostics for finalists
  tar_target(deepdive_reports, deepdive_all(shortlists, obs_qc, mlt_proj, cfg_rgrid)),

  # Robustness (small K bins -> bootstrap p-values)
  tar_target(robustness, robustness_all(shortlists, obs_qc, mlt_proj, cfg_rgrid)),

  # Reporting
  tar_target(screen_tables, write_screening_tables(screen_all, out_dir = "reports/screening")),
  tar_target(finalist_decks, write_finalist_reports(deepdive_reports, out_dir = "reports/finalists")),
  tar_target(qc_appendices, write_qc_reports(obs_qc, out_dir = "reports/qc"))
)

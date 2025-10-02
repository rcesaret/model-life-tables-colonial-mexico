# R/30_models.R
suppressPackageStartupMessages({ library(dplyr); library(readr); library(stringr); library(tidyr); library(tibble) })

# --- UN 2011 abridged ---
# We expect columns like: Family, Type (or Type_MLT), Sex, Level/E0,
# age labelling via 'x' (lower age) and 'n' (width), and life-table cols Lxn, lxn, qxn, mxn.
# The UN provides model life tables by abridged age groups and by single years. :contentReference[oaicite:0]{index=0}
read_un_abridged <- function(path){
  df <- read_csv_safely(path)

  # Try to detect age bounds
  lower <- if("x" %in% names(df)) df$x else if("AgeLower" %in% names(df)) df$AgeLower else if("Age" %in% names(df)) df$Age else NA
  width <- if("n" %in% names(df)) df$n else if("AgeWidth" %in% names(df)) df$AgeWidth else NA

  # If width not provided, infer from common abridged pattern; open interval will be NA
  if(all(is.na(width)) && !all(is.na(lower))){
    width <- dplyr::lead(lower) - lower
    width[is.na(width) | width <= 0] <- NA
  }
  age_upper <- ifelse(is.na(width), NA_real_, lower + width - 1)

  # Life table columns (abridged)
  Lxn <- dplyr::coalesce(df$Lxn, df$`Lxn`, df$`Lx n`, df$`LxN`)
  lxn <- dplyr::coalesce(df$lxn, df$`lxn`)
  qxn <- dplyr::coalesce(df$qxn, df$`qxn`)
  mxn <- dplyr::coalesce(df$mxn, df$`mxn`)

  sex  <- dplyr::coalesce(df$Sex, df$sex, df$SEX)
  fam  <- dplyr::coalesce(df$Family, df$Type, df$Type_MLT, df$Pattern)
  lev  <- dplyr::coalesce(df$Level, df$E0, df$`e(0)`, df$Variant)

  out <- tibble(
    source  = "UN2011",
    family  = as.character(fam),
    level   = as.character(lev),
    sex     = as.character(sex),
    age_lower = as.numeric(lower),
    age_upper = as.numeric(age_upper),
    Lx      = as.numeric(Lxn),
    lx      = as.numeric(lxn),
    mx      = as.numeric(mxn),
    qx      = as.numeric(qxn)
  ) |>
    filter(!is.na(age_lower)) |>
    mutate(model_id = paste(source, family, level, sex, sep = ":"))
  out
}

# --- UN 2011 single-year ---
# Columns like lx1, Lx1, qx1, mx1 at ages 0,1,2,... :contentReference[oaicite:1]{index=1}
read_un_1y <- function(path){
  df <- read_csv_safely(path)
  age <- dplyr::coalesce(df$x, df$Age, df$age)

  Lx1 <- dplyr::coalesce(df$Lx1, df$`Lx1`)
  lx1 <- dplyr::coalesce(df$lx1, df$`lx1`)
  mx1 <- dplyr::coalesce(df$mx1, df$`mx1`)
  qx1 <- dplyr::coalesce(df$qx1, df$`qx1`)

  sex  <- dplyr::coalesce(df$Sex, df$sex, df$SEX)
  fam  <- dplyr::coalesce(df$Family, df$Type, df$Type_MLT, df$Pattern)
  lev  <- dplyr::coalesce(df$Level, df$E0, df$`e(0)`, df$Variant)

  tibble(
    source  = "UN2011",
    family  = as.character(fam),
    level   = as.character(lev),
    sex     = as.character(sex),
    age_lower = as.numeric(age),
    age_upper = as.numeric(age),   # single-year
    Lx      = as.numeric(Lx1),
    lx      = as.numeric(lx1),
    mx      = as.numeric(mx1),
    qx      = as.numeric(qx1)
  ) |>
    filter(!is.na(age_lower)) |>
    mutate(model_id = paste(source, family, level, sex, sep = ":"))
}

# --- Coale-Demeny West reference tables (user CSV) ---
# Expect columns: sex,family='West',level,age_lower,age_upper,Lx (or nLx)
read_cd_west <- function(path){
  df <- read_csv_safely(path)
  df <- df |> rename_with(~tolower(.x))

  df_long <- df |>
    tidyr::pivot_longer(
      cols = -c(agebracket, agebracketstart, agebracketend),
      names_to = "name",
      values_to = "Lx"
    ) |>
    mutate(
      name_upper = toupper(name),
      tokens = stringr::str_match(name_upper, "^(\\d+)([A-Z]+)_(M|F)_R(\\d+)$"),
      level_code = tokens[, 2],
      family_code = tokens[, 3],
      sex_code = tokens[, 4],
      r_code = tokens[, 5],
      Lx = as.numeric(Lx)
    ) |>
    tidyr::drop_na(level_code, family_code, sex_code)

  df_long |>
    mutate(
      source = "CD",
      family = dplyr::case_when(
        family_code == "W" ~ "West",
        TRUE ~ toupper(family_code)
      ),
      level = paste0("Level", level_code),
      sex = dplyr::case_when(
        sex_code == "M" ~ "male",
        sex_code == "F" ~ "female",
        TRUE ~ sex_code
      ),
      age_lower = as.numeric(agebracketstart),
      age_upper = suppressWarnings(as.numeric(agebracketend)),
      age_upper = ifelse(is.na(age_upper), NA_real_, age_upper)
    ) |>
    select(source, family, level, sex, age_lower, age_upper, Lx) |>
    mutate(model_id = paste(source, family, level, sex, sep=":"))
}

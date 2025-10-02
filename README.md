# Model Life Tables for Colonial Mexico

A reproducible demographic analysis pipeline for matching historical census data from colonial Mexico to identify best-fitting model life tables using a variety of demographic methods.

## Overview

This project implements a rigorous, likelihood-based framework for evaluating goodness-of-fit between historical population age distributions and standard model life tables (UN 2011, Coale-Demeny West). The pipeline:

- **Screens hundreds of candidate models** against observed age-sex distributions
- **Estimates growth rates** (`r`) via multinomial likelihood profiling
- **Ranks models** using AIC weights and multiple distance metrics (Hellinger, Total Variation, Wasserstein)
- **Handles diverse age schemas** including open intervals at 50+, 90+, and coarse 16th-century bins
- **Applies demographic QC** with age-heaping diagnostics and optional smoothing (Arriaga, Carrier-Farrag)
- **Generates publication-ready outputs** (HTML tables, diagnostic plots, finalist reports)

The methodology follows best practices from historical demography and IUSSP/UN protocols, emphasizing transparent model selection and robust inference for sparse or heaped data.

---

## Repository Structure

```
model-life-tables-colonial-mexico/
├── _targets.R              # Main pipeline orchestration (targets framework)
├── README.md               # This file
├── LICENSE
├── .gitignore
│
├── configs/                # Configuration files (YAML)
│   ├── datasets.yml        # Dataset registry (paths, schemas, readers)
│   ├── rgrid.yml           # Growth rate grid for profiling
│   ├── qc.yml              # Quality control thresholds
│   └── schemas/            # Age-binning schemas
│       ├── schema_oax_90plus.yml       # Oaxaca 1777 (90+ open)
│       ├── schema_1790_50plus.yml      # US/MX 1790 (50+ open)
│       └── schema_16c_coarse.yml       # 16th century (coarse bins)
│
├── data_raw/               # Raw input data (not tracked in git)
│   ├── 18th_century/
│   │   ├── Oaxaca1777.csv
│   │   └── Census1790_States_AgeSex2.csv
│   ├── model_tables/
│   │   ├── unpd_2011_mlt_130_1y_abridged.csv
│   │   ├── MLT_UN2011_130_1y_complete.csv
│   │   └── WestLifeTables.csv
│   └── 16th_century/       # (future datasets)
│
├── data_intermediate/      # Intermediate outputs (targets cache)
├── data_model/             # Processed model tables
│
├── reports/                # Generated outputs
│   ├── screening/          # Model screening tables (HTML)
│   ├── finalists/          # Deep-dive reports for top models
│   └── qc/                 # Quality control summaries
│
└── R/                      # Analysis scripts
    ├── 00_utils.R          # Utilities (CSV readers, distance metrics)
    ├── 05_config.R         # Config loaders (YAML parsers)
    ├── 10_qc.R             # Quality control & data readers
    ├── 20_adjust.R         # Smoothing methods (Arriaga, Carrier-Farrag)
    ├── 30_models.R         # Model life table parsers (UN, CD West)
    ├── 35_projector.R      # Project models to arbitrary age schemas
    ├── 40_fit_screen.R     # Likelihood profiling & screening
    ├── 50_deepdive.R       # Finalist diagnostics (residuals, profiles)
    ├── 55_sparsebins.R     # Bootstrap p-values for sparse data
    └── 70_reporting.R      # Publication outputs (tables, figures)
```

---

## Prerequisites

### Software

- **R** ≥ 4.1.0 (recommended: 4.3+)
- **RStudio** (optional, for interactive development)

### R Packages

Install all dependencies with:

```r
install.packages(c(
  "targets",      # Pipeline orchestration
  "yaml",         # Config parsing
  "dplyr",        # Data manipulation
  "tidyr",        # Data reshaping
  "purrr",        # Functional programming
  "readr",        # CSV I/O
  "stringr",      # String operations
  "tibble",       # Modern data frames
  "ggplot2",      # Plotting
  "gt",           # Publication tables
  "glue",         # String interpolation
  "furrr",        # Parallel mapping
  "DemoTools",    # Demographic methods (smoothing, indices)
  "ungroup",      # PCLM graduation
  "transport",    # Wasserstein distance
  "MortCast"      # Log-quadratic life tables
))
```

**Note**: `DemoTools` may require additional system dependencies on Linux. See [DemoTools installation guide](https://timriffe.github.io/DemoTools/).

---

## Configuration

### 1. Dataset Registry (`configs/datasets.yml`)

Register each observed dataset with:

- **`dataset_id`**: Unique identifier
- **`path`**: Relative path to CSV
- **`schema_id`**: Age-binning schema (must match a file in `configs/schemas/`)
- **`reader`**: Parser function name (`oaxaca`, `us1790`, etc.)
- **`sex_mode`**: `"total"`, `"both"`, or `"split"`
- **`geography`**: Region/state name (or `geography_field` for multi-region files)
- **`year`**: Census year

**Example**:

```yaml
datasets:
  - dataset_id: OAX1777_total
    path: data_raw/18th_century/Oaxaca1777.csv
    schema_id: schema_oax_90plus
    reader: oaxaca
    sex_mode: total
    geography: Oaxaca
    year: 1777
```

### 2. Age Schemas (`configs/schemas/*.yml`)

Define age bins as `[lower, upper]` pairs. Use `null` for open intervals.

**Example** (`schema_1790_50plus.yml`):

```yaml
schema_id: schema_1790_50plus
label: "US 1790 (50+ open)"
bins:
  - [0, 4]
  - [5, 9]
  - [10, 14]
  # ...
  - [45, 49]
  - [50, null]   # Open interval
```

### 3. Growth Rate Grid (`configs/rgrid.yml`)

Control the profiling of growth rate `r`:

```yaml
default:
  r_min: -0.03        # Lower bound (decline)
  r_max: 0.06         # Upper bound (rapid growth)
  r_step: 0.001       # Coarse grid step
  refine_window: 0.005  # Local refinement window
  refine_step: 0.0002   # Fine grid step
```

### 4. QC Thresholds (`configs/qc.yml`)

Set quality control flags:

```yaml
thresholds:
  whipple_flag: 125    # Whipple index threshold
  myers_flag: 40       # Myers blended index threshold
  asai_inspect: 20     # Age-Sex Accuracy Index (inspect)
  asai_flag: 40        # ASAI (flag for smoothing)
actions:
  smoothing_methods: ["arriaga", "cf"]  # Arriaga, Carrier-Farrag
  allow_pclm: true     # Enable PCLM graduation
```

---

## Data Preparation

### Observed Datasets

**Expected CSV format** (example for `Oaxaca1777.csv`):

```csv
Name,Year,AgeGroup,Total,Male,Female
Oaxaca,1777,0_4,7368,3860,3508
Oaxaca,1777,5_9,7601,3968,3633
...
Oaxaca,1777,90_up,18,11,7
```

**Key requirements**:

- Age groups as `0_4`, `5_9`, ..., `90_up` (or similar)
- Counts by sex (`Total`, `Male`, `Female`)
- Parser functions in `R/10_qc.R` handle label-to-numeric conversion

### Model Life Tables

Place UN 2011 and Coale-Demeny West tables in `data_raw/model_tables/`:

1. **UN 2011 abridged** (`unpd_2011_mlt_130_1y_abridged.csv`):

   - Columns: `Family`, `Level`, `Sex`, `x` (age), `n` (width), `Lxn`, `lxn`, `qxn`, `mxn`
2. **UN 2011 single-year** (`MLT_UN2011_130_1y_complete.csv`):

   - Columns: `Family`, `Level`, `Sex`, `Age`, `Lx1`, `lx1`, `qx1`, `mx1`
3. **Coale-Demeny West** (`WestLifeTables.csv`):

   - Columns: `family`, `level`, `sex`, `age_lower`, `age_upper`, `Lx`

**Sources**:

- UN 2011 tables: [UN Population Division](https://population.un.org/wpp/Download/Standard/Mortality/)
- Coale-Demeny: Historical demography literature or `demography` R package exports

---

## Running the Pipeline

### Basic Execution

From the repository root in R:

```r
library(targets)
tar_make()
```

This will:

1. Load configurations
2. Parse model life tables
3. Project models to dataset-specific age schemas
4. Read and QC observed data
5. Screen all models (profile `r`, compute AIC, distances)
6. Rank and select finalists
7. Generate reports in `reports/`

### Parallel Execution

Enable parallel processing (recommended for large model sets):

```r
library(targets)
library(future)
plan(multisession, workers = 4)  # Adjust workers to your CPU cores
tar_make()
```

### Inspect Pipeline

```r
tar_visnetwork()  # Visualize dependency graph
tar_outdated()    # Check which targets need updating
tar_read(screen_all)  # Load a specific target into memory
```

### Incremental Updates

The `{targets}` framework caches results. If you modify only one dataset or config, only affected targets rerun:

```r
# Edit configs/datasets.yml, then:
tar_make()  # Only new/changed datasets are processed
```

---

## Outputs

### 1. Screening Tables (`reports/screening/`)

HTML tables per unit (geography × sex × dataset) showing:

- **Model ID** (source level:sex)
- **`r_hat`**: Estimated growth rate
- **AIC**, **AIC weight**: Model ranking
- **G²**, **X²**: Deviance and Pearson chi-square
- **Hellinger**, **TV**, **Wasserstein**: Distance metrics

**Interpretation**: Models with highest AIC weights are best-fitting. Examine residuals and distances for substantive fit.

### 2. Finalist Reports (`reports/finalists/`)

Deep-dive diagnostics for top 5 models + Coale-Demeny West references:

- **Residual plots**: Pearson residuals by age bin
- **Profile likelihood**: `r` curve with 95% CI
- **Observed vs. expected**: Age pyramid comparisons
- **Robustness checks**: Bootstrap p-values for sparse bins

### 3. QC Summaries (`reports/qc/`)

Age-heaping indices (Whipple, Myers), Age-Sex Accuracy Index, and smoothing notes.

---

## Extending the Pipeline

### Adding New Datasets

1. Place CSV in `data_raw/` (e.g., `data_raw/16th_century/Puebla1568.csv`)
2. Define schema in `configs/schemas/schema_16c_coarse.yml` if needed
3. Register in `configs/datasets.yml`:
   ```yaml
   - dataset_id: PUEBLA1568
     path: data_raw/16th_century/Puebla1568.csv
     schema_id: schema_16c_coarse
     reader: puebla16c  # Implement parser in R/10_qc.R
     sex_mode: total
     geography: Puebla
     year: 1568
   ```
4. Add parser function to `R/10_qc.R`:
   ```r
   if(reader == "puebla16c") {
     # Custom parsing logic
   }
   ```
5. Run `tar_make()`

### Custom Age Schemas

Create `configs/schemas/schema_custom.yml`:

```yaml
schema_id: schema_custom
label: "Custom binning"
bins:
  - [0, 9]
  - [10, 19]
  - [20, 39]
  - [40, null]
```

The projector (`R/35_projector.R`) automatically aggregates model `Lx` to match.

### Alternative Model Life Tables

Add parsers to `R/30_models.R` following the pattern of `read_un_abridged()` and `read_cd_west()`. Ensure output schema includes:

- `source`, `family`, `level`, `sex`
- `age_lower`, `age_upper`, `Lx`
- `model_id` (unique identifier)

---

## Methodology References

### Demographic Methods

- **UN Model Life Tables (2011)**: [UN Population Division](https://population.un.org/wpp/)
- **DemoTools**: Riffe et al. (2019). [GitHub](https://github.com/timriffe/DemoTools)
- **PCLM (Penalized Composite Link Model)**: Rizzi et al. (2015). *Statistical Methods in Medical Research*. [ungroup package](https://github.com/mpascariu/ungroup)
- **Log-Quadratic Model**: Wilmoth et al. (2012). [MortCast package](https://CRAN.R-project.org/package=MortCast)

### Statistical Framework

- **Multinomial likelihood**: Standard for grouped count data
- **AIC weights**: Burnham & Anderson (2002). *Model Selection and Multimodel Inference*
- **Deviance (G²) and Pearson (X²)**: Cressie & Read (1984). *Journal of the Royal Statistical Society*
- **Bootstrap p-values**: For sparse bins, following Davison & Hinkley (1997)

### Historical Demography

- **Stable population theory**: Coale (1972). *The Growth and Structure of Human Populations*
- **Model life table usage**: IUSSP Tools for Demographic Estimation ([MORTPAK manual](https://www.un.org/development/desa/pd/data/mortpak))
- **Age heaping**: Whipple (1919), Myers (1940), UN protocols

---

## Troubleshooting

### Package Installation Issues

**DemoTools on Linux**:

```bash
# Ubuntu/Debian
sudo apt-get install libgsl-dev

# Then in R:
install.packages("DemoTools")
```

**MortCast dependencies**:

```r
install.packages("wpp2019")  # Required for LogQuad
```

### Data Format Errors

If `tar_make()` fails with "column not found":

1. Check CSV column names match parser expectations in `R/10_qc.R`
2. Verify `schema_id` in `datasets.yml` matches a file in `configs/schemas/`
3. Inspect raw CSV with `read_csv_safely("path/to/file.csv")`

### Memory Issues (Large Model Sets)

Reduce parallel workers or process datasets sequentially:

```r
plan(sequential)  # Disable parallelism
tar_make()
```

### Debugging Targets

```r
tar_load(obs_raw)      # Load intermediate target
tar_meta()             # View all target metadata
tar_destroy()          # Clear cache (use with caution)
```

---

## Citation

If you use this pipeline in published research, please cite:

> Cesaretti, R. (2025). *Model Life Tables for Colonial Mexico: A Reproducible Demographic Analysis Pipeline*. GitHub repository: https://github.com/rcesaret/model-life-tables-colonial-mexico

And cite the underlying methods:

- UN (2011). *Model Life Tables for Developing Countries*
- Riffe, T. et al. (2019). DemoTools R package
- Rizzi, S. et al. (2015). PCLM method in *Statistical Methods in Medical Research*

---

## License

[Specify license, e.g., MIT, GPL-3, CC-BY-4.0]

---

## Contact

**Rudolf Cesaretti**
[Email/Institution]
[ORCID/ResearchGate/Website]

For issues or feature requests, open an issue on [GitHub](https://github.com/rcesaret/model-life-tables-colonial-mexico/issues).

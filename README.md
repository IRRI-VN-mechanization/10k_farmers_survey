# Drivers of rice yield, profit and N-use efficiency in the Mekong River Delta - analysis code

This repository reproduces all statistical results, tables and figures of the paper from the cleaned
farmer-survey data (~10,000 farmers; WS 2022–23, SA 2023, AW 2023).

## 1. Folder layout

```
farm_sys_paper_code/
├── run_all.R                  # runs everything in order
├── farm_sys_paper.Rproj       # open this in RStudio (sets the project root)
├── R/
│   ├── config.R               # paths, seeds, variable lists, run switches
│   ├── packages.R             # all packages, loaded once
│   └── utils.R                # functions used by the scripts
├── scripts/
│   ├── 01_data_prep.R         # recoding (no cleaning), district names, deposit dataset
│   ├── 02_lmm.R               # weighted mixed models
│   ├── 03_descriptives.R      # sample sizes, farmers' practice, EMMs, correlations, crop calendar
│   ├── 04_random_forest.R     # RF + SHAP, pooled and per season  (slow)
│   ├── 05_shap_figures.R      # Fig. 6 and SHAP/PDP supplementary figures
│   ├── 06_optimization.R      # RF + NSGA-II 
│   ├── 07_district_clusters.R # Cluster maps and cluster profiles
│   ├── 08_1m5r_compliance.R   # 1M5R benchmark comparison 
├── exploratory/               # code removed from the main analysis (not used in the paper)
├── data/                      # 
└── outputs/                   # created automatically
    ├── processed/  models/  tables/
    └── figures/{model, descriptive, paper}/
```

## 2. Input data

| File | Put it in | Notes |
|---|---|---|
| `cleaned_data.csv` | `data/cleaned_data/` | already-cleaned survey data, one row per farmer-season; script 01 only recodes categories and units |
| `mrd_commune_boundary.shp` (+ .shx .dbf .prj .cpg) | `data/location_data/` | adds district/commune names via `admin_code` |
| `mrd_district.shp`, `mrd_province.shp` (+ parts) | `data/location_data/` | maps (scripts 07, 08) |

A de-identified analysis file (`outputs/processed/analysis_dataset_for_deposit.csv`) is written by
script 01 for the public data repository.

## 3. How to run

```r
# once (renv.lock is created with renv::snapshot(); see CODE_REVIEW.md, D3)
install.packages("renv"); renv::restore()     # installs the package versions in renv.lock
# every time
source("run_all.R")
```

Requires R ≥ 4.2 (UTF-8 source files). Each script can also be run on its own (they read their inputs from `outputs/`), but run them
in number order the first time.

**Run time.** Scripts 04 (12 random forests with SHAP, nsim = 500) and 06 (4 NSGA-II runs) take
several hours. By default they *reuse* saved results in `outputs/models/`; set
`REFIT_RF <- TRUE` / `RERUN_OPT <- TRUE` in `R/config.R` to recompute.

**Reproducibility.** All random steps use `SEED = 42` (train/test split, caret CV, SHAP, NSGA-II;
k-means of districts uses seed 123 as in the original). `outputs/sessionInfo.txt` records package versions.

## 4. Where each paper item comes from

| Paper item | Script | Output file |
|---|---|---|
| Sample sizes, farmers' current practice | 03 | `tables/descriptive_tables.xlsx` |
| LMM ANOVA | 02 | `tables/Table4_lmm_anova.docx/.xlsx` |
| LMM coefficients| 02 | `tables/Table5_lmm_coefficients.docx/.xlsx` |
| SI Tables VIF, model comparison | 02 | `tables/SI_lmm_vif_and_model_comparison.xlsx` |
| Crop calendar, drawn by hand — source data | 03 | `descriptive_tables.xlsx`, sheet `crop_calendar` |
| District clusters | 07 | `figures/paper/cluster_p.png`, `tables/Fig5_district_cluster_profiles.xlsx` |
| SHAP importance | 05 | `figures/paper/i1.png` |
| RF performance (CV and test R², RMSE) | 04 | `tables/rf_performance.xlsx` |
| Pareto front, clusters | 06 | `figures/paper/op_i1.png` |
| % change vs practice | 06 | `tables/Table6_pareto_clusters.docx`, `Table6_and_SI_optimization_summary.xlsx` |
| SI Tables season optimization | 06 | `tables/Table6_pareto_clusters_ws/sa/aw.docx` |
| SI SHAP dependence, 2D PDP, season SHAP | 05 | `figures/paper/a1_*.png`, `a2_*.png`, `a3.png`, `i2.png` |
| SI N–P–K–seed correlations | 03 | `figures/descriptive/fertilizer_cor.png` |
| SI 1M5R benchmarks | 08 | `figures/paper/1M5R_non_compliance_grid_map.png`, `tables/SI_1m5r_noncompliance.xlsx` |

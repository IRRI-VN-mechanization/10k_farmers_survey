# =============================================================================
# config.R — paths, seeds and analysis settings shared by all scripts
# -----------------------------------------------------------------------------
# All paths are RELATIVE to the project root (the folder containing
# run_all.R). Open the project in RStudio (farm_sys_paper.Rproj) or start R in
# =============================================================================

suppressPackageStartupMessages(library(here))

# ---- Input data ---------------------------
PATHS <- list(
  cleaned_data   = here("data", "cleaned_data", "cleaned_data.csv"),
  commune_shp    = here("data", "location_data", "mrd_commune_boundary.shp"),
  district_shp   = here("data", "location_data", "mrd_district.shp"),
  province_shp   = here("data", "location_data", "mrd_province.shp"),
  fertilizer_raw = here("data", "raw_data", "10kha-vietnam-fertilizer.xlsx"),

  # ---- Outputs (created automatically) ---------------------------------------
  processed   = here("outputs", "processed"),
  models      = here("outputs", "models"),
  tables      = here("outputs", "tables"),
  fig_model   = here("outputs", "figures", "model"),
  fig_desc    = here("outputs", "figures", "descriptive"),
  fig_paper   = here("outputs", "figures", "paper")
)

for (p in PATHS[c("processed", "models", "tables", "fig_model", "fig_desc", "fig_paper")]) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

out <- function(key, file) file.path(PATHS[[key]], file)   # helper: out("tables", "x.docx")

# ---- Run switches ------------------------------------------------------------
# Random forests (~12 models, SHAP nsim = 500) and NSGA-II are slow.
# FALSE = reuse saved .rds files in outputs/models if they exist.
REFIT_RF  <- FALSE
RERUN_OPT <- FALSE

# ---- Reproducibility settings -----------------------------------------------
SEED       <- 42      # used for train/test split, caret CV, SHAP, NSGA-II
N_SIM_SHAP <- 500     # Monte-Carlo repetitions in fastshap::explain()
SEASONS    <- c("WS", "SA", "AW")

# ---- Mixed models ----------------------------------------------
LMM_FIXED  <- c("N_amt", "P2O5_amt", "K2O_amt", "sowing_method", "farming_system",
                "seed_rate", "soil_type", "crop_establisment_time", "harvest_time")
LMM_RANDOM <- c("province", "season", "rice_variety")
LMM_SCALE  <- c("N_amt", "P2O5_amt", "K2O_amt", "crop_establisment_time",
                "harvest_time", "seed_rate")

# ---- Random forests  ----------------------------------
RF_X      <- c("N_amt", "P2O5_amt", "K2O_amt", "sowing_method", "farming_system",
               "crop_establisment_time", "harvest_time", "seed_rate", "soil_type",
               "province", "season", "rice_variety")
RF_X_PFPN <- setdiff(RF_X, "N_amt")   # PFPN model for SHAP excludes N (PFPN = yield / N)

RF_YIELD_VAR <- "yield_ha"

# ---- Optimization  -----------------------------------------
OPT_VARS        <- c("N_amt", "P2O5_amt", "K2O_amt", "seed_rate")
OPT_BOUNDS_PROB <- c(0.025, 0.975)
OPT_PFPN_RANGE  <- c(40, 80)
NSGA2 <- list(popsize = 96, generations = 150, cprob = 0.9, cdist = 20,
              mprob = 1 / 4, mdist = 20)

# ---- District clustering (Fig. 5) -------------------------------------------
DISTRICT_K <- 5

# ---- Labels -----------------------------------------------------------------
VAR_MAPPING <- c(
  "N_amt" = "Nitrogen amount", "P2O5_amt" = "Phosphorus amount",
  "K2O_amt" = "Potassium amount", "sowing_method" = "Sowing method",
  "Sowing methodprecision seeding" = "Precision seeding",
  "farming_system" = "Farming system",
  "Farming systemsingle rice" = "Farming system (Single rice)",
  "Farming systemtriple rice" = "Farming system (Triple rice)",
  "crop_establisment_time" = "Crop establishment time",
  "harvest_time" = "Harvesting time", "seed_rate" = "Seed rate",
  "soil_type" = "Soil type",
  "Soil typealluvial" = "Soil type (Alluvial)", "Soil typegrey" = "Soil type (Grey)",
  "Soil typeother" = "Soil type (Other)", "Soil typesalinity" = "Soil type (Salinity)",
  "province" = "Province", "rice_variety" = "Rice variety", "season" = "Season"
)

PROVINCE_ABBR <- c(
  "Thành phố Cần Thơ" = "CT", "Tỉnh An Giang" = "AG", "Tỉnh Bạc Liêu" = "BL",
  "Tỉnh Bến Tre" = "BT", "Tỉnh Cà Mau" = "CM", "Tỉnh Đồng Tháp" = "DT",
  "Tỉnh Hậu Giang" = "HG", "Tỉnh Kiên Giang" = "KG", "Tỉnh Long An" = "LA",
  "Tỉnh Sóc Trăng" = "ST", "Tỉnh Tiền Giang" = "TG", "Tỉnh Trà Vinh" = "TV",
  "Tỉnh Vĩnh Long" = "VL"
)

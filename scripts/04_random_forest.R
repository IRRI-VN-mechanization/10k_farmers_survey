# =============================================================================
# 04_random_forest.R — fit RF models (pooled and per season) + SHAP values
# Paper: SHAP dependence / PDP figures (SI), surrogate models
# Output: outputs/models/{yield,profit,pfpn}_model/rf_*.rds (13 models), outputs/tables/rf_performance.xlsx
# Runtime: several hours (SHAP nsim = 500). Set REFIT_RF <- FALSE in config.R to reuse.
# =============================================================================

source(here::here("R", "config.R")); source(here::here("R", "packages.R")); source(here::here("R", "utils.R"))
df_model <- readRDS(out("processed", "df_model.rds"))

# The 13 models of the paper:
#   pooled : rf_yield, rf_profit, rf_pfpn_withoutfer  -> SHAP (Fig. 6)
#            rf_pfpn (WITH N)                          -> PFPN 40–80 constraint in the optimization
#   per season (ws, sa, aw): rf_yield_<s>, rf_profit_<s>, rf_pfpn_withoutfer_<s> -> SI SHAP figures,
#            season-specific optimization
RF_SPECS <- list(
  yield           = list(y = RF_YIELD_VAR, x = RF_X),
  profit          = list(y = "net_income", x = RF_X),
  pfpn_withoutfer = list(y = "PFPN",       x = RF_X_PFPN),   # N excluded: PFPN = yield / N
  pfpn            = list(y = "PFPN",       x = RF_X)         # pooled only
)
RF_RUNS <- rbind(
  data.frame(resp = c("yield", "profit", "pfpn_withoutfer", "pfpn"), ds = "all"),
  expand.grid(resp = c("yield", "profit", "pfpn_withoutfer"), ds = tolower(SEASONS),
              stringsAsFactors = FALSE))

DATASETS <- c(list(all = df_model),
              setNames(lapply(SEASONS, function(s) filter(df_model, season == s)), tolower(SEASONS)))

run_name <- function(resp, ds) if (ds == "all") resp else paste0(resp, "_", ds)

for (i in seq_len(nrow(RF_RUNS))) {
  resp <- RF_RUNS$resp[i]; ds <- RF_RUNS$ds[i]
  f <- rf_path(run_name(resp, ds))
  if (file.exists(f) && !REFIT_RF) { message("skip (exists): ", basename(f)); next }
  message("RF ", i, "/", nrow(RF_RUNS), ": ", run_name(resp, ds))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  res <- run_rf_analysis(df = DATASETS[[ds]], y_var = RF_SPECS[[resp]]$y,
                         x_vars = RF_SPECS[[resp]]$x, random_state = SEED, nsim = N_SIM_SHAP)
  saveRDS(res, f)
}

# ---- Model performance (report in Results/SI) -------------------------------
perf <- bind_rows(lapply(seq_len(nrow(RF_RUNS)), function(i) {
  f <- rf_path(run_name(RF_RUNS$resp[i], RF_RUNS$ds[i]))
  if (!file.exists(f)) return(NULL)
  cbind(dataset = RF_RUNS$ds[i], rf_metrics(readRDS(f), RF_RUNS$resp[i]))
}))
writexl::write_xlsx(perf, out("tables", "rf_performance.xlsx"))
print(perf)
message("04 done")

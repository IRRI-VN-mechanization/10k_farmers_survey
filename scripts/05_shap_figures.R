# =============================================================================
# 05_shap_figures.R — SHAP importance, dependence plots, 2D PDPs, combined figures
# Paper: SHAP importance plot, Supplementary figures
# =============================================================================

source(here::here("R", "config.R")); source(here::here("R", "packages.R")); source(here::here("R", "utils.R"))
rf <- function(name) readRDS(out("models", paste0("rf_", name, ".rds")))

models <- list(yield = rf("yield"), pfpn = rf("pfpn_withoutfer"), profit = rf("profit"))
FIG <- PATHS$fig_model

# ---- Fig. 6: SHAP importance (pooled) ----------------------------------------
save_shap_plot(models$yield$shap_values_full,  "Yield determinants (SHAP values)",  "shap_yield.png", VAR_MAPPING, FIG)
save_shap_plot(models$pfpn$shap_values_full,   "PFPN determinants (SHAP values)",   "shap_pfpn.png",  VAR_MAPPING, FIG)
save_shap_plot(models$profit$shap_values_full, "Profit determinants (SHAP values)", "shap_pf.png",    VAR_MAPPING, FIG)
combine_img(FIG, PATHS$fig_paper, c("shap_yield", "shap_pfpn", "shap_pf"), ncol = 3, output_name = "i1.png")

# ---- SI: SHAP importance per season (season column dropped: constant) --------
for (s in tolower(SEASONS)) {
  m <- list(yield = rf(paste0("yield_", s)), pfpn = rf(paste0("pfpn_withoutfer_", s)),
            pf = rf(paste0("profit_", s)))
  for (nm in names(m)) {
    sh <- m[[nm]]$shap_values_full
    keep <- setdiff(colnames(sh), "season")
    lab <- c(yield = "yield", pfpn = "PFPN", pf = "profit")[[nm]]
    save_shap_plot(sh[, keep], sprintf("[%s] %s determinants (SHAP values)", toupper(s), lab),
                   sprintf("shap_%s_%s.png", nm, s), VAR_MAPPING, FIG)
  }
  combine_img(FIG, PATHS$fig_paper, paste0(c("shap_yield_", "shap_pfpn_", "shap_pf_"), s),
              ncol = 3, output_name = sprintf("a2_%d.png", match(toupper(s), SEASONS)))
}

# ---- SI: SHAP dependence plots (pooled models) --------------------------------
features      <- c("N_amt", "P2O5_amt", "K2O_amt", "sowing_method", "farming_system",
                   "seed_rate", "soil_type", "crop_establisment_time", "harvest_time")
features_pfpn <- setdiff(features, "N_amt")

for (f in features) {
  plot_shapviz_dependency(models$yield$shap_values_full, f, color_col = NULL, type = "yield",
                          trim_outliers = TRUE, file_name = paste0("dp_yield_", f, ".png"), save_dir = FIG)
  plot_shapviz_dependency(models$profit$shap_values_full, f, color_col = NULL, type = "profit",
                          trim_outliers = TRUE, file_name = paste0("dp_profit_", f, ".png"), save_dir = FIG)
}
for (f in features_pfpn) {
  plot_shapviz_dependency(models$pfpn$shap_values_full, f, color_col = NULL, type = "PFPN",
                          trim_outliers = TRUE, file_name = paste0("dp_pfpn_", f, ".png"), save_dir = FIG)
}

# ---- SI: 2D partial dependence, seed rate x N / P / K (yield model) ------------
for (fert in c("N_amt", "P2O5_amt", "K2O_amt")) {
  save_pdp_2d_trimmed(c("seed_rate", fert), models$yield$model_results$caret_object,
                      models$yield$data$X_full_processed, models$yield$data$y_full, FIG)
}

# ---- Combined figures ------------------------------------------------------
combine_img(FIG, PATHS$fig_paper, c("dp_yield_N_amt", "dp_yield_P2O5_amt", "dp_yield_K2O_amt",
                                    "dp_yield_seed_rate"), ncol = 2, output_name = "i2.png")
# CHANGED: the original made a1_1.png twice (the first version was overwritten); the kept one:
combine_img(FIG, PATHS$fig_paper, c("dp_yield_N_amt", "dp_profit_N_amt", "dp_yield_P2O5_amt",
                                    "dp_profit_P2O5_amt", "dp_yield_K2O_amt", "dp_profit_K2O_amt"),
            ncol = 2, output_name = "a1_1.png")
combine_img(FIG, PATHS$fig_paper, c("dp_yield_seed_rate", "dp_pfpn_seed_rate", "dp_profit_seed_rate",
                                    "dp_yield_soil_type", "dp_pfpn_soil_type", "dp_profit_soil_type"),
            ncol = 2, output_name = "a1_2.png")
combine_img(FIG, PATHS$fig_paper, c("dp_yield_crop_establisment_time", "dp_pfpn_crop_establisment_time",
                                    "dp_profit_crop_establisment_time", "dp_yield_harvest_time",
                                    "dp_pfpn_harvest_time", "dp_profit_harvest_time"),
            ncol = 2, output_name = "a1_3.png")
combine_img(FIG, PATHS$fig_paper, c("dp_yield_sowing_method", "dp_pfpn_sowing_method", "dp_profit_sowing_method",
                                    "dp_yield_farming_system", "dp_pfpn_farming_system", "dp_profit_farming_system"),
            ncol = 2, output_name = "a1_4.png")
combine_img(FIG, PATHS$fig_paper, c("pdp_2d_[PDP]_seed_rate_N_amt", "pdp_2d_[PDP]_seed_rate_K2O_amt",
                                    "pdp_2d_[PDP]_seed_rate_P2O5_amt"), ncol = 3, output_name = "a3.png")
message("05 done")

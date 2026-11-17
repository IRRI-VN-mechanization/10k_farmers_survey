# =============================================================================
# 06_optimization.R — surrogate multi-objective optimization (RF + NSGA-II)
# Runs the SAME procedure 4 times: all seasons pooled, then WS, SA, AW.
# Output (same names as the original project):
#   outputs/optimization/optimization.rds / .xlsx          (pooled)
#   outputs/optimization/optimization_ws|sa|aw.rds / .xlsx (per season)
# a run is skipped when its .xlsx exists (unless RERUN_OPT <- TRUE in config.R).
# =============================================================================

source(here::here("R", "config.R")); source(here::here("R", "packages.R")); source(here::here("R", "utils.R"))
df_cleaned       <- readRDS(out("processed", "df_cleaned.rds"))
current_practice <- readRDS(out("processed", "current_practice.rds"))
rf_model <- function(name) readRDS(rf_path(name))$model_results$model

OPT_DIR <- here::here("outputs", "optimization")
dir.create(OPT_DIR, recursive = TRUE, showWarnings = FALSE)

# Data on which candidate inputs are evaluated:
# pooled = all rows of df_cleaned; season runs = that season's rows only.
# Bounds (2.5–97.5% quantiles) are therefore computed within each dataset.
DATASETS <- c(list(all = df_cleaned),
              setNames(lapply(SEASONS, function(s) filter(df_cleaned, season == s)), tolower(SEASONS)))

# Surrogate models used in each run:
#   yield / profit : pooled run -> rf_yield, rf_profit
#                    season run -> rf_yield_<s>, rf_profit_<s>   (SEASON_OPT_MODELS = "seasonal")
#                                  or the pooled models          (SEASON_OPT_MODELS = "pooled")
#   PFPN constraint: rf_pfpn (pooled, WITH N) in every run — the season PFPN models
#                    exclude N and therefore cannot respond to the N decision variable.
SEASON_OPT_MODELS <- "seasonal"   # <- set to "pooled" if that is what you used

opt_models <- function(ds) {
  sfx <- if (ds == "all" || SEASON_OPT_MODELS == "pooled") "" else paste0("_", ds)
  list(rf_yield  = rf_model(paste0("yield", sfx)),
       rf_profit = rf_model(paste0("profit", sfx)),
       rf_pfpn   = rf_model("pfpn"))
}

# Normalised hypervolume of a saved nsga2 result (convergence diagnostic)
hypervolume <- function(res) {
  pf <- mco::paretoFront(res)
  pf_norm <- apply(pf, 2, function(v) (v - min(v)) / (max(v) - min(v)))
  mco::dominatedHypervolume(pf_norm, ref = c(1.1, 1.1))
}

pareto <- list(); diag_rows <- list()
for (ds in names(DATASETS)) {
  sfx  <- if (ds == "all") "" else paste0("_", ds)
  xlsx <- file.path(OPT_DIR, paste0("optimization", sfx, ".xlsx"))
  rds  <- file.path(OPT_DIR, paste0("optimization", sfx, ".rds"))

  if (file.exists(xlsx) && !RERUN_OPT) {
    message("reuse: ", basename(xlsx))
    pareto[[ds]] <- as.data.frame(readxl::read_excel(xlsx))
  } else {
    message("NSGA-II (", ds, "): ", NSGA2$popsize, " x ", NSGA2$generations + 1, " evaluations ...")
    m <- opt_models(ds)
    o <- run_optimization(DATASETS[[ds]], rf_yield = m$rf_yield, rf_profit = m$rf_profit,
                          rf_pfpn = m$rf_pfpn)
    saveRDS(o$nsga2, rds)                 # same object as the original optimization.rds
    writexl::write_xlsx(o$pareto_eval, xlsx)
    pareto[[ds]] <- o$pareto_eval
  }

  b <- sapply(DATASETS[[ds]][, OPT_VARS], quantile, probs = OPT_BOUNDS_PROB, na.rm = TRUE)
  diag_rows[[ds]] <- data.frame(
    dataset = ds, n_rows_evaluated = nrow(DATASETS[[ds]]),
    n_pareto_solutions = nrow(pareto[[ds]]),
    hypervolume_normalised = if (file.exists(rds)) hypervolume(readRDS(rds)) else NA,
    t(setNames(c(floor(b[1, ]), ceiling(b[2, ])),
               c(paste0(OPT_VARS, "_lower"), paste0(OPT_VARS, "_upper")))))
}
diag_tab <- bind_rows(diag_rows)

# ---- Fig. 7: Pareto front + regression fit + clusters ---------------------------
FIG <- PATHS$fig_desc
clusters <- list(); regressions <- list()
for (ds in names(pareto)) {
  pe <- pareto[[ds]]
  sfx <- if (ds == "all") "" else paste0("_", ds)
  lab <- if (ds == "all") "" else paste0("[", toupper(ds), "] ")

  plot_pareto(pe, title = paste0(lab, "Pareto trade-off between rice yield and profit"),
              legend_position = "bottom", out_dir = FIG, file_name = paste0("op_front", sfx, ".png"))

  # Pooled front: k chosen by silhouette; per season: k = 3 fixed (as in the original)
  clusters[[ds]] <- run_pareto_pipeline(pe, docx_path = out("tables", paste0("Table6_pareto_clusters", sfx, ".docx")),
                                        k = if (ds == "all") NULL else 3, seed = SEED)
  plot_pareto_clusters(clusters[[ds]]$data, output_path = file.path(FIG, paste0("op_i6", sfx, ".png")))
  regressions[[ds]] <- run_pareto_regression_pipeline(pe, output_path = file.path(FIG, paste0("op_fit", sfx, ".png")))
}

# Front coloured by each decision variable (Supplementary)
pe <- pareto$all
for (v in list(c("N_amt", "op_i2", "N"), c("P2O5_amt", "op_i3", "P2O5"),
               c("K2O_amt", "op_i4", "K2O"), c("seed_rate", "op_i5", "seed rate"))) {
  plot_pareto(pe, color_var = v[1], color_label = "kg/ha",
              y_label = "Mean predicted profit (million VND ha⁻¹)",
              x_label = expression("Mean predicted yield (t ha"^-1*")"),
              title = paste0("Pareto front by ", v[3], " application rate (kg ha⁻¹)"),
              out_dir = FIG, file_name = paste0(v[2], ".png"))
}
# CHANGED: renamed — the original saved this as op_i1.png and then overwrote it with Fig. 7
combine_img(FIG, PATHS$fig_paper, c("op_i2", "op_i3", "op_i4", "op_i5"), ncol = 2,
            output_name = "SI_pareto_by_inputs.png")
combine_img(FIG, PATHS$fig_paper, c("op_fit", "op_i6"), ncol = 2, output_name = "op_i1.png")   # Fig. 7

# ---- Table 6 + % change vs farmers' practice (numbers quoted in Discussion) ------
comparison <- bind_rows(lapply(names(clusters), function(ds) {
  pr <- filter(current_practice, season == if (ds == "all") "All" else toupper(ds))
  cbind(dataset = ds, compare_to_practice(clusters[[ds]]$summary, pr))
}))

reg_tab <- bind_rows(lapply(names(regressions), function(ds)
  cbind(dataset = ds, regressions[[ds]]$fit_table)))

writexl::write_xlsx(list(clusters_vs_practice = comparison, current_practice = current_practice,
                         regression_fits = reg_tab, diagnostics = diag_tab,
                         silhouette_pooled = if (is.null(clusters$all$silhouette)) data.frame() else clusters$all$silhouette),
                    out("tables", "Table6_and_SI_optimization_summary.xlsx"))
message("06 done")

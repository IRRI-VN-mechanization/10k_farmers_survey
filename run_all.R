# =============================================================================
# run_all.R — reproduces every table and figure of the paper, in order
# Usage: open farm_sys_paper.Rproj (or setwd() to this folder) and run
#   source("run_all.R")
# Slow steps (04 random forests + SHAP, 06 NSGA-II) reuse saved results in
# outputs/models unless REFIT_RF / RERUN_OPT are TRUE in R/config.R.
# =============================================================================

steps <- c("01_data_prep.R", "02_lmm.R", "03_descriptives.R", "04_random_forest.R",
           "05_shap_figures.R", "06_optimization.R", "07_district_clusters.R",
           "08_1m5r_compliance.R")

for (s in steps) {
  message("\n==== ", s, " ====")
  t0 <- Sys.time()
  source(here::here("scripts", s), local = new.env())
  message(sprintf("finished in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

# Record the exact software versions used
writeLines(capture.output(sessionInfo()), here::here("outputs", "sessionInfo.txt"))
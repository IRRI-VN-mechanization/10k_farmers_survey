# =============================================================================
# packages.R — every package used by the analysis, loaded in one place
# -----------------------------------------------------------------------------
#   install.packages("renv"); renv::init()   # first time, on your computer
#   renv::snapshot()                          # writes renv.lock -> commit it
#   renv::restore()                           # what other people run
# =============================================================================

PKGS <- c(
  # data handling
  "here", "dplyr", "tidyr", "purrr", "stringr", "readxl", "writexl",
  # spatial
  "sf", "terra",
  # mixed models
  "nlme", "lme4", "performance", "emmeans", "broom.mixed", "parameters", "car",
  # machine learning + interpretation
  "caret", "ranger", "fastshap", "shapviz", "iml",
  # optimization + clustering
  "mco", "cluster",
  # tables + figures
  "flextable", "ggplot2", "viridis", "scales", "GGally", "patchwork", "magick"
)

missing <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Please install missing packages first:\n  install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(ggplot2)
  library(flextable); library(sf); library(nlme); library(performance)
  library(emmeans); library(caret); library(ranger); library(shapviz)
  library(iml); library(mco); library(writexl); library(readxl)
})

# dplyr::select / filter win over MASS / stats versions loaded by other packages
select <- dplyr::select
filter <- dplyr::filter

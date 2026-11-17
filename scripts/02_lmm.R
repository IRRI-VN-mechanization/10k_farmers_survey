# =============================================================================
# 02_lmm.R — weighted linear mixed models for yield, PFPN and profit
# Paper: Table LMM ANOVA, LMM coefficients, Supplementary Tables
# Output: outputs/models/lmm_final_models.rds, outputs/tables/*.docx / *.xlsx
# =============================================================================

source(here::here("R", "config.R")); source(here::here("R", "packages.R")); source(here::here("R", "utils.R"))
df_cleaned <- readRDS(out("processed", "df_cleaned.rds"))

# Each response: (1) homoscedastic lmer -> heteroscedasticity test,
# (2) nlme::lme with varPower and varExp, (3) pick the better model.
responses <- list(
  yield  = list(y = "yield_14",   fixed = LMM_FIXED),
  PFPN   = list(y = "PFPN",       fixed = setdiff(LMM_FIXED, "N_amt")),   # N excluded
  profit = list(y = "net_income", fixed = LMM_FIXED)
)

# Optimiser settings that achieved convergence in the original analysis
ctrl_default <- nlme::lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 800, opt = "nlminb")
ctrl_list <- list(
  yield  = list(varPower = ctrl_default, varExp = ctrl_default),
  PFPN   = list(varPower = nlme::lmeControl(maxIter = 1000, msMaxIter = 1000, msMaxEval = 1500,
                                            tolerance = 1e-6, msTol = 1e-7, opt = "nlminb",
                                            returnObject = TRUE),
                varExp   = ctrl_default),
  profit = list(varPower = ctrl_default,
                varExp   = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500))
)

# Final choice in the original analysis (by compare_performance): see Supplementary Table 2
FINAL_WEIGHTS <- c(yield = "varPower", PFPN = "varExp", profit = "varPower")

fits <- list(); comparisons <- list(); diagnostics <- list()
for (r in names(responses)) {
  message("LMM: ", r)
  spec <- responses[[r]]

  m0 <- fit_lmer(df_cleaned, spec$y, spec$fixed, LMM_RANDOM, LMM_SCALE)
  het0 <- performance::check_heteroscedasticity(m0)

  m_pow <- fit_lme(df_cleaned, spec$y, spec$fixed, LMM_RANDOM, LMM_SCALE,
                   weights_method = "varPower", lme_control = ctrl_list[[r]]$varPower)
  m_exp <- tryCatch(fit_lme(df_cleaned, spec$y, spec$fixed, LMM_RANDOM, LMM_SCALE,
                            weights_method = "varExp", lme_control = ctrl_list[[r]]$varExp),
                    error = function(e) { message("  varExp failed: ", conditionMessage(e)); NULL })

  comparisons[[r]] <- if (is.null(m_exp)) {
    data.frame(response = r, note = "varExp did not converge; varPower used")
  } else {
    cbind(response = r, as.data.frame(compare_performance(m_pow, m_exp, rank = TRUE)))
  }

  final <- if (FINAL_WEIGHTS[[r]] == "varPower") m_pow else m_exp
  fits[[r]] <- final

  res <- residuals(final)
  diagnostics[[r]] <- list(
    heteroscedasticity_unweighted = het0,
    heteroscedasticity_final      = performance::check_heteroscedasticity(final),
    collinearity                  = performance::check_collinearity(final),
    ks_normality                  = ks.test(res, "pnorm", mean = mean(res), sd = sd(res)),
    outliers                      = performance::check_outliers(final))
}
saveRDS(fits, out("models", "lmm_final_models.rds"))
saveRDS(diagnostics, out("models", "lmm_diagnostics.rds"))

# ---- Supplementary Table 1 (VIF) and Table 2 (model comparison) --------------
vif_tab <- bind_rows(lapply(names(diagnostics), function(r)
  cbind(response = r, as.data.frame(diagnostics[[r]]$collinearity))))
writexl::write_xlsx(list(VIF = vif_tab, model_comparison = bind_rows(comparisons)),
                    out("tables", "SI_lmm_vif_and_model_comparison.xlsx"))

# ---- Table 5: coefficients, random effects, model fit -----------------------
fe   <- lapply(fits, export_fixed_effects, fixed_map = VAR_MAPPING)
rand <- lapply(fits, export_random_effects_lme, data = df_cleaned, rand_map = VAR_MAPPING)
fit  <- lapply(fits, export_model_fit)

merged_fe <- fe %>% imap(~ rename_with(.x, function(n) paste(.y, n), -`Fixed effects`)) %>%
  reduce(full_join, by = "Fixed effects")
merged_rand <- rand %>% imap(~ setNames(.x, c("Random effects", paste(.y, "StDev")))) %>%
  reduce(full_join, by = "Random effects")
merged_fit <- fit %>% imap(~ setNames(.x, c("Model fit", paste(.y, "Value")))) %>%
  reduce(full_join, by = "Model fit")

writexl::write_xlsx(list(fixed_effects = merged_fe, random_effects = merged_rand,
                         model_fit = merged_fit),
                    out("tables", "Table5_lmm_coefficients.xlsx"))

ft5 <- flextable(merged_fe) %>% theme_booktabs() %>% bold(part = "header") %>%
  colformat_char(na_str = "-") %>% autofit()
ft5_r <- flextable(merged_rand) %>% theme_booktabs() %>% autofit()
ft5_f <- flextable(merged_fit) %>% theme_booktabs() %>% autofit()
save_as_docx(`Fixed effects` = ft5, `Random effects` = ft5_r, `Model fit` = ft5_f,
             path = out("tables", "Table5_lmm_coefficients.docx"))

# ---- Table 4: marginal (type III-equivalent) ANOVA --------------------------
aov <- lapply(fits, export_anova_lme, fixed_map = VAR_MAPPING) %>%
  imap(~ rename_with(.x, function(n) paste(.y, n), -`Fixed effects`)) %>%
  reduce(full_join, by = "Fixed effects")
writexl::write_xlsx(aov, out("tables", "Table4_lmm_anova.xlsx"))
save_as_docx(flextable(aov) %>% theme_booktabs() %>% colformat_char(na_str = "-") %>% autofit(),
             path = out("tables", "Table4_lmm_anova.docx"))

message("02 done")

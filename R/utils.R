# =============================================================================
# utils.R — functions used by the scripts in scripts/
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 1. Mixed models
# -----------------------------------------------------------------------------

# Prepare model data: keep variables, drop NA/Inf response, factors, z-scaling.
prep_model_data <- function(data, response, fixed_vars, random_vars, scale_vars) {
  all_vars <- unique(c(response, fixed_vars, random_vars, scale_vars))
  df_sub <- data[, all_vars, drop = FALSE] %>%
    filter(!is.na(.data[[response]]), is.finite(.data[[response]])) %>%
    na.omit()

  cols_to_factor <- unique(c(random_vars, setdiff(fixed_vars, scale_vars)))
  df_sub[cols_to_factor] <- lapply(df_sub[cols_to_factor], as.factor)

  if (!is.null(scale_vars)) {
    df_sub[scale_vars] <- lapply(df_sub[scale_vars],
                                 function(x) as.numeric(scale(as.numeric(as.character(x)))))
  }
  df_sub
}

# Homoscedastic LMM with CROSSED random intercepts (lme4).
# Used only to test assumptions before the weighted model.
fit_lmer <- function(data, response, fixed_vars, random_vars, scale_vars) {
  df_sub <- prep_model_data(data, response, fixed_vars, random_vars, scale_vars)
  f <- as.formula(paste(response, "~", paste(fixed_vars, collapse = " + "), "+",
                        paste0("(1 | ", random_vars, ")", collapse = " + ")))
  lme4::lmer(f, data = df_sub, REML = TRUE)
}

# Weighted LMM (nlme)
fit_lme <- function(data, response, fixed_vars, random_vars, scale_vars,
                    weights_method = "varPower",
                    lme_control = nlme::lmeControl(maxIter = 500, msMaxIter = 500,
                                                   msMaxEval = 800, opt = "nlminb")) {
  df_sub <- prep_model_data(data, response, fixed_vars, random_vars, scale_vars)

  w_func <- switch(weights_method,
                   "varPower"      = nlme::varPower(form = ~ fitted(.)),
                   "varExp"        = nlme::varExp(form = ~ fitted(.)),
                   "varConstPower" = nlme::varConstPower(form = ~ fitted(.)),
                   stop("weights_method must be varPower, varExp or varConstPower"))

  fixed_formula  <- as.formula(paste(response, "~", paste(fixed_vars, collapse = " + ")))
  random_formula <- as.formula(paste("~ 1 |", paste(random_vars, collapse = "/")))
  environment(fixed_formula) <- environment(random_formula) <- environment()

  model <- nlme::lme(fixed = fixed_formula, random = random_formula, data = df_sub,
                     weights = w_func, control = lme_control, method = "REML")
  model$call$fixed   <- fixed_formula
  model$call$random  <- random_formula
  model$call$control <- lme_control
  model
}

stars <- function(p) dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                                      p < 0.05 ~ "*", TRUE ~ "")


export_fixed_effects <- function(model, fixed_map = NULL) {
  broom.mixed::tidy(model, effects = "fixed") %>%
    mutate(term = if (!is.null(fixed_map)) stringr::str_replace_all(term, fixed_map) else term,
           Estimate = paste0(format(round(estimate, 3), nsmall = 3), stars(p.value)),
           SE = format(round(std.error, 3), nsmall = 3),
           df = if ("df" %in% names(.)) format(round(df, 2), nsmall = 2) else "N/A",
           P = format.pval(p.value, digits = 2, eps = 1e-16)) %>%
    select(`Fixed effects` = term, Estimate, SE, df, P)
}

export_random_effects_lme <- function(model, data, rand_map = NULL) {
  random_tab <- as.data.frame(parameters::model_parameters(model, effects = "random"))
  groups <- unique(random_tab$Group[random_tab$Group != "Residual"])
  # NOTE: n = distinct values of the column in the full data. With the nested
  # structure the number of fitted groups is larger (e.g. province x season).
  n_tab <- data.frame(Group = groups,
                      n = vapply(groups, function(g) dplyr::n_distinct(data[[g]]), numeric(1)))
  random_tab %>%
    left_join(n_tab, by = "Group") %>%
    mutate(Group_mapped = ifelse(Group %in% names(rand_map), rand_map[Group], Group),
           `Random effects` = ifelse(Group == "Residual", Group_mapped,
                                     paste0(Group_mapped, " (n=", n, ")")),
           StDev = round(Coefficient, 3)) %>%
    select(`Random effects`, StDev)
}

export_model_fit <- function(model) {
  perf <- performance::model_performance(model)
  data.frame(`Model fit` = c("Marginal R² (Fixed Effects)", "Conditional R² (Full Model)"),
             Value = format(round(c(perf$R2_marginal, perf$R2_conditional), 3), nsmall = 3),
             check.names = FALSE)
}

export_anova_lme <- function(model, fixed_map = NULL) {
  a <- as.data.frame(anova(model, type = "marginal"))
  a$Term <- rownames(a)
  a %>%
    mutate(Term = if (!is.null(fixed_map)) stringr::str_replace_all(Term, fixed_map) else Term,
           `F value` = format(round(`F-value`, 2), nsmall = 2),
           Df = paste0(numDF, ", ", denDF),
           P = format.pval(`p-value`, digits = 2, eps = 1e-16),
           `p-value` = ifelse(stars(`p-value`) == "", "ns", stars(`p-value`))) %>%
    select(`Fixed effects` = Term, `F value`, Df, `p-value`, P) %>%
    filter(`Fixed effects` != "(Intercept)") %>%
    `rownames<-`(NULL)
}

calculate_emmeans <- function(model, group_var, label = "Yield (t/ha)") {
  as.data.frame(emmeans::emmeans(model, specs = reformulate(group_var))) %>%
    mutate(!!paste0(label, " ± SE") := paste0(round(emmean, 2), " ± ", round(SE, 2))) %>%
    select(all_of(group_var), all_of(paste0(label, " ± SE")))
}


# -----------------------------------------------------------------------------
# 2. Random forest + SHAP
# -----------------------------------------------------------------------------

prepare_data <- function(df, x_vars, y_var) {
  location_cols <- c("farmer_ID", "province", "district", "commune", "admin_code",
                     "province_geo", "district_geo", "commune_geo", "stratified_group")
  keep_cols <- unique(c(y_var, x_vars, location_cols))
  df_subset <- df[, names(df) %in% keep_cols] %>%
    filter(!is.na(.data[[y_var]]), is.finite(.data[[y_var]])) %>%
    na.omit()
  list(X = df_subset[, names(df_subset) %in% x_vars, drop = FALSE],
       y = df_subset[[y_var]],
       locations = df_subset[, names(df_subset) %in% location_cols, drop = FALSE])
}

# 80/20 split stratified by province x season (`stratified_group`).
create_train_test_split <- function(X, y, locations, test_size = 0.2, random_state = 42) {
  set.seed(random_state)
  idx <- caret::createDataPartition(locations$stratified_group, p = 1 - test_size, list = FALSE)
  list(X_train = X[idx, , drop = FALSE], X_test = X[-idx, , drop = FALSE],
       y_train = y[idx], y_test = y[-idx],
       loc_train = locations[idx, , drop = FALSE], loc_test = locations[-idx, , drop = FALSE])
}

feature_engineering <- function(X_train, X_test, X_full = NULL) {
  to_factor <- function(df) mutate(df, across(where(~ !is.numeric(.)), as.factor))
  X_train_f <- to_factor(X_train)
  X_test_f  <- to_factor(X_test)
  factor_levels <- list()
  for (col in names(X_train_f)) {
    if (is.factor(X_train_f[[col]])) {
      factor_levels[[col]] <- levels(X_train_f[[col]])
      X_test_f[[col]] <- factor(X_test_f[[col]], levels = factor_levels[[col]])
    }
  }
  X_full_f <- NULL
  if (!is.null(X_full)) {
    X_full_f <- to_factor(X_full)
    for (col in names(factor_levels)) {
      X_full_f[[col]] <- factor(X_full_f[[col]], levels = factor_levels[[col]])
    }
  }
  list(X_train_processed = X_train_f, X_test_processed = X_test_f,
       X_full_processed = X_full_f,
       prep_state = list(factor_levels = factor_levels, feature_names = names(X_train_f)))
}

run_rf_model_caret <- function(X_train, y_train, X_test, y_test, num_trees = 500, seed = 42) {
  train_control <- caret::trainControl(method = "cv", number = 5, allowParallel = TRUE)
  tune_grid <- expand.grid(mtry = round(seq(2, ncol(X_train), length.out = 5)),
                           splitrule = "variance", min.node.size = c(1, 5, 10))
  set.seed(seed)
  caret_fit <- caret::train(x = X_train, y = y_train, method = "ranger",
                            trControl = train_control, tuneGrid = tune_grid,
                            num.trees = num_trees,
                            importance = "permutation")  # kept for identical results; not reported
  pred <- predict(caret_fit, newdata = X_test)
  list(model = caret_fit$finalModel, caret_object = caret_fit,
       rmse = sqrt(mean((pred - y_test)^2)),
       test_r2 = cor(pred, y_test)^2,
       mae = mean(abs(pred - y_test)))
}

calculate_SHAP <- function(model, X, nsim = 50, seed = 42) {
  set.seed(seed)
  pred_wrapper <- function(object, newdata) predict(object, data = newdata)$predictions
  shap_values <- fastshap::explain(object = model, X = X, pred_wrapper = pred_wrapper,
                                   nsim = nsim, adjust = TRUE)
  shapviz::shapviz(shap_values, X_pred = data.matrix(X), X = X, interactions = TRUE)
}

run_rf_analysis <- function(df, x_vars, y_var, test_size = 0.2, random_state = 42, nsim = 50) {
  prep  <- prepare_data(df, x_vars, y_var)
  split <- create_train_test_split(prep$X, prep$y, prep$locations, test_size, random_state)
  proc  <- feature_engineering(split$X_train, split$X_test, prep$X)
  model_results <- run_rf_model_caret(proc$X_train_processed, split$y_train,
                                      proc$X_test_processed, split$y_test,
                                      num_trees = 500, seed = random_state)
  message("  SHAP values for the full dataset (nsim = ", nsim, ") ...")
  shap_full <- calculate_SHAP(model_results$model, proc$X_full_processed, nsim = nsim)
  list(model_results = model_results, shap_values_full = shap_full,
       prep_state = proc$prep_state,
       config = list(x_vars = x_vars, y_var = y_var, test_size = test_size,
                     random_state = random_state, nsim = nsim,
                     n_obs = length(prep$y), n_train = length(split$y_train)),
       data = list(X_full_processed = proc$X_full_processed, y_full = prep$y),
       locations_full = prep$locations,
       session_info = sessionInfo())
}

# CHANGED: uses caret's selected model (bestTune; RMSE criterion) instead of the
# row with the highest CV R², and adds the hold-out test metrics.
rf_metrics <- function(rds_model, model_name) {
  co  <- rds_model$model_results$caret_object
  res <- merge(co$results, co$bestTune)
  nz <- function(x) if (is.null(x)) NA_real_ else x   # older .rds files lack test_r2 / n_obs
  data.frame(Model = model_name, n_obs = nz(rds_model$config$n_obs),
             CV_R2 = res$Rsquared, CV_RMSE = res$RMSE,
             Test_R2 = nz(rds_model$model_results$test_r2),
             Test_RMSE = nz(rds_model$model_results$rmse))
}

save_shap_plot <- function(shap_data, title, file_name, mapping, out_dir) {
  p <- shapviz::sv_importance(shap_data, kind = "bar", alpha = 1) +
    scale_y_discrete(labels = mapping) +
    ggtitle(title) +
    theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5))
  ggsave(file.path(out_dir, file_name), plot = p, width = 8, height = 6, dpi = 300)
  invisible(p)
}

# SHAP dependence plot. trim_outliers = TRUE drops points outside the 2.5–97.5th
# percentiles of the feature FOR PLOTTING ONLY (models are fitted on all data —
plot_shapviz_dependency <- function(shp, feature_col, color_col = "auto", type = "yield",
                                    trim_outliers = FALSE, file_name = NULL,
                                    save_dir = "plots", width = 7, height = 5) {
  feature_data <- shapviz::get_feature_values(shp)[[feature_col]]
  is_categorical <- is.factor(feature_data) || is.character(feature_data)
  if (trim_outliers && !is_categorical) {
    q <- quantile(feature_data, c(0.025, 0.975), na.rm = TRUE)
    shp <- shp[feature_data >= q[1] & feature_data <= q[2], ]
  }
  th <- theme(plot.title = element_text(size = 20),
              axis.title.y = element_text(size = 16, margin = margin(r = 10)),
              axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 16))
  p <- shapviz::sv_dependence(shp, v = feature_col, color_var = color_col)
  p <- if (is_categorical) {
    p + geom_boxplot(width = 0.2, alpha = 0.4, outlier.shape = NA, color = "black")
  } else {
    p + geom_smooth(aes(group = 1), method = "gam", formula = y ~ s(x, bs = "cs"),
                    color = "#e74c3c", se = FALSE, linewidth = 1.3, na.rm = TRUE)
  }
  p <- p + labs(title = paste0("SHAP Dependency: ", feature_col, " - ", type)) + th
  if (!is.null(file_name)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(file.path(save_dir, file_name), plot = p, width = width, height = height, dpi = 300)
  }
  invisible(p)
}

save_pdp_2d <- function(predictor, feature_pair, output_dir, type = "[PDP]",
                        width = 7, height = 5.5, dpi = 300) {
  pdp_2d <- iml::FeatureEffect$new(predictor, feature = feature_pair, method = "pdp")
  p <- plot(pdp_2d) +
    scale_fill_viridis_c(option = "mako", name = expression(hat(y))) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(size = 20, hjust = 0),
          plot.subtitle = element_text(size = 14, color = "grey30", hjust = 0),
          panel.grid = element_blank()) +
    labs(title = paste(type, "2D Partial Dependence Plot -"),
         subtitle = paste("Joint effect of", feature_pair[1], "and", feature_pair[2],
                          "on predicted yield"))
  f <- file.path(output_dir, paste0("pdp_2d_", type, "_", feature_pair[1], "_",
                                    feature_pair[2], ".png"))
  ggsave(f, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(f)
}

# 2D PDP on rows where BOTH features lie within their 2.5–97.5th percentiles.
save_pdp_2d_trimmed <- function(feature_pair, model_obj, X_data, y_data, output_dir,
                                type = "[PDP]") {
  f1 <- feature_pair[1]; f2 <- feature_pair[2]
  in_range <- function(v) v >= quantile(v, 0.025, na.rm = TRUE) & v <= quantile(v, 0.975, na.rm = TRUE)
  keep <- which(in_range(X_data[[f1]]) & in_range(X_data[[f2]]))
  pred <- iml::Predictor$new(model = model_obj, data = X_data[keep, ], y = y_data[keep])
  save_pdp_2d(pred, feature_pair, output_dir, type = type)
}

combine_img <- function(in_img_path, out_img_path, img_order, ncol = 4,
                        output_name = "combined_image.png", dpi = 300, margin_factor = 1.1) {
  img_files <- file.path(in_img_path, paste0(img_order, ".png"))
  missing <- img_files[!file.exists(img_files)]
  if (length(missing) > 0) warning("Missing files:\n", paste(missing, collapse = "\n"))
  img_files <- img_files[file.exists(img_files)]
  if (length(img_files) == 0) stop("No images found")

  info <- magick::image_info(magick::image_read(img_files[1]))
  nrow <- ceiling(length(img_files) / ncol)
  width  <- 16
  height <- (width / ncol) * (info$height / info$width) * nrow * margin_factor

  plots <- lapply(img_files, function(f) {
    img <- magick::image_read(f); ii <- magick::image_info(img)
    ggplot() + annotation_raster(as.raster(img), -Inf, Inf, -Inf, Inf) + theme_void() +
      theme(plot.margin = margin(5, 5, 5, 5), aspect.ratio = ii$height / ii$width)
  })
  combined <- patchwork::wrap_plots(plots, ncol = ncol)
  ggsave(file.path(out_img_path, output_name), combined, width = width, height = height,
         dpi = dpi, bg = "white")
  invisible(combined)
}


# -----------------------------------------------------------------------------
# 3. Surrogate multi-objective optimization (RF + NSGA-II)
# -----------------------------------------------------------------------------

clamp_int <- function(val, lo, hi) min(max(round(val), lo), hi)

# Evaluate one candidate input set x = (N, P2O5, K2O, seed rate).
# The SAME input values are imposed on every farmer-season row in `data`, and the
eval_one <- function(x, data, rf_yield, rf_profit, rf_pfpn, low_b, upp_b,
                     pfpn_low = 40, pfpn_high = 80) {
  vals <- c(N_amt     = clamp_int(x[1], low_b["N_amt"],     upp_b["N_amt"]),
            P2O5_amt  = clamp_int(x[2], low_b["P2O5_amt"],  upp_b["P2O5_amt"]),
            K2O_amt   = clamp_int(x[3], low_b["K2O_amt"],   upp_b["K2O_amt"]),
            seed_rate = clamp_int(x[4], low_b["seed_rate"], upp_b["seed_rate"]))
  tmp <- data
  for (v in names(vals)) tmp[[v]] <- vals[[v]]

  mean_yield  <- mean(predict(rf_yield,  data = tmp)$predictions)
  mean_profit <- mean(predict(rf_profit, data = tmp)$predictions)  # profit is RF-predicted
  mean_pfpn   <- mean(predict(rf_pfpn,   data = tmp)$predictions)

  list(F = c(-mean_yield, -mean_profit),                           # nsga2 minimises
       G = c(mean_pfpn - pfpn_low, pfpn_high - mean_pfpn),         # feasible if >= 0
       vals = vals, mean_yield = mean_yield, mean_profit = mean_profit, mean_pfpn = mean_pfpn)
}

# Run NSGA-II for one dataset (pooled or one season) and re-evaluate the front.
run_optimization <- function(data, rf_yield, rf_profit, rf_pfpn, vars = OPT_VARS,
                             probs = OPT_BOUNDS_PROB, pfpn_range = OPT_PFPN_RANGE,
                             settings = NSGA2, seed = SEED) {
  bounds <- sapply(data[, vars, drop = FALSE], quantile, probs = probs, na.rm = TRUE)
  low_b <- floor(bounds[1, ]); upp_b <- ceiling(bounds[2, ])

  ev <- function(x) eval_one(x, data, rf_yield, rf_profit, rf_pfpn, low_b, upp_b,
                             pfpn_range[1], pfpn_range[2])
  set.seed(seed)
  res <- mco::nsga2(fn = function(x) ev(x)$F, idim = length(vars), odim = 2,
                    constraints = function(x) ev(x)$G, cdim = 2,
                    lower.bounds = as.numeric(low_b[vars]),
                    upper.bounds = as.numeric(upp_b[vars]),
                    popsize = settings$popsize, generations = settings$generations,
                    cprob = settings$cprob, cdist = settings$cdist,
                    mprob = settings$mprob, mdist = settings$mdist)

  # Normalised hypervolume (convergence diagnostic; report in SI)
  pf <- mco::paretoFront(res)
  pf_norm <- apply(pf, 2, function(v) (v - min(v)) / (max(v) - min(v)))
  hv <- mco::dominatedHypervolume(pf_norm, ref = c(1.1, 1.1))

  pareto_eval <- lapply(seq_len(nrow(res$par)), function(i) {
    e <- ev(as.numeric(res$par[i, ]))
    data.frame(t(e$vals), yield = e$mean_yield, profit = e$mean_profit, PFPN = e$mean_pfpn)
  }) %>% bind_rows() %>% distinct()

  list(nsga2 = res, pareto_eval = pareto_eval, hypervolume = hv,
       bounds = rbind(low_b = low_b, upp_b = upp_b), n_rows = nrow(data))
}

plot_pareto <- function(data, x_var = "yield", y_var = "profit", color_var = NULL,
                        point_color = "#2C7BB6", color_label = NULL,
                        title = "Pareto trade-off between rice yield and profit",
                        x_label = expression("Predicted yield (t ha"^{-1}*")"),
                        y_label = "Predicted profit", point_size = 3, point_alpha = 0.8,
                        base_size = 14, legend_position = "right", out_dir, file_name,
                        viridis_option = "C") {
  p <- if (is.null(color_var)) {
    ggplot(data, aes(.data[[x_var]], .data[[y_var]])) +
      geom_point(size = point_size, alpha = point_alpha, color = point_color)
  } else {
    ggplot(data, aes(.data[[x_var]], .data[[y_var]], color = .data[[color_var]])) +
      geom_point(size = point_size, alpha = point_alpha) +
      scale_color_viridis_c(option = viridis_option, name = color_label)
  }
  p <- p + labs(x = x_label, y = y_label, title = title) +
    theme_bw(base_size = base_size) +
    theme(legend.position = legend_position, plot.title = element_text(size = 22),
          axis.title.y = element_text(size = 16, margin = margin(r = 10)),
          axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 16))
  ggsave(file.path(out_dir, file_name), plot = p, width = 8, height = 6, dpi = 300)
  invisible(p)
}

# k-means on the Pareto front. k = NULL -> chosen by average silhouette (k = 2..10).
run_pareto_pipeline <- function(data, docx_path, cluster_cols = c("yield", "profit"),
                                k = NULL, seed = 42, k_range = 2:10) {
  cluster_scaled <- scale(data[, cluster_cols])
  sil_tab <- NULL
  if (is.null(k)) {
    set.seed(seed)
    d <- dist(cluster_scaled)
    sil_tab <- data.frame(k = k_range, avg_silhouette = sapply(k_range, function(i) {
      km <- kmeans(cluster_scaled, centers = i, nstart = 100)
      mean(cluster::silhouette(km$cluster, d)[, 3])
    }))
    k <- sil_tab$k[which.max(sil_tab$avg_silhouette)]
    message("  Silhouette-optimal k = ", k)
  }
  set.seed(seed)
  km <- kmeans(cluster_scaled, centers = k, nstart = 100)

  labels <- if (k == 3) c("Highest-yield high-input", "Balanced moderate-input",
                          "Lowest-yield input-saving") else paste("Cluster", seq_len(k))
  reorder <- data %>% mutate(cl = km$cluster) %>% group_by(cl) %>%
    summarise(m = mean(yield), .groups = "drop") %>% arrange(desc(m)) %>%
    mutate(cluster = row_number())
  out_data <- data %>% mutate(cl = km$cluster) %>% left_join(reorder[, c("cl", "cluster")], by = "cl") %>%
    mutate(cluster_label = factor(labels[cluster], levels = labels),
           cluster = factor(cluster)) %>% select(-cl)

  summary_tab <- out_data %>% group_by(cluster_label) %>%
    summarise(`No. of solutions` = n(), `N rate` = mean(N_amt), `P₂O₅ rate` = mean(P2O5_amt),
              `K₂O rate` = mean(K2O_amt), `Seed rate` = mean(seed_rate),
              Yield = mean(yield), Profit = mean(profit), PFPN = mean(PFPN), .groups = "drop") %>%
    mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
    rename(`Cluster classification` = cluster_label)

  ft <- flextable::flextable(summary_tab) %>% flextable::theme_booktabs() %>%
    flextable::autofit() %>% flextable::bold(part = "header") %>% flextable::fontsize(size = 10, part = "all")
  flextable::save_as_docx(ft, path = docx_path)
  list(data = out_data, summary = summary_tab, k = k, silhouette = sil_tab)
}

plot_pareto_clusters <- function(data, output_path, width = 9.5, height = 6, dpi = 300) {
  p <- ggplot(data, aes(yield, profit, color = cluster_label)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(x = expression("Mean predicted yield (t ha"^-1*")"),
         y = expression("Mean predicted profit (million VND ha"^-1*")"),
         color = "", title = "Pareto cluster identification") +
    theme_bw(base_size = 16) +
    theme(legend.position = "right", plot.title = element_text(size = 24),
          panel.grid.minor = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))
  ggsave(output_path, p, width = width, height = height, dpi = dpi)
  invisible(p)
}

# Linear / quadratic / cubic fit of profit on standardised yield along the front.
run_pareto_regression_pipeline <- function(data, output_path, width = 8, height = 6, dpi = 300) {
  mu <- mean(data$yield); s <- sd(data$yield)
  reg <- data %>% arrange(yield) %>% mutate(yield_z = (yield - mu) / s)
  # Fit as separate objects (as in the original) so compare_performance() names
  # them "model_linear", "model_quadratic", "model_cubic"
  model_linear    <- lm(profit ~ yield_z, data = reg)
  model_quadratic <- lm(profit ~ yield_z + I(yield_z^2), data = reg)
  model_cubic     <- lm(profit ~ yield_z + I(yield_z^2) + I(yield_z^3), data = reg)
  models <- list(model_linear = model_linear, model_quadratic = model_quadratic,
                 model_cubic = model_cubic)

  comp <- performance::compare_performance(model_linear, model_quadratic, model_cubic, rank = TRUE)
  best_name <- as.character(comp$Name[which.max(comp$Performance_Score)])
  if (!best_name %in% names(models)) {
    stop("Could not match the selected model name '", best_name, "' to the fitted models")
  }
  best <- models[[best_name]]
  b <- coef(best); r2 <- summary(best)$r.squared

  grid <- data.frame(yield = seq(min(reg$yield), max(reg$yield), length.out = 300))
  grid$yield_z <- (grid$yield - mu) / s
  grid$profit_pred <- predict(best, newdata = grid)

  eq <- switch(best_name,
    model_linear    = sprintf("Linear fit: Profit = %.1f %+.1f(Y)\nR² = %.2f", b[1], b[2], r2),
    model_quadratic = sprintf("Quadratic fit: Profit = %.1f %+.1f(Y) %+.2f(Y)²\nR² = %.2f", b[1], b[2], b[3], r2),
    model_cubic     = sprintf("Cubic fit: Profit = %.1f %+.1f(Y) %+.2f(Y)² %+.2f(Y)³\nR² = %.2f", b[1], b[2], b[3], b[4], r2))
  eq <- paste0(eq, "   (Y = standardised yield)")

  p <- ggplot(reg, aes(yield, profit)) +
    geom_point(size = 3, alpha = 0.8, color = "#2C7BB6") +
    geom_line(data = grid, aes(yield, profit_pred), color = "red", linewidth = 1, inherit.aes = FALSE) +
    labs(x = "Mean predicted yield (t ha⁻¹)", y = "Mean predicted profit (million VND ha⁻¹)",
         title = "Pareto trade-off between rice yield and profit", subtitle = eq) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(size = 24), plot.subtitle = element_text(size = 14),
          panel.grid.minor = element_blank())
  ggsave(output_path, p, width = width, height = height, dpi = dpi)
  fit_table <- data.frame(model = names(models),
                          R2  = sapply(models, function(m) summary(m)$r.squared),
                          AIC = sapply(models, AIC),
                          selected_by_performance_score = names(models) == best_name,
                          lowest_AIC = names(models) == names(models)[which.min(sapply(models, AIC))])
  list(best_model = best, best_name = best_name, comparison = comp,
       fit_table = fit_table, plot = p)
}

# % change of each Pareto cluster relative to farmers' current practice
compare_to_practice <- function(cluster_summary, practice) {
  cols <- c(`N rate` = "N", `P₂O₅ rate` = "P", `K₂O rate` = "K",
            `Seed rate` = "seed", Yield = "yield", Profit = "profit", PFPN = "PFPN")
  out <- cluster_summary
  for (cc in names(cols)) {
    out[[paste0(cc, " (% vs practice)")]] <-
      round(100 * (cluster_summary[[cc]] - practice[[cols[[cc]]]]) / practice[[cols[[cc]]]], 1)
  }
  out
}


# -----------------------------------------------------------------------------
# 4. District clustering (Fig. 5)
# -----------------------------------------------------------------------------

run_seasonal_kmeans <- function(data, cluster_cols, yield_col = "yield", k = 3,
                                k_max = 10, seed = 123, elbow_file = NULL) {
  cols <- unique(c(cluster_cols, yield_col))
  bad <- rowSums(is.na(data[, cols])) > 0 | rowSums(!is.finite(as.matrix(data[, cols]))) > 0
  data_clean <- data[!bad, ]
  if (nrow(data_clean) <= k_max) stop("Not enough valid rows after removing NA/Inf.")

  z <- scale(data_clean[, cluster_cols])
  set.seed(seed)
  wss <- sapply(2:k_max, function(i) kmeans(z, centers = i, nstart = 100)$tot.withinss)
  if (!is.null(elbow_file)) {
    png(elbow_file, width = 1600, height = 1200, res = 200)
    plot(2:k_max, wss, type = "b", pch = 19, xlab = "Number of clusters (k)",
         ylab = "Total within-cluster sum of squares",
         main = paste("Elbow plot -", unique(data_clean$season)))
    abline(v = k, col = "red", lty = 2)
    dev.off()
  }
  set.seed(seed)
  km <- kmeans(z, centers = k, nstart = 100)
  reorder <- data_clean %>% mutate(cl = km$cluster) %>% group_by(cl) %>%
    summarise(m = mean(.data[[yield_col]]), .groups = "drop") %>%
    arrange(desc(m)) %>% mutate(cluster_new = row_number())
  data_clean %>% mutate(cl = km$cluster) %>%
    inner_join(reorder[, c("cl", "cluster_new")], by = "cl") %>% select(-cl)
}

read_province_map <- function(path) {
  sf::read_sf(path) %>%
    mutate(province_abbr = ifelse(province %in% names(PROVINCE_ABBR),
                                  PROVINCE_ABBR[province], province))
}


# -----------------------------------------------------------------------------
# 5. Where RF model files are stored
#    rf_path("yield")               -> outputs/models/yield_model/rf_yield.rds
#    rf_path("pfpn_withoutfer_ws")  -> outputs/models/pfpn_model/rf_pfpn_withoutfer_ws.rds
# -----------------------------------------------------------------------------
rf_path <- function(name) {
  folder <- if (startsWith(name, "yield")) "yield_model" else
            if (startsWith(name, "profit")) "profit_model" else "pfpn_model"
  file.path(PATHS$models, folder, paste0("rf_", name, ".rds"))
}

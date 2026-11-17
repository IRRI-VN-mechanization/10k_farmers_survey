# =============================================================================
# 03_descriptives.R — summary statistics and supporting tables
# Paper: farmers' current practice (baseline, season-specific results),
#        sample sizes, precision-seeding share, EMM tables, N-P-K-seed
#        correlations (Supplementary), crop-calendar table 
#        soil and variety shares by province/season
# =============================================================================

source(here::here("R", "config.R")); source(here::here("R", "packages.R")); source(here::here("R", "utils.R"))
df_cleaned <- readRDS(out("processed", "df_cleaned.rds"))

# ---- Sample sizes (report in Methods) ----------------------------------------
sample_sizes <- df_cleaned %>%
  group_by(season) %>%
  summarise(farmer_seasons = n(), farmers = n_distinct(farmer_ID),
            provinces = n_distinct(province), districts = n_distinct(district_geo),
            communes = n_distinct(admin_code), .groups = "drop") %>%
  bind_rows(df_cleaned %>% summarise(season = "All", farmer_seasons = n(),
                                     farmers = n_distinct(farmer_ID),
                                     provinces = n_distinct(province),
                                     districts = n_distinct(district_geo),
                                     communes = n_distinct(admin_code)))

# ---- Farmers' current practice (means) ----------------------------------------
practice_summary <- function(d) {
  summarise(d,
            N = mean(N_amt, na.rm = TRUE), P = mean(P2O5_amt, na.rm = TRUE),
            K = mean(K2O_amt, na.rm = TRUE), seed = mean(seed_rate, na.rm = TRUE),
            yield = mean(yield_14, na.rm = TRUE), profit = mean(net_income, na.rm = TRUE),
            PFPN = mean(PFPN[is.finite(PFPN) & PFPN != 0], na.rm = TRUE)) %>%  # 0/Inf dropped
    mutate(across(where(is.numeric), ~ round(.x, 1)))
}
current_practice <- bind_rows(
  df_cleaned %>% practice_summary() %>% mutate(season = "All", .before = 1),
  df_cleaned %>% group_by(season) %>% practice_summary() %>% mutate(season = as.character(season)))
saveRDS(current_practice, out("processed", "current_practice.rds"))   # used by 06

# ---- Shares quoted in the text ----------------------------------------------
shares <- list(
  sowing_method  = df_cleaned %>% count(sowing_method) %>% mutate(pct = round(100 * n / sum(n), 1)),
  farming_system = df_cleaned %>% count(season, farming_system) %>%
    group_by(season) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup(),
  soil_by_province = df_cleaned %>% count(season, province, soil_type) %>%
    group_by(season, province) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup(),
  top5_varieties_by_province = df_cleaned %>% filter(!is.na(rice_variety)) %>%
    count(season, province, rice_variety) %>% group_by(season, province) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>% slice_max(pct, n = 5, with_ties = FALSE) %>% ungroup()
)

# ---- Correlations among N, P, K and seed rate (2.5–97.5% trimmed) ------------
in_range <- function(v) v >= quantile(v, 0.025, na.rm = TRUE) & v <= quantile(v, 0.975, na.rm = TRUE)
data_trimmed <- df_cleaned %>%
  filter(in_range(N_amt), in_range(P2O5_amt), in_range(K2O_amt), in_range(seed_rate))
cor_mat <- round(cor(data_trimmed[, c("N_amt", "P2O5_amt", "K2O_amt", "seed_rate")]), 2)

fig_corr <- GGally::ggpairs(
  data_trimmed, columns = c("N_amt", "P2O5_amt", "K2O_amt", "seed_rate"),
  upper = list(continuous = GGally::wrap("cor", method = "pearson", size = 6, color = "black")),
  lower = list(continuous = GGally::wrap("smooth", method = "lm", alpha = 0.2, size = 0.6)),
  diag  = list(continuous = GGally::wrap("densityDiag", fill = "grey80", alpha = 0.7))) +
  theme_bw(base_size = 14)
ggsave(out("fig_desc", "fertilizer_cor.png"), fig_corr, width = 8.5, height = 8.5, dpi = 300)

# ---- Crop calendar by farming system x season x province --
crop_calendar <- df_cleaned %>%
  mutate(Middle_CE_date = as.Date(Middle_CE_date), Middle_Harvest_date = as.Date(Middle_Harvest_date)) %>%
  group_by(farming_system, season, province) %>%
  summarise(start = min(Middle_CE_date, na.rm = TRUE), end = max(Middle_Harvest_date, na.rm = TRUE),
            .groups = "drop") %>%
  filter(is.finite(start), is.finite(end)) %>%
  mutate(start_month = format(start, "%B %Y"), end_month = format(end, "%B %Y"))

# ---- Export ---------------------------------------------------------------
writexl::write_xlsx(c(list(sample_sizes = sample_sizes, 
                           current_practice = current_practice,
                           correlation_NPK_seed = cbind(variable = rownames(cor_mat), as.data.frame(cor_mat)),
                           crop_calendar = crop_calendar), shares),
                    out("tables", "descriptive_tables.xlsx"))
message("03 done")

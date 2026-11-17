# =============================================================================
# 08_1m5r_compliance.R — share of farmers above 1M5R / 1-million-ha benchmarks
# Paper: Supplementary (1M5R_non_compliance_grid_map.png) + table
# =============================================================================

source(here::here("R", "config.R")); source(here::here("R", "packages.R")); source(here::here("R", "utils.R"))
df_cleaned <- readRDS(out("processed", "df_cleaned.rds"))

BENCH <- list(N = 101, P2O5 = 60, K2O = 40, seed = 120, min_splits = 3)

df_1m5r <- df_cleaned %>%
  filter(!(province %in% c("ST", "Tỉnh Sóc Trăng", "Soc Trang") & season == "AW")) %>%  # Soc Trang AW excluded
  # coalesce(..., FALSE): missing values count as "not following", as in the original
  mutate(not_splits    = !coalesce(num_fer_splits >= BENCH$min_splits, FALSE),
         not_N         = !coalesce(N_amt < BENCH$N, FALSE),
         not_P         = !coalesce(P2O5_amt < BENCH$P2O5, FALSE),
         not_K         = !coalesce(K2O_amt < BENCH$K2O, FALSE),
         not_seed      = !coalesce(seed_rate < BENCH$seed, FALSE),
         not_cert_seed = !(tolower(as.character(seed_type)) %in% c("certified", "xác nhận", "1", "true")))

pct_not_follow <- df_1m5r %>%
  group_by(district_geo, season) %>%
  summarise(across(starts_with("not_"), ~ 100 * mean(.x, na.rm = TRUE), .names = "pct_{.col}"),
            total_farms = n(), .groups = "drop")

# Delta-wide summary by season (numbers quoted in the Discussion)
pct_by_season <- df_1m5r %>% group_by(season) %>%
  summarise(across(starts_with("not_"), ~ round(100 * mean(.x, na.rm = TRUE), 1)), n = n(), .groups = "drop")

writexl::write_xlsx(list(by_district = pct_not_follow, by_season = pct_by_season),
                    out("tables", "SI_1m5r_noncompliance.xlsx"))

district_map <- sf::read_sf(PATHS$district_shp)
province_map <- read_province_map(PATHS$province_shp)
labels <- c(pct_not_splits = "Fertilizer splits (< 3)", pct_not_N = "Nitrogen rate (> 100 kg/ha)",
            pct_not_P = "Phosphorus rate (≥ 60 kg/ha)", pct_not_K = "Potassium rate (≥ 40 kg/ha)",
            pct_not_seed = "Seed rate (≥ 120 kg/ha)", pct_not_cert_seed = "Uncertified seed use")

pct_long_map <- pct_not_follow %>%
  inner_join(district_map, by = c("district_geo" = "district")) %>%
  pivot_longer(starts_with("pct_"), names_to = "indicator", values_to = "non_compliance_pct") %>%
  mutate(indicator = labels[indicator]) %>%
  sf::st_as_sf()

p_grid <- ggplot() +
  geom_sf(data = district_map, fill = "#F5F5F5", color = "gray85", linewidth = 0.1) +
  geom_sf(data = pct_long_map, aes(fill = non_compliance_pct), color = "gray60", linewidth = 0.15) +
  geom_sf(data = province_map, fill = NA, color = "black", linewidth = 0.25) +
  geom_sf_text(data = province_map, aes(label = province_abbr), size = 2.2) +
  scale_fill_gradient(low = "#FFFFFF", high = "#E76F51", limits = c(0, 100),
                      name = "Above benchmark (%)", labels = scales::percent_format(scale = 1),
                      na.value = "#F5F5F5") +
  facet_grid(indicator ~ season) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        strip.text = element_text(face = "bold", size = 10), legend.position = "bottom",
        legend.key.width = unit(2.5, "cm"), plot.background = element_rect(fill = "white", color = NA))
ggsave(out("fig_paper", "1M5R_non_compliance_grid_map.png"), p_grid, width = 12, height = 14, dpi = 300)
message("08 done")

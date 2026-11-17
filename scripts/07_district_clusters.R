# =============================================================================
# 07_district_clusters.R — k-means of district means (yield, PFPN, profit) per season
# Paper: Fig. 5 (cluster_p.png + the profile tables under the maps)
# =============================================================================

source(here::here("R", "config.R")); source(here::here("R", "packages.R")); source(here::here("R", "utils.R"))
df_cleaned <- readRDS(out("processed", "df_cleaned.rds"))

df_summary <- df_cleaned %>%
  filter(is.finite(PFPN)) %>%
  group_by(district_geo, season, province) %>%
  summarise(n_obs = n(),                                   # CHANGED: n per district reported
            mean_yield = mean(yield_14, na.rm = TRUE), mean_PFPN = mean(PFPN, na.rm = TRUE),
            mean_profit = mean(net_income, na.rm = TRUE), .groups = "drop")

# k = 5 fixed; elbow (WSS) plots for k = 2..10 saved for the SI
df_final_kmeans <- bind_rows(lapply(split(df_summary, df_summary$season), function(d)
  run_seasonal_kmeans(d, cluster_cols = c("mean_yield", "mean_PFPN", "mean_profit"),
                      yield_col = "mean_yield", k = DISTRICT_K, k_max = 10,
                      elbow_file = out("fig_desc", paste0("elbow_districts_", unique(d$season), ".png")))))

df_merged_cluster <- df_cleaned %>%
  filter(is.finite(PFPN)) %>%
  inner_join(select(df_final_kmeans, district_geo, province, season, cluster_new),
             by = c("district_geo", "province", "season"))

# ---- Cluster profiles (tables in Fig. 5) ------------------------------------
numeric_profile <- df_merged_cluster %>%
  group_by(season, cluster_new) %>%
  summarise(n_obs = n(), n_districts = n_distinct(district_geo),
            Yield = mean(yield_14, na.rm = TRUE), PFPN = mean(PFPN, na.rm = TRUE),
            Profit = mean(net_income, na.rm = TRUE), `Seed rate` = mean(seed_rate, na.rm = TRUE),
            `N amount` = mean(N_amt, na.rm = TRUE), `P2O5 amount` = mean(P2O5_amt, na.rm = TRUE),
            `K2O amount` = mean(K2O_amt, na.rm = TRUE),
            `Crop est. time` = mean(crop_establisment_time, na.rm = TRUE),
            `Harvest time` = mean(harvest_time, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(-c(season, cluster_new), names_to = "Variable", values_to = "value") %>%
  mutate(Category = "Mean")

categorical_profile <- df_merged_cluster %>%
  select(farmer_ID, season, cluster_new, sowing_method, farming_system, soil_type) %>%
  distinct() %>%
  pivot_longer(c(sowing_method, farming_system, soil_type), names_to = "Category", values_to = "Variable") %>%
  filter(!is.na(Variable)) %>%
  count(season, Category, Variable, cluster_new) %>%
  group_by(season, Category, cluster_new) %>% mutate(value = 100 * n / sum(n)) %>% ungroup() %>%
  select(-n)

top_varieties <- df_merged_cluster %>%
  select(farmer_ID, season, cluster_new, rice_variety) %>% distinct() %>%
  filter(!is.na(rice_variety)) %>% count(season, cluster_new, rice_variety) %>%
  group_by(season, cluster_new) %>% mutate(value = 100 * n / sum(n)) %>%
  slice_max(value, n = 5, with_ties = FALSE) %>% ungroup() %>%
  transmute(season, cluster_new, Category = "rice_variety (%)", Variable = rice_variety, value)

district_counts <- df_final_kmeans %>% count(season, province, cluster_new, name = "value") %>%
  transmute(season, cluster_new, Category = "districts per province", Variable = province, value)

profile <- bind_rows(numeric_profile, categorical_profile, top_varieties, district_counts) %>%
  mutate(value = round(value, 1)) %>%
  pivot_wider(names_from = cluster_new, values_from = value, names_prefix = "Cluster ") %>%
  arrange(season, Category != "Mean")

# Regional means (Upper / Central / Coastal / Ca Mau) quoted in the Discussion
regional <- df_merged_cluster %>%
  mutate(region = case_when(province %in% c("AG", "DT", "CT", "LA", "KG") ~ "upper",
                            province %in% c("TG", "HG", "VL") ~ "central",
                            province %in% c("BL", "ST", "TV") ~ "coastal",
                            province == "CM" ~ "fringe")) %>%
  group_by(season, region) %>%
  summarise(across(c(yield_14, PFPN, net_income, seed_rate, N_amt, P2O5_amt, K2O_amt),
                   ~ round(mean(.x, na.rm = TRUE), 1)), .groups = "drop")

writexl::write_xlsx(list(cluster_profiles = profile, district_clusters = df_final_kmeans,
                         regional_means = regional),
                    out("tables", "Fig5_district_cluster_profiles.xlsx"))

# ---- Fig. 5 maps --------------------------------------------------------------
district_map <- sf::read_sf(PATHS$district_shp)
province_map <- read_province_map(PATHS$province_shp)
map_clusters <- district_map %>%
  inner_join(df_final_kmeans %>% distinct(district_geo, season, cluster_new) %>%
               mutate(cluster_new = as.character(cluster_new)),
             by = c("district" = "district_geo"))

cluster_p <- ggplot() +
  geom_sf(data = district_map, fill = "#F2F2F2", color = "white", linewidth = 0.15) +
  geom_sf(data = map_clusters, aes(fill = cluster_new), color = "white", linewidth = 0.15) +
  geom_sf(data = province_map, fill = NA, color = "#333333", linewidth = 0.3) +
  geom_sf_label(data = province_map, aes(label = province_abbr), size = 2.4, color = "#3A3A3A",
                fill = scales::alpha("#FFFFFF", 0.8), label.padding = unit(0.12, "lines"), label.size = NA) +
  facet_wrap(~ season) +
  scale_fill_manual(values = c("1" = "#2A9D8F", "2" = "#8AB17D", "3" = "#E9C46A", "4" = "#F4A261", "5" = "#E76F51"),
                    labels = c("1" = "C1 - Top performance", "2" = "C2 - High performance",
                               "3" = "C3 - Moderate performance", "4" = "C4 - Low performance",
                               "5" = "C5 - Lowest performance"), name = "") +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 11),
        axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(x = NULL, y = NULL)
ggsave(out("fig_paper", "cluster_p.png"), cluster_p, width = 10, height = 6, dpi = 300)
message("07 done")

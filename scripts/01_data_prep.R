# =============================================================================
# 01_data_prep.R — load the ready (cleaned) data; NO cleaning or recoding
# Input : data/cleaned_data/cleaned_data.csv
#         (one row per farmer-season, final variable names and categories)
# Output: outputs/processed/df_cleaned.rds, df_model.rds
# =============================================================================

source(here::here("R", "config.R")); source(here::here("R", "packages.R")); source(here::here("R", "utils.R"))

df_cleaned <- read.csv(PATHS$cleaned_data, fileEncoding = "UTF-8") %>%
  mutate(season = factor(season, levels = SEASONS),          # WS, SA, AW order in tables/plots
         stratified_group = paste(province, season, sep = "_"))  # strata for RF train/test split

# Only if district names are not yet in the file: add them from the commune shapefile
if (!"district_geo" %in% names(df_cleaned)) {
  admin <- sf::st_read(PATHS$commune_shp, quiet = TRUE) %>% sf::st_drop_geometry() %>%
    select(admin_code, district_geo = district)
  df_cleaned <- left_join(df_cleaned, admin, by = "admin_code")
}

df_model <- df_cleaned   # same data; both names are used by later scripts

saveRDS(df_cleaned, out("processed", "df_cleaned.rds"))
saveRDS(df_model,   out("processed", "df_model.rds"))

message("01 done: ", nrow(df_cleaned), " farmer-season observations, ",
        n_distinct(df_cleaned$farmer_ID), " farmers")

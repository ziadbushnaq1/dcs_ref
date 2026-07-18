# Usage: Rscript sens_trim_extreme.R <asset_id> <trim_pct> [trim_pct2 ...]
# e.g.:  Rscript sens_trim_extreme.R landsat_hyperscale_all 1 5
library(tidyverse); library(sf); library(here)
source(here("analysis", "heat", "an_01_functions.R"))

args     <- commandArgs(trailingOnly = TRUE)
ASSET_ID <- args[1]
TRIMS    <- as.numeric(args[-1]); if (length(TRIMS) == 0) TRIMS <- c(1, 5)
 
SENSOR <- "landsat_monthly"
TREAT_MIN <- 300; TREAT_MAX <- 600; CONTROL_MIN <- 1000; CONTROL_MAX <- 7500

pixel_data <- read_csv(
  here("data","processed",ASSET_ID,"landsat_monthly_obs_lst_landcover.csv"),
  col_select = c(export_id, longitude, latitude, year, month,
                 date_yyyymmdd, LST_Celsius, Elevation),
  show_col_types = FALSE)

best_dates <- pixel_data %>%
  group_by(export_id, year, month, date_yyyymmdd) %>%
  summarise(n_valid = sum(!is.na(LST_Celsius)), .groups="drop") %>%
  group_by(export_id, year, month) %>%
  filter(n_valid == max(n_valid)) %>% filter(date_yyyymmdd == min(date_yyyymmdd)) %>%
  ungroup() %>% select(export_id, year, month, date_yyyymmdd)
pixel_data <- semi_join(pixel_data, best_dates,
                        by = c("export_id","year","month","date_yyyymmdd"))
rm(best_dates); gc()

dc_points <- read_csv(here("data","data_final","isolation_sets",
                           paste0(ASSET_ID,".csv")), show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

results <- map_dfr(c(0, TRIMS), function(p) {
  d <- if (p == 0) pixel_data else {
    # Trim within DC x year x month: extremes are defined against the local
    # same-scene distribution, not the pooled panel (a Phoenix June pixel
    # should not be trimmed for being hotter than a Boston January one).
    pixel_data %>%
      group_by(export_id, year, month) %>%
      filter(LST_Celsius >  quantile(LST_Celsius, p/100,      na.rm = TRUE),
             LST_Celsius <  quantile(LST_Celsius, 1 - p/100,  na.rm = TRUE)) %>%
      ungroup()
  }
  res <- run_pixel_analysis(d, dc_points,
                            TREAT_MIN, TREAT_MAX, CONTROL_MIN, CONTROL_MAX,
                            fe_spec = "pixel_id + year", use_power_builtout = FALSE,
                            filter_elev = TRUE, elev_threshold = 50,
                            is_event_study = FALSE, sensor = SENSOR)
  ct <- summary(res$model)$coeftable
  out <- tibble(asset = ASSET_ID, trim_pct = p,
                n_obs = res$model$nobs,
                estimate  = ct["treated_post","Estimate"],
                std_error = ct["treated_post","Std. Error"],
                p_val     = ct["treated_post","Pr(>|t|)"])
  rm(res, d); gc(); out
})

print(knitr::kable(results, digits = 3))
write_csv(results, here("results",
                        paste0("sens_trim_", ASSET_ID, ".csv")))
# ==============================================================================
# check_nlcd_change.R
# Quantifies year-to-year NLCD land-cover change in the extracted pixel data.
#
# Answers three questions:
#   1. How much does any pixel's land cover change year to year? (stability)
#   2. Which NLCD classes change most, and where (treatment vs control ring)?
#   3. Does change spike around year_operational? (construction signature —
#      directly relevant to the ref_year = -1 contamination concern)
#
# Run per asset:  Rscript check_nlcd_change.R landsat_iso10000m
# or set asset_id manually below and source() it.
# ==============================================================================

library(tidyverse)
library(here)

args     <- commandArgs(trailingOnly = TRUE)
asset_id <- if (length(args) >= 1) args[1] else "landsat_iso10000m"

# NLCD proportions are annual — identical for every scene within a year —
# so we only need one row per pixel-year, not the full monthly data.
nlcd_cols <- paste0("NLCD_", c(11,12,21,22,23,24,31,41,42,43,
                               51,52,71,72,73,74,81,82,90,95))

message("Reading: ", asset_id)
px <- read_csv(
  here("data", "processed", asset_id, "landsat_monthly_obs_lst_landcover.csv"),
  col_select = all_of(c("export_id", "longitude", "latitude", "year", nlcd_cols)),
  show_col_types = FALSE
) %>%
  distinct(export_id, longitude, latitude, year, .keep_all = TRUE)

# Drop pixel-years with no NLCD match (unmatched left-join rows)
n_before <- nrow(px)
px <- px %>% filter(!is.na(NLCD_11))
message("Pixel-years: ", format(n_before, big.mark = ","),
        " | with NLCD: ", format(nrow(px), big.mark = ","),
        " (", round(100 * nrow(px) / n_before, 1), "% matched)")

# DC metadata for operational year and distance classification
dc <- read_csv(
  here("data", "data_final", "isolation_sets", paste0(asset_id, ".csv")),
  show_col_types = FALSE
) %>% select(export_id, year_operational, dc_x = projected_x, dc_y = projected_y)

# Classify pixels into rough zones (same geometry logic as the analysis)
coords <- sf::sf_project(from = sf::st_crs(4326), to = sf::st_crs(5070),
                         pts = as.matrix(px[, c("longitude", "latitude")]))
px$pixel_x <- coords[, 1]; px$pixel_y <- coords[, 2]

px <- px %>%
  left_join(dc, by = "export_id") %>%
  mutate(
    dist_to_dc = sqrt((pixel_x - dc_x)^2 + (pixel_y - dc_y)^2),
    zone = case_when(
      dist_to_dc <= 600            ~ "treatment_ring(0-600m)",
      dist_to_dc >= 1000 &
        dist_to_dc <= 7500         ~ "control_ring(1000-7500m)",
      TRUE                         ~ "other"
    ),
    rel_year = year - year_operational
  )

# ── 1. Per-pixel year-over-year change ────────────────────────────────────
# Total absolute change across all 20 class proportions, pixel vs itself
# one year earlier. 0 = identical; 2 = complete conversion (one class to
# another); values are comparable across pixels and years.

yoy <- px %>%
  arrange(export_id, longitude, latitude, year) %>%
  group_by(export_id, longitude, latitude) %>%
  mutate(across(all_of(nlcd_cols),
                ~ abs(.x - lag(.x)), .names = "d_{.col}")) %>%
  ungroup() %>%
  filter(!is.na(d_NLCD_11)) %>%
  mutate(total_change = rowSums(across(starts_with("d_NLCD"))))

cat("\n== 1. Year-over-year pixel change (total abs. proportion change) ==\n")
yoy %>%
  summarise(
    mean_change   = mean(total_change),
    median_change = median(total_change),
    p90           = quantile(total_change, 0.90),
    p99           = quantile(total_change, 0.99),
    pct_unchanged = mean(total_change < 0.01) * 100,
    pct_major     = mean(total_change > 0.5) * 100   # majority of pixel converted
  ) %>% print()

cat("\nBy year (is change concentrated in specific years / NLCD vintages?):\n")
yoy %>%
  group_by(year) %>%
  summarise(mean_change = mean(total_change),
            pct_major   = mean(total_change > 0.5) * 100,
            n = n(), .groups = "drop") %>%
  print(n = Inf)

# ── 2. Which classes change, and where ────────────────────────────────────
cat("\n== 2. Mean absolute YoY change by class and zone ==\n")
yoy %>%
  filter(zone != "other") %>%
  group_by(zone) %>%
  summarise(across(starts_with("d_NLCD"), mean), .groups = "drop") %>%
  pivot_longer(-zone, names_to = "class", values_to = "mean_abs_change") %>%
  mutate(class = gsub("d_", "", class)) %>%
  group_by(zone) %>%
  slice_max(mean_abs_change, n = 6) %>%
  print(n = Inf)
# Reference: 21-24 developed (increasing intensity), 31 barren,
# 41-43 forest, 81 pasture, 82 crops, 90/95 wetlands.

# ── 3. Change relative to operational year ────────────────────────────────
# The construction-signature test: does developed/barren share jump and
# vegetation drop in the treatment ring during rel_year -2..0?

cat("\n== 3. Land-cover trajectory around opening (treatment ring) ==\n")
px %>%
  filter(zone == "treatment_ring(0-600m)",
         rel_year >= -5, rel_year <= 5) %>%
  mutate(developed  = NLCD_21 + NLCD_22 + NLCD_23 + NLCD_24,
         barren     = NLCD_31,
         vegetation = NLCD_41 + NLCD_42 + NLCD_43 + NLCD_71 +
           NLCD_81 + NLCD_82) %>%
  group_by(rel_year) %>%
  summarise(mean_developed = mean(developed),
            mean_barren    = mean(barren),
            mean_veg       = mean(vegetation),
            n_pixels = n(), .groups = "drop") %>%
  print(n = Inf)

cat("\nSame trajectory, control ring (should be flat if controls are clean):\n")
px %>%
  filter(zone == "control_ring(1000-7500m)",
         rel_year >= -5, rel_year <= 5) %>%
  mutate(developed  = NLCD_21 + NLCD_22 + NLCD_23 + NLCD_24,
         barren     = NLCD_31,
         vegetation = NLCD_41 + NLCD_42 + NLCD_43 + NLCD_71 +
           NLCD_81 + NLCD_82) %>%
  group_by(rel_year) %>%
  summarise(mean_developed = mean(developed),
            mean_barren    = mean(barren),
            mean_veg       = mean(vegetation),
            n_pixels = n(), .groups = "drop") %>%
  print(n = Inf)
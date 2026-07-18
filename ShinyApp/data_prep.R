# ============================================================
# data_prep.R — One-Time Data Preparation Script
# Run this script ONCE to download and save spatial data.
# Output RDS files are then read by global.R at app start.
# ============================================================

library(tidyverse)
library(sf)
library(tigris)
library(arcgislayers)
library(here)

options(tigris_use_cache = TRUE)

# # --- Virginia County Boundaries -----------------------------
# va_counties <- counties(state = "51", year = 2025)
# 
# # --- Bodies of Water (per county) ---------------------------
# va_water_list <- purrr::map(va_counties$COUNTYFP, ~ {
#   area_water(state = "51", county = .x, year = 2025)
# })
# va_water <- dplyr::bind_rows(va_water_list) %>%
#   st_transform(crs = 4326)
# 
# # --- Electric Transmission Lines (ArcGIS, last updated 2023) 
# url   <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/US_Electric_Power_Transmission_Lines/FeatureServer/0"
# layer <- arc_open(url)
# elecGridData <- arc_select(layer)
# 
# va_border <- st_union(va_counties) %>% st_transform(crs = 4326)
# va_lines  <- elecGridData %>%
#   st_transform(crs = 4326) %>%
#   st_intersection(va_border)   # clip to VA border
# 
# # --- Save to /data ------------------------------------------
# saveRDS(va_water, file = here("data", "clean01_va_water.rds"))
# saveRDS(va_lines, file = here("data", "clean01_va_elec_lines.rds"))

message("Data preparation complete. Files saved to /data.")


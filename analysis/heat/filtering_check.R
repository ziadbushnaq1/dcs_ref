library(tidyverse)
library(sf)
library(here)

source(here("analysis", "heat", "an_01_functions.R"))

check_global_conflicts <- function(classified_pixels, all_dcs_sf, max_treat_m = 600) {
  
  # Isolate only the treatment pixels to check their viability
  treat_pixels <- classified_pixels %>%
    filter(status == "Treatment") %>%
    distinct(export_id, pixel_id, pixel_x, pixel_y)
  
  # Convert those pixels into spatial points using the projected coords
  pixels_sf <- treat_pixels %>%
    st_as_sf(coords = c("pixel_x", "pixel_y"), crs = 5070)
  
  # Buffer the master list of all known data centers
  all_dcs_buffered <- all_dcs_sf %>%
    st_transform(5070) %>%
    st_buffer(dist = max_treat_m)
  
  # st_intersects returns a list of indices where each pixel hits a DC buffer
  intersections <- st_intersects(pixels_sf, all_dcs_buffered)
  
  # Count how many data center buffers overlap each pixel
  # 1 overlap = clean (it only overlaps its own host DC)
  # >1 overlap = conflicted (it hits a neighbor)
  treat_pixels$overlap_count <- lengths(intersections)
  
  # Aggregate the counts back to the data center level
  survival_stats <- treat_pixels %>%
    group_by(export_id) %>%
    summarize(
      original_pixels = n(),
      clean_pixels = sum(overlap_count == 1),
      conflicted_pixels = sum(overlap_count > 1),
      pct_surviving = round((clean_pixels / original_pixels) * 100, 1)
    ) %>%
    arrange(clean_pixels) # Sort lowest to highest to easily spot drop-offs
  
  return(survival_stats)
}


# 1. Load the absolute master list of ALL data centers
master_dcs <- read_csv(here("data", "data_final", "clean01_datacenter.csv")) %>% 
  filter(stage %in% c("Operational", "Under Construction")) %>% st_as_sf(coords = c("projected_x", "projected_y"), crs = 5070)

# Define which subset you want to test
ASSET_ID <- "landsat_hyperscale_all"

# Load the target data centers for this specific analysis
target_dcs <- read_csv(
  here("data", "data_final", "isolation_sets", paste0(ASSET_ID, ".csv")),
  show_col_types = FALSE
) %>% 
  st_as_sf(coords = c("projected_x", "projected_y"), crs = 5070)

# Load the raw pixel data for this specific analysis
raw_csv_data <- read_csv(
  here("data", "processed", ASSET_ID, "landsat_monthly_obs_lst_landcover.csv"),
  show_col_types = FALSE
)

# 2. Run your normal zone assignment (do not drop conflicts yet)
pixels_classified <- assign_treatment_zones(
  pixel_df = raw_csv_data, 
  dc_points = target_dcs, 
  max_treat_m = 600,
  sensor = "landsat" # Make sure to pass your sensor type here
)

# 3. Generate the survival report
viability_report <- check_global_conflicts(
  classified_pixels = pixels_classified, 
  all_dcs_sf = master_dcs, 
  max_treat_m = 600
)

write_csv(viability_report, here("data", "processed", "conflict_survival_report.csv"))

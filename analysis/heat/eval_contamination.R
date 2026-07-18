# eval_contamination.R
library(tidyverse)
library(sf)
library(duckdb)
library(here)
library(RANN)

HS <- c(411,412,648,664,2949,2950,2998,3012,3051,3052)

# 1. Load Facilities
# Master list used for checking SUTVA contamination (ALL facilities matter here)
master_dcs <- read_csv(here("data","data_final","clean01_datacenter.csv"), show_col_types = FALSE) %>%
  filter(!is.na(projected_x)) %>%
  select(master_id = export_id, projected_x, projected_y)

# Target list: ONLY Hyperscale facilities for the evaluation
dc_points <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"), show_col_types = FALSE) %>%
  filter(export_id %in% HS) %>% # <-- NEW: Hyperscale filter applied
  select(export_id, dc_x = projected_x, dc_y = projected_y)

# 2. Extract Unique Pixels via DuckDB 
cat("Extracting unique pixel coordinates...\n")
con <- dbConnect(duckdb())
f <- here("data","processed","landsat_all146","landsat_all146_obs30m_l89.csv")
hs_list <- paste(HS, collapse = ",")
# <-- NEW: DuckDB filtered to just the HS rows to save memory and time
px <- dbGetQuery(con, glue::glue("SELECT DISTINCT export_id, longitude, latitude FROM read_csv('{f}', ignore_errors=true) WHERE export_id IN ({hs_list})"))
dbDisconnect(con)

# Project pixels to match DC coordinates (EPSG 5070)
coords_matrix <- sf_project(from = st_crs(4326), to = st_crs(5070), pts = as.matrix(px[, c("longitude", "latitude")]))
px$px_x <- coords_matrix[, 1]
px$px_y <- coords_matrix[, 2]

# Join host DC coordinates to calculate base distances
px <- px %>%
  left_join(dc_points, by = "export_id") %>%
  mutate(dist_to_host = sqrt((px_x - dc_x)^2 + (px_y - dc_y)^2))

# 3. Find Contamination using Fast k-Nearest Neighbors
cat("Scanning for spatial contamination across master DC list...\n")
# If any master DC is within 600m, it creates a treatment zone.
nn <- nn2(data = as.matrix(master_dcs[, c("projected_x", "projected_y")]),
          query = as.matrix(px[, c("px_x", "px_y")]),
          k = 15, searchtype = "radius", radius = 600)

# Count how many total master DCs cover each pixel
px$n_treating_dcs <- rowSums(nn$nn.idx > 0)

# 4. Evaluate Mechanisms for Individual Facilities
cat("Evaluating control ratios per facility...\n")

radii_to_test <- list(
  "0_to_600" = c(0, 600),
  "0_to_300" = c(0, 300),
  "300_to_600" = c(300, 600)
)

results <- list()

for (r_name in names(radii_to_test)) {
  t_min <- radii_to_test[[r_name]][1]
  t_max <- radii_to_test[[r_name]][2]
  
  # Classify base assignment
  grid <- px %>%
    mutate(
      base_status = case_when(
        dist_to_host >= t_min & dist_to_host <= t_max ~ "Treat",
        dist_to_host >= 1000 & dist_to_host <= 1500 ~ "Control",
        TRUE ~ "Exclude"
      )
    ) %>% filter(base_status != "Exclude")
  
  # Apply Contamination Logic
  eval <- grid %>%
    mutate(
      # Control Hygiene: Must have ZERO treating DCs covering it
      is_clean_control = (base_status == "Control" & n_treating_dcs == 0),
      
      # Treatment Contamination: Overlaps >= 2 DCs (itself + another)
      is_clean_treat = (base_status == "Treat" & n_treating_dcs == 1),
      is_multi_treat = (base_status == "Treat" & n_treating_dcs > 1)
    ) %>%
    group_by(export_id) %>%
    summarise(
      ring = r_name,
      raw_control_px = sum(base_status == "Control"),
      raw_treat_px = sum(base_status == "Treat"),
      
      clean_control_px = sum(is_clean_control),
      
      # Mechanism 1: Drop Doubly Treated
      drop_treat_px = sum(is_clean_treat),
      
      # Mechanism 2: Additive (Keep multi-treated pixels for intensity score)
      additive_treat_px = sum(base_status == "Treat"),
      .groups = "drop"
    ) %>%
    mutate(
      ratio_raw = raw_control_px / max(raw_treat_px, 1),
      ratio_drop_mechanism = clean_control_px / max(drop_treat_px, 1),
      ratio_additive_mechanism = clean_control_px / max(additive_treat_px, 1)
    )
  
  results[[r_name]] <- eval
}

final_report <- bind_rows(results)
write_csv(final_report, here("results", "facility_contamination_ratios.csv"))
cat("Done. Ratios saved to results/facility_contamination_ratios.csv\n")
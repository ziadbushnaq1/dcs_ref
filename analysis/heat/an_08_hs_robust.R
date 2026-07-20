# an_08_hs_robust.R — hyperscale robustness: FE ladder, clustering,
# contamination scope/timing. All rows differ from the ANCHOR in ONE axis.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","an_08_grid_runner.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
setFixest_nthreads(8); data.table::setDTthreads(8)

d <- load_hyperscale_panel()
MASTER_SETS <- list(all = d$master_ops,
                    hs_only = d$master_ops %>% filter(capacity_type == "Hyperscaler"))
GROUPS <- list(hs = d$dc_points)
spatial_rings <- list(c(0,600))          # headline ring only

anchor <- tibble(grp = "hs", ring_idx = 1L, intens = TRUE,
                 use_construction = TRUE, mod_fe = "pixel_id + year",
                 cluster_var = "export_id", treat_scope = "all",
                 contam_scope = "all", contam_timing = "static",
                 contam_buffer = 0L)

# ── EDIT HERE: each row differs from the anchor in exactly one axis ─────
params <- bind_rows(
  anchor,                                                       # anchor
  anchor %>% mutate(mod_fe = "pixel_id + year^export_id"),      # FE ladder
  anchor %>% mutate(mod_fe = "pixel_id + year^month"),
  anchor %>% mutate(mod_fe = "pixel_id + year^sensor"),         # L5/L89 seam
  anchor %>% mutate(mod_fe = "pixel_id + year^campus_id"), 
  anchor %>% mutate(cluster_var = "export_id"),                 # clustering
  anchor %>% mutate(cluster_var = "none"), 
  anchor %>% mutate(treat_scope = "hs_only"),                   # dose roster
  anchor %>% mutate(contam_scope = "hs_only"),                  # drop roster
  anchor %>% mutate(contam_timing = "dynamic", contam_buffer = 0L),  # timing
  anchor %>% mutate(contam_timing = "dynamic", contam_buffer = 3L))
                    
# ────────────────────────────────────────────────────────────────────────

run_ring_grid(params, d$pixel_data, GROUPS, MASTER_SETS, spatial_rings,
              ctrl_min = 1000, ctrl_max = 1500, ref_year = -4,
              out_csv = here("results","hs_robust_headline.csv"))
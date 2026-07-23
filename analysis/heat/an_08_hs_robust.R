# an_08_hs_robust.R — hyperscale robustness: FE ladder, clustering,
# contamination scope/timing. All rows differ from the ANCHOR in ONE axis.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","an_08_grid_runner.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
setFixest_nthreads(8); data.table::setDTthreads(8)

d <- load_hyperscale_panel()
master_full <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                        show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
cat("Inventory stages:\n"); print(count(st_drop_geometry(master_full), stage))

master_ops <- master_full %>% filter(stage == "Operational")

MASTER_SETS <- list(
  all     = master_ops,                                   # dose + default drop
  hs_only = master_ops %>% filter(capacity_type == "Hyperscaler"),
  # Contamination-only roster: construction sites emit heat/disturbance
  # (~0.2C in our own estimates) but are excluded from `all` because the
  # stage filter targets operational facilities. NEVER use as treat_scope:
  # construction-stage facilities have no operational dose.
  all_oc  = master_full %>% filter(stage %in% c("Operational", "Under Construction")))
GROUPS <- list(hs = d$dc_points)
spatial_rings <- list(c(0,600))          # headline ring only

anchor <- tibble(grp = "hs", ring_idx = 1L, intens = TRUE,
                 use_construction = TRUE, mod_fe = "pixel_id + year",
                 cluster_var = "export_id", treat_scope = "hs_only",
                 contam_scope = "all_oc", contam_timing = "dynamic",
                 contam_buffer = 0L)

# ── EDIT HERE: each row differs from the anchor in exactly one axis ─────
specs <- bind_rows(
  anchor,
  anchor %>% mutate(contam_timing = "static"),
  anchor %>% mutate(contam_timing = "dynamic", contam_buffer = 3L),
  anchor %>% mutate(contam_scope = "all"),
  anchor %>% mutate(contam_scope = "hs_only"),
  anchor %>% mutate(treat_scope = "all"),
  anchor %>% mutate(mod_fe = "pixel_id + year^export_id"),
  anchor %>% mutate(mod_fe = "pixel_id + year^month"),
  anchor %>% mutate(mod_fe = "pixel_id + year^sensor"),
  anchor %>% mutate(mod_fe = "pixel_id + year^campus_id"))

params <- bind_rows(specs, anchor %>% mutate(cluster_var = "campus_id"), anchor %>% mutate(cluster_var = "none"))
                    
# ────────────────────────────────────────────────────────────────────────

run_ring_grid(params, d$pixel_data, GROUPS, MASTER_SETS, spatial_rings,
              ctrl_min = 1000, ctrl_max = 1500, ref_year = -4,
              out_csv = here("results","hs_robust_headline.csv"))

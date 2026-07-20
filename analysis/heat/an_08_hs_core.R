# an_08_hs_core.R — hyperscale-17, main ring grid, default config only.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","an_08_grid_runner.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
setFixest_nthreads(8); data.table::setDTthreads(8)

d <- load_hyperscale_panel()
MASTER_SETS <- list(all = d$master_ops,
                    hs_only = d$master_ops %>% filter(capacity_type == "Hyperscaler"))
GROUPS <- list(hs = d$dc_points)
spatial_rings <- list(c(0,150), c(0,300), c(300,600), c(0,600))

# ── EDIT HERE: shrink by commenting values out ──────────────────────────
params <- expand.grid(
  grp = "hs",
  ring_idx = 1:4,
  intens = c(FALSE, TRUE),
  use_construction = c(FALSE, TRUE),
  mod_fe = "pixel_id + year",
  stringsAsFactors = FALSE)
# ────────────────────────────────────────────────────────────────────────

run_ring_grid(params, d$pixel_data, GROUPS, MASTER_SETS, spatial_rings,
              ctrl_min = 1000, ctrl_max = 1500, ref_year = -4,
              out_csv = here("results","hs_core_ring_grid.csv"))
# an_08_all_core.R — 145-DC sample, main ring grid, groups all + non_hs.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","an_08_grid_runner.R"))
setFixest_nthreads(8); data.table::setDTthreads(8)

# ── Load: cloud filter + best-date selection in DuckDB ──────────────────
f <- here("data","processed","landsat_all146","landsat_all146_obs30m_l89.csv")
con <- dbConnect(duckdb())
dbExecute(con, "SET memory_limit='24GB'")
pixel_data <- dbGetQuery(con, glue::glue("
  WITH src AS (
    SELECT export_id, longitude, latitude, year, month, date_yyyymmdd,
           LST_Celsius, Emissivity, ST_uncertainty, Elevation, scene_cloud_cover
    FROM read_csv('{f}', ignore_errors=true)
    WHERE scene_cloud_cover < 30
  ),
  counts AS (
    SELECT export_id, year, month, date_yyyymmdd,
           COUNT(*) FILTER (WHERE LST_Celsius IS NOT NULL) AS n_valid
    FROM src GROUP BY 1,2,3,4
  ),
  best AS (
    SELECT export_id, year, month, date_yyyymmdd FROM (
      SELECT *, ROW_NUMBER() OVER (PARTITION BY export_id, year, month
              ORDER BY n_valid DESC, date_yyyymmdd ASC) rk FROM counts) WHERE rk=1
  )
  SELECT src.* FROM src JOIN best USING (export_id, year, month, date_yyyymmdd)"))
dbDisconnect(con)
cat("All-DC panel rows:", format(nrow(pixel_data), big.mark=","),
    "| DCs:", n_distinct(pixel_data$export_id),
    "| max year:", max(pixel_data$year), "\n")
cat("All-DC max year:", max(pixel_data$year), "\n")   # window check vs hyperscale

dc_points_all <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"),
                          show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
master_ops <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                       show_col_types = FALSE) %>%
  filter(stage == "Operational") %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
HS <- read_csv(here("data","data_final","hyperscale_roster.csv"),
               show_col_types = FALSE) %>% pull(export_id)

MASTER_SETS <- list(all = master_ops,
                    hs_only = master_ops %>% filter(capacity_type == "Hyperscaler"))
GROUPS <- list(all = dc_points_all,
               non_hs = dc_points_all %>% filter(!export_id %in% HS))
spatial_rings <- list(c(0,150), c(0,300), c(300,600), c(0,600))

# ── EDIT HERE ───────────────────────────────────────────────────────────
params <- expand.grid(
  grp = c("all","non_hs"),
  ring_idx = 1:4,
  intens = c(FALSE, TRUE),
  use_construction = c(FALSE, TRUE),
  mod_fe = "pixel_id + year",
  stringsAsFactors = FALSE)
# ────────────────────────────────────────────────────────────────────────

run_ring_grid(params, pixel_data, GROUPS, MASTER_SETS, spatial_rings,
              ctrl_min = 1000, ctrl_max = 1500, ref_year = -4,
              out_csv = here("results","all_core_ring_grid.csv"))
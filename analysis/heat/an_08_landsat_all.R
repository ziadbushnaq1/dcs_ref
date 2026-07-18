# an_08_landsat_all.R — all 145 DCs at 30m, binary vs intensity, ring grid
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
options(bitmapType = "cairo")
setFixest_nthreads(8); data.table::setDTthreads(8)

f <- here("data","processed","landsat_all146","landsat_all146_obs30m_l89.csv")

# ── Load: best-date selection + cloud filter in DuckDB; R never sees 232M rows
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
cat("All-DC 30m panel rows:", format(nrow(pixel_data), big.mark=","),
    "| DCs:", n_distinct(pixel_data$export_id), "\n")
cat("ST_uncertainty summary:\n"); print(summary(pixel_data$ST_uncertainty))

dc_points_all <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"),
                          show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

master_ops <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                       show_col_types = FALSE) %>%
  filter(stage == "Operational") %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

HS <- read_csv(here("data","data_final","hyperscale_roster.csv"),
               show_col_types = FALSE) %>% pull(export_id)

# ── Subgroups: full sample + the heterogeneity split ────────────────────
GROUPS <- list(
  all       = dc_points_all,
  non_hs    = dc_points_all %>% filter(!export_id %in% HS)
  # hyperscale-only already covered by an_08_prelim_hyperscale.R
)

# ── Leaner grid than hyperscale version: 145 DCs x 30m = heavy models ───
spatial_rings <- list(c(0, 150), c(0, 300), c(300, 600), c(0, 600))
params <- expand.grid(
  grp = names(GROUPS),
  ring_idx = seq_along(spatial_rings),
  intens = c(FALSE, TRUE),
  use_construction = c(FALSE, TRUE),
  mod_fe = c("pixel_id + year"),
  stringsAsFactors = FALSE
)
# year^export_id only for the primary ring (expensive at 145 DCs)
params <- bind_rows(params,
                    expand.grid(grp = names(GROUPS), ring_idx = 4L,  
                                intens = c(FALSE, TRUE),
                                use_construction = c(FALSE, TRUE),
                                mod_fe = "pixel_id + year^export_id",
                                stringsAsFactors = FALSE))

params <- bind_rows(params,
                    expand.grid(grp = names(GROUPS), ring_idx = 3L,   # match your headline ring
                                intens = c(FALSE, TRUE),
                                use_construction = c(FALSE, TRUE),
                                mod_fe = "pixel_id + year^month",
                                stringsAsFactors = FALSE) %>%
                      mutate(drop_unknown_controls = TRUE))

# All existing rows use the default (drop unknown-neighbor controls)
params$drop_unknown_controls <- TRUE

# One extra row: headline cell only (all-DC group, ring 300-600, intensity,
# construction dummy, baseline FE) with unknown-neighbor controls KEPT.
# ring_idx = 3L because spatial_rings[[3]] is c(300, 600).
params <- bind_rows(params,
                    tibble(grp = "all", ring_idx = 3L,
                           intens = TRUE, use_construction = TRUE,
                           mod_fe = "pixel_id + year",
                           drop_unknown_controls = FALSE))

CTRL_MIN <- 1000; CTRL_MAX <- 1500; REF_YEAR <- -4;
out <- list()

if (REF_YEAR < -1) {
  cat("\n=======================================================================\n")
  cat(" TIMELINE ALERT: Baseline explicitly anchored at year", REF_YEAR, "\n")
  cat(" -> Models with use_construction = TRUE : Transition years absorbed by dummy.\n")
  cat(" -> Models with use_construction = FALSE: Transition years physically dropped (Donut).\n")
  cat("=======================================================================\n")
} else {
  cat("\n=======================================================================\n")
  cat(" TIMELINE ALERT: Standard Event Study Baseline (Ref Year", REF_YEAR, ")\n")
  cat("=======================================================================\n")
}

cat("\n==== All-DC ring loop:", nrow(params), "models ====\n\n")

for (i in seq_len(nrow(params))) {
  ring   <- spatial_rings[[params$ring_idx[i]]]
  grp    <- params$grp[i]
  intens <- params$intens[i]
  constr_flag <- params$use_construction[i]
  mod_fe <- params$mod_fe[i]
  unk_flag <- params$drop_unknown_controls[i]
  cat(sprintf("[%d/%d] Group: %s | Ring: %d-%dm | Intens: %s | Construction: %s | FE: %s\n",
              i, nrow(params), grp, ring[1], ring[2], intens, constr_flag, mod_fe))
  
  res <- tryCatch(
    run_pixel_analysis(
      raw_csv_data = pixel_data, dc_points = GROUPS[[grp]],
      min_treat_m = ring[1], max_treat_m = ring[2],
      min_control_m = CTRL_MIN, max_control_m = CTRL_MAX,
      fe_spec = mod_fe, use_power_builtout = FALSE,
      filter_elev = FALSE, elev_threshold = 50, is_event_study = FALSE,
      ref_year = REF_YEAR, use_intensity = intens,
      master_dcs = if (intens) master_ops else NULL,
      sensor = "landsat_monthly",
      use_construction = constr_flag,
      drop_unknown_controls = unk_flag),
    error = function(e) { message(" -> SKIP: ", conditionMessage(e)); NULL })
  if (is.null(res)) next
  
  ct <- summary(res$model)$coeftable
  counts_df <- res$counts
  
  for (tm in intersect(c("treated_post","construction_period"), rownames(ct))) {
    out[[length(out)+1]] <- tibble(
      group = grp, 
      ring_min = ring[1],
      ring_max = ring[2],
      ring_label = paste0(ring[1],"-",ring[2],"m"),
      intensity = intens,
      use_construction = constr_flag,,
      drop_unknown_controls = unk_flag,
      fe_spec = mod_fe, 
      ref_year = REF_YEAR,
      term = tm,
      estimate = ct[tm,"Estimate"], 
      se = ct[tm,"Std. Error"],
      p_value = ct[tm,"Pr(>|t|)"], 
      n_obs = res$model$nobs
    ) %>% bind_cols(counts_df) 
  }
  rm(res, ct); gc()
}

results <- bind_rows(out)

cat("\n\n==== Results: Baseline Models (use_construction = FALSE) ====\n")
print(knitr::kable(
  results %>% 
    filter(use_construction == FALSE) %>% 
    arrange(group, fe_spec, ref_year, intensity, ring_min, ring_max, desc(term)), 
  digits = 3
))

cat("\n\n==== Results: Construction Models (use_construction = TRUE) ====\n")
print(knitr::kable(
  results %>% 
    filter(use_construction == TRUE) %>% 
    arrange(group, fe_spec, ref_year, intensity, ring_min, ring_max, desc(term)), 
  digits = 3
))

write_csv(results, here("results","all145_30m_ring_grid.csv"))
cat("==== Done ====\n")
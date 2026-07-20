# an_08_all_seasonal.R — seasonal heterogeneity, ALL-DC sample (145 clusters).
# PREDICTION (pre-stated, matching hyperscale result): summer > pooled >
# shoulder > winter, with winter near zero. 145 clusters give the power to
# estimate the gradient precisely that the 17-cluster sample lacks.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
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

dc_points_all <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"),
                          show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
master_ops <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                       show_col_types = FALSE) %>%
  filter(stage == "Operational") %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
MASTER_SETS <- list(all = master_ops)

# Headline cell held fixed; ONLY season varies across rows.
RING <- c(0, 600); CTRL_MIN <- 1000; CTRL_MAX <- 1500
REF_YEAR <- -4; FE <- "pixel_id + year"

SEASONS <- list(pooled = 1:12, summer = 6:8,
                winter = c(12, 1, 2), shoulder = c(3, 4, 5, 9, 10, 11))

out <- list()
for (s in names(SEASONS)) {
  mons <- SEASONS[[s]]
  dat  <- pixel_data %>% filter(month %in% mons)
  cat(sprintf("[%s] rows: %s\n", s, format(nrow(dat), big.mark=",")))
  
  res <- tryCatch(
    run_pixel_analysis(
      raw_csv_data = dat, dc_points = dc_points_all,
      min_treat_m = RING[1], max_treat_m = RING[2],
      min_control_m = CTRL_MIN, max_control_m = CTRL_MAX,
      fe_spec = FE, filter_elev = FALSE, is_event_study = FALSE,
      ref_year = REF_YEAR, use_intensity = TRUE,
      master_dcs = MASTER_SETS$all,
      sensor = "landsat_monthly", use_construction = TRUE,
      cluster_var = "export_id",
      contamination_master = MASTER_SETS$all,
      contam_timing = "static"),
    error = function(e) { message(" -> SKIP: ", conditionMessage(e)); NULL })
  if (is.null(res)) next
  
  ct <- summary(res$model)$coeftable
  for (tm in intersect(c("treated_post","construction_period"), rownames(ct))) {
    out[[length(out)+1]] <- tibble(
      group = "all", season = s, months = paste(mons, collapse=","),
      ring_label = "0-600m", intensity = TRUE, use_construction = TRUE,
      fe_spec = FE, ref_year = REF_YEAR,
      contam_scope = "all", contam_timing = "static", term = tm,
      estimate = ct[tm,"Estimate"], se = ct[tm,"Std. Error"],
      p_value = ct[tm,"Pr(>|t|)"], n_obs = res$model$nobs
    ) %>% bind_cols(res$counts)
  }
  rm(res, ct, dat); gc()
}

results <- bind_rows(out)
cat("\n==== All-DC Seasonal Heterogeneity ====\n")
print(knitr::kable(results %>%
                     arrange(term, factor(season, names(SEASONS))), digits = 3))
write_csv(results, here("results","all_seasonal_headline.csv"))
cat("==== Done ====\n")
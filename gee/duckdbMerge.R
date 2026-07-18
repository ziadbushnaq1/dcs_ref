# gee_merge.R is crashing R session for landsat — DuckDB does the joins on disk

library(duckdb)
library(here)

con <- dbConnect(duckdb())

# ==============================================================================
# Loop 1: 150m Landsat monthly obs + NLCD join (original isolation assets)
# ==============================================================================
for (asset_id in c('landsat_hyperscale_all', 'landsat_iso10000m',
                   'landsat_iso7500m', 'landsat_iso5000m', 'landsat_iso2500m')) {
  
  asset_raw_dir <- here('data', 'raw', asset_id)
  asset_out_dir <- here('data', 'processed', asset_id)
  dir.create(asset_out_dir, showWarnings = FALSE, recursive = TRUE)
  
  nlcd_files <- list.files(asset_raw_dir,
                           pattern = paste0("^", asset_id, "_NLCD_150m_Batch_.*\\.csv$"),
                           full.names = TRUE)
  obs_files  <- list.files(asset_raw_dir,
                           pattern = paste0("^", asset_id, "_Landsat_Monthly_Obs_Batch_.*\\.csv$"),
                           full.names = TRUE)
  
  # Filter out 2-byte empty files
  nlcd_files <- nlcd_files[file.size(nlcd_files) > 100]
  obs_files  <- obs_files[file.size(obs_files)   > 100]
  
  if (length(obs_files) == 0) {
    message(asset_id, " — no Monthly Obs files found, skipping")
    next
  }
  
  nlcd_list <- paste0("'", nlcd_files, "'", collapse = ", ")
  obs_list  <- paste0("'",  obs_files, "'", collapse = ", ")
  
  out_path <- file.path(asset_out_dir, "landsat_monthly_obs_lst_landcover.csv")
  
  dbExecute(con, glue::glue("
    COPY (
      SELECT
        obs.*,
        nlcd.* EXCLUDE (longitude, latitude, year, export_id, Elevation, \"system:index\", \".geo\")
      FROM read_csv([{obs_list}],  ignore_errors=true) AS obs
      LEFT JOIN read_csv([{nlcd_list}], ignore_errors=true) AS nlcd
        ON  round(obs.longitude,  6) = round(nlcd.longitude,  6)
        AND round(obs.latitude,   6) = round(nlcd.latitude,   6)
        AND obs.year      = nlcd.year
        AND obs.export_id = nlcd.export_id
    ) TO '{out_path}' (HEADER, DELIMITER ',')
  "))
  
  message(asset_id, " — written to ", out_path)
}

# ==============================================================================
# Loop 2: 30m per-DC exports — L8/9-only file + L5+L8/9 unified file
# No NLCD join here (30m NLCD merges separately; joined at analysis time)
# ==============================================================================
for (asset_id in c('landsat_all146', 'landsat_pre2014', 'landsat_hs_extra')) {
  
  asset_raw_dir <- here('data', 'raw', asset_id)
  asset_out_dir <- here('data', 'processed', asset_id)
  if (!dir.exists(asset_raw_dir)) { message(asset_id, " — no raw dir, skipping"); next }
  dir.create(asset_out_dir, showWarnings = FALSE, recursive = TRUE)
  
  l89_files <- list.files(asset_raw_dir,
                          pattern = paste0("^", asset_id, "_Obs30m(_2026)?_DC.*\\.csv$"),
                          full.names = TRUE)
  l5_files  <- list.files(asset_raw_dir,
                          pattern = paste0("^", asset_id, "_L5Obs30m_DC.*\\.csv$"),
                          full.names = TRUE)
  l89_files <- l89_files[file.size(l89_files) > 100]
  l5_files  <- l5_files[file.size(l5_files)   > 100]
  
  if (length(l89_files) == 0 && length(l5_files) == 0) {
    message(asset_id, " — no 30m files found, skipping")
    next
  }
  
  out_l89 <- file.path(asset_out_dir, paste0(asset_id, "_obs30m_l89.csv"))
  out_uni <- file.path(asset_out_dir, paste0(asset_id, "_obs30m_unified.csv"))
  
  # --- L8/9-only file (primary modern dataset) ---
  if (length(l89_files) > 0) {
    l89_list <- paste0("'", l89_files, "'", collapse = ", ")
    dbExecute(con, glue::glue("
      COPY (SELECT *, 8 AS sensor
            FROM read_csv([{l89_list}], ignore_errors=true, union_by_name=true))
      TO '{out_l89}' (HEADER, DELIMITER ',')"))
    message(asset_id, " — L8/9 merged: ", length(l89_files), " files -> ", out_l89)
  }
  
  # --- Unified file (L5 + L8/9) ---
  if (length(l5_files) > 0) {
    l5_list <- paste0("'", l5_files, "'", collapse = ", ")
    if (length(l89_files) > 0) {
      dbExecute(con, glue::glue("
        COPY (
          SELECT *, 8 AS sensor
          FROM read_csv([{l89_list}], ignore_errors=true, union_by_name=true)
          UNION ALL BY NAME
          SELECT *
          FROM read_csv([{l5_list}], ignore_errors=true, union_by_name=true)
        ) TO '{out_uni}' (HEADER, DELIMITER ',')"))
    } else {
      dbExecute(con, glue::glue("
        COPY (SELECT *
              FROM read_csv([{l5_list}], ignore_errors=true, union_by_name=true))
        TO '{out_uni}' (HEADER, DELIMITER ',')"))
    }
    message(asset_id, " — unified merged (L5: ", length(l5_files),
            " + L89: ", length(l89_files), " files) -> ", out_uni)
  }
  
  nlcd30_files <- list.files(asset_raw_dir,
                             pattern = paste0("^", asset_id, "_NLCD_30m_Batch_.*\\.csv$"),
                             full.names = TRUE)
  nlcd30_files <- nlcd30_files[file.size(nlcd30_files) > 100]
  if (length(nlcd30_files) > 0) {
    nlcd30_list <- paste0("'", nlcd30_files, "'", collapse = ", ")
    out_nlcd <- file.path(asset_out_dir, paste0(asset_id, "_nlcd30m.csv"))
    dbExecute(con, glue::glue("
      COPY (SELECT * FROM read_csv([{nlcd30_list}], ignore_errors=true, union_by_name=true))
      TO '{out_nlcd}' (HEADER, DELIMITER ',')"))
    message(asset_id, " — NLCD 30m lookup: ", length(nlcd30_files), " files")
  }
}

dbDisconnect(con)
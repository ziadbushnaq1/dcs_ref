library(tidyverse)
library(here)

# ==============================================================================
# Config — mirror the ASSETS dict from dc_lst_run.ipynb
# ==============================================================================


ASSETS <- list(
  modis   = c('modis_hyperscale_all',
              'modis_iso10000m', 'modis_iso7500m',
              'modis_iso5000m',  'modis_iso2500m')#,
  # landsat = c('landsat_hyperscale_all',
  #             'landsat_iso10000m', 'landsat_iso7500m',
  #             'landsat_iso5000m',  'landsat_iso2500m')
)

raw_dir  <- here('data', 'raw')
out_dir  <- here('data', 'processed')
id_col   <- "export_id"
coord_digits <- 6

# ==============================================================================
# Helpers (unchanged)
# ==============================================================================

read_batches <- function(task_name_prefix, dir) {
  files <- list.files(
    dir,
    pattern = paste0("^", task_name_prefix, "_Batch_.*\\.csv$"),
    full.names = TRUE
  )
  # GEE produces 2-byte header-only CSVs for batches whose data center
  # slice was empty (happens when TOTAL_BATCHES > number of data centers).
  # Filter these out before reading to prevent read_csv parse errors.
  files <- files[file.size(files) > 100]
  
  if (length(files) == 0) {
    stop("No files found for '", task_name_prefix, "' in ", dir)
  }
  message("Reading ", length(files), " file(s) for ", task_name_prefix)
  map_dfr(files, read_csv, show_col_types = FALSE)
}

round_coords <- function(df, digits = coord_digits) {
  df %>%
    mutate(
      longitude = round(longitude, digits),
      latitude  = round(latitude, digits)
    )
}

# ==============================================================================
# MODIS loop
# ==============================================================================

for (asset_id in ASSETS$modis) {
  message("\n", paste(rep("=", 50), collapse = ""))
  message("Merging MODIS exports for: ", asset_id)
  message(paste(rep("=", 50), collapse = ""))
  
  asset_raw_dir <- file.path(raw_dir, asset_id)
  asset_out_dir <- file.path(out_dir, asset_id)
  dir.create(asset_out_dir, showWarnings = FALSE, recursive = TRUE)
  
  join_keys <- c("longitude", "latitude", "year", id_col)
  
  # Skip this asset if exports haven't been pulled yet
  if (!dir.exists(asset_raw_dir) || length(list.files(asset_raw_dir)) == 0) {
    message("No files found for ", asset_id, " — skipping")
    next
  }
  
  # NLCD 1000m (shared join target for day + night)
  nlcd_1000 <- read_batches(paste0(asset_id, "_NLCD_1000m"), asset_raw_dir) %>%
    round_coords() %>%
    select(-Elevation)
  
  # Monthly day
  modis_monthly_day <- read_batches(
    paste0(asset_id, "_MODIS_Monthly_Day"), asset_raw_dir) %>%
    round_coords()
  
  # Monthly night
  modis_monthly_night <- read_batches(
    paste0(asset_id, "_MODIS_Monthly_Night"), asset_raw_dir) %>%
    round_coords() %>%
    rename(LST_Celsius = LST_Night_Celsius)
  
  # Monthly day + NLCD
  modis_monthly_day <- modis_monthly_day %>%
    left_join(nlcd_1000, by = join_keys) %>%
    mutate(low_confidence_day = n_clear_days < 3)
  
  message(
    asset_id, " MODIS day rows: ", nrow(modis_monthly_day),
    " | matched to land cover: ", sum(!is.na(modis_monthly_day$NLCD_11)),
    " | unmatched: ", sum(is.na(modis_monthly_day$NLCD_11))
  )
  
  write_csv(modis_monthly_day,
            file.path(asset_out_dir, "modis_monthly_day_lst_landcover.csv"))
  
  # Monthly night + NLCD
  modis_monthly_night <- modis_monthly_night %>%
    left_join(nlcd_1000, by = join_keys) %>%
    mutate(low_confidence_night = n_clear_days < 3)
  
  message(
    asset_id, " MODIS night rows: ", nrow(modis_monthly_night),
    " | matched to land cover: ", sum(!is.na(modis_monthly_night$NLCD_11)),
    " | unmatched: ", sum(is.na(modis_monthly_night$NLCD_11))
  )
  
  write_csv(modis_monthly_night,
            file.path(asset_out_dir, "modis_monthly_night_lst_landcover.csv"))
  
  # Monthly best day — single best-quality observation per pixel per month
  # from the first 7 days. No averaging: each row is one real scene.
  # date_yyyymmdd records which day the pixel came from.
  best_path <- file.path(asset_raw_dir,
                         paste0(asset_id, "_MODIS_Monthly_Best_Batch_1.csv"))
  
  if (file.exists(best_path)) {
    modis_monthly_best <- read_batches(
      paste0(asset_id, "_MODIS_Monthly_Best"), asset_raw_dir) %>%
      round_coords()
    
    # Diagnostic — add temporarily after round_coords() on modis_monthly_best
    cat("\n--- Best join diagnostic for", asset_id, "---\n")
    cat("Best columns:   ", paste(names(modis_monthly_best), collapse=", "), "\n")
    cat("NLCD columns:   ", paste(names(nlcd_1000), collapse=", "), "\n")
    cat("Best year range:", range(modis_monthly_best$year), "\n")
    cat("NLCD year range:", range(nlcd_1000$year), "\n")
    cat("Best export_ids (sample):", head(unique(modis_monthly_best$export_id), 3), "\n")
    cat("NLCD export_ids (sample):", head(unique(nlcd_1000$export_id), 3), "\n")
    cat("Best lon sample:", head(modis_monthly_best$longitude, 3), "\n")
    cat("NLCD lon sample:", head(nlcd_1000$longitude[nlcd_1000$export_id == modis_monthly_best$export_id[1]], 3), "\n")
    
    modis_monthly_best <- modis_monthly_best %>% left_join(nlcd_1000, by = join_keys)
    
    message(
      asset_id, " MODIS best rows: ", nrow(modis_monthly_best),
      " | matched to land cover: ", sum(!is.na(modis_monthly_best$NLCD_11)),
      " | unmatched: ", sum(is.na(modis_monthly_best$NLCD_11))
    )
    
    write_csv(modis_monthly_best,
              file.path(asset_out_dir, "modis_monthly_best_lst_landcover.csv"))
  } else {
    message(asset_id, " MODIS monthly best — no files found, skipping")
  }
  
  # Daily MODIS — no NLCD join here; join happens at analysis stage
  # using the year column, same as Landsat individual observations.
  daily_path <- file.path(asset_raw_dir,
                          paste0(asset_id, "_MODIS_Daily_Obs_Batch_1.csv"))
  if (file.exists(daily_path)) {
    modis_daily <- read_batches(
      paste0(asset_id, "_MODIS_Daily_Obs"), asset_raw_dir) %>%
      round_coords()
    
    message(
      asset_id, " MODIS daily rows: ", nrow(modis_daily)
    )
    
    write_csv(modis_daily,
              file.path(asset_out_dir, "modis_daily_obs.csv"))
  } else {
    message(asset_id, " MODIS daily — no files found, skipping")
  }
  # At end of MODIS loop body, before closing }
  rm(nlcd_1000, modis_monthly_day, modis_monthly_night)
  if (exists("modis_monthly_best")) rm(modis_monthly_best)
  if (exists("modis_daily"))        rm(modis_daily)
  gc()
}

# ==============================================================================
# Landsat loop
# ==============================================================================

for (asset_id in ASSETS$landsat) {
  message("\n", paste(rep("=", 50), collapse = ""))
  message("Merging Landsat exports for: ", asset_id)
  message(paste(rep("=", 50), collapse = ""))
  
  asset_raw_dir <- file.path(raw_dir, asset_id)
  asset_out_dir <- file.path(out_dir, asset_id)
  dir.create(asset_out_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (!dir.exists(asset_raw_dir) || length(list.files(asset_raw_dir)) == 0) {
    message("No files found for ", asset_id, " — skipping")
    next
  }
  
  nlcd_150 <- read_batches(paste0(asset_id, "_NLCD_150m"), asset_raw_dir) %>%
    round_coords() %>%
    select(-Elevation)
  
  join_keys_annual <- c("longitude", "latitude", "year", id_col)
  join_keys_obs    <- c("longitude", "latitude", "year", id_col)
  
  # Monthly Landsat mean — backward compat, not actively used since switching
  # to landsat_monthly_obs. Checked independently so its absence doesn't
  # block the blocks below.
  monthly_path <- file.path(asset_raw_dir,
                            paste0(asset_id, "_Landsat_Monthly_Day_Batch_1.csv"))
  if (file.exists(monthly_path)) {
    landsat_monthly <- read_batches(
      paste0(asset_id, "_Landsat_Monthly_Day"), asset_raw_dir) %>%
      round_coords() %>%
      left_join(nlcd_150, by = join_keys_annual) %>%
      mutate(low_obs_month = n_obs < 2)
    message(asset_id, " Landsat monthly day rows: ", nrow(landsat_monthly))
    write_csv(landsat_monthly,
              file.path(asset_out_dir, "landsat_monthly_day_lst_landcover.csv"))
  } else {
    message(asset_id, " Landsat monthly day — no files found, skipping")
  }
  
  # Monthly obs (first 14 days) — independent of monthly_day.
  # Scene selection done in R via per-DC best-date logic in an_04/an_05.
  monthly_obs_path <- file.path(asset_raw_dir,
                                paste0(asset_id, "_Landsat_Monthly_Obs_Batch_1.csv"))
  if (file.exists(monthly_obs_path)) {
    landsat_monthly_obs <- read_batches(
      paste0(asset_id, "_Landsat_Monthly_Obs"), asset_raw_dir) %>%
      round_coords() %>%
      left_join(nlcd_150, by = join_keys_annual)
    message(
      asset_id, " Landsat monthly obs rows: ", nrow(landsat_monthly_obs),
      " | matched to land cover: ", sum(!is.na(landsat_monthly_obs$NLCD_11)),
      " | unmatched: ", sum(is.na(landsat_monthly_obs$NLCD_11))
    )
    write_csv(landsat_monthly_obs,
              file.path(asset_out_dir, "landsat_monthly_obs_lst_landcover.csv"))
  } else {
    message(asset_id, " Landsat monthly obs — no files found, skipping")
  }
  
  # Individual Landsat observations — independent of monthly_day.
  obs_path <- file.path(asset_raw_dir,
                        paste0(asset_id, "_Landsat_Obs_Batch_1.csv"))
  if (file.exists(obs_path)) {
    landsat_obs <- read_batches(
      paste0(asset_id, "_Landsat_Obs"), asset_raw_dir) %>%
      round_coords() %>%
      left_join(nlcd_150, by = join_keys_obs)
    message(
      asset_id, " Landsat obs rows: ", nrow(landsat_obs),
      " | matched to land cover: ", sum(!is.na(landsat_obs$NLCD_11)),
      " | unmatched: ", sum(is.na(landsat_obs$NLCD_11))
    )
    write_csv(landsat_obs,
              file.path(asset_out_dir, "landsat_obs_lst_landcover.csv"))
  } else {
    message(asset_id, " Landsat individual obs — no files found, skipping")
  }
  # Free memory before next asset — Landsat obs files are large
  # and R does not garbage-collect automatically between iterations.
  rm(nlcd_150)
  if (exists("landsat_monthly_obs")) rm(landsat_monthly_obs)
  if (exists("landsat_monthly"))     rm(landsat_monthly)
  if (exists("landsat_obs"))         rm(landsat_obs)
  gc()
}
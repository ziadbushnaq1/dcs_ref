library(tidyverse)
library(sf)
library(exactextractr)
library(fixest)

build_did_panel <- function(classified_pixels, use_power_builtout = FALSE, use_intensity = FALSE, sensor = "landsat") {
  
  group_cols <- if (sensor == "modis") {
    c("export_id", "year", "month")
  } else if (sensor %in% c("modis_daily", "landsat_daily")) {
    c("export_id", "date_yyyymmdd")
  } else if (sensor == "landsat_monthly") {
    c("export_id", "year", "month") 
  } else {
    c("export_id", "year")
  }
  
  panel_data <- classified_pixels %>%
    st_drop_geometry()
  
  if (!"n_treating" %in% names(panel_data)) {
    panel_data$n_treating <- 0
  }
  
  panel_data <- panel_data %>% 
    mutate(
      year = as.numeric(year),
      treated_post = case_when(
        use_intensity & status == "Treatment" ~ n_treating,
        status == "Treatment" & !is.na(year_operational) & year >= year_operational & use_power_builtout ~ log(power_builtout + 1),
        status == "Treatment" & !is.na(year_operational) & year >= year_operational & !use_power_builtout ~ 1,
        TRUE ~ 0
      ),
      # construction indicator, rel years -3..-1.
      # NLCD trajectories (check_nlcd_change.R, all 4 Landsat iso assets) show
      # disturbance from rel_year -3 and barren peak at -1/0; -5/-4 are clean.
      # Year 0 is already in treated_post, so construction covers -3 ... -1 only.
      construction_period = case_when(
        status == "Treatment" & !is.na(year_operational) &
          year >= year_operational - 3 & year < year_operational ~ 1,
        TRUE ~ 0
      ),
      relative_year = year - year_operational,
      treat_assignment = case_when(
        status == "Treatment" & use_power_builtout ~ log(power_builtout + 1),
        status == "Treatment" & !use_power_builtout ~ 1,
        TRUE ~ 0
      )
    ) %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      control_mean_lst = mean(LST_Celsius[status == "Control"], na.rm = TRUE),
      relative_lst = LST_Celsius - control_mean_lst
    ) %>%
    ungroup()
  
  return(panel_data)
}

assign_treatment_zones <- function(pixel_df, dc_points, 
                                   min_treat_m = 0, max_treat_m = 600, 
                                   min_control_m = 1000, max_control_m = 7500,
                                   sensor = "landsat") {
  
  dc_metric <- dc_points %>% 
    st_transform(5070) %>% 
    # counties with the same name in different states could be problematic here, need to change cl_00 to keep state FIPS code
    select(export_id, year_operational,
           any_of(c("power_builtout","NAMELSAD","GEOID","campus_id",
                    "seam_cohort","source_set")))
  
  # Assign pixel_id on the plain data frame, BEFORE st_as_sf() consumes
  # longitude/latitude into a geometry column. Grouping on
  # (export_id, longitude, latitude) gives every physical pixel the SAME
  # id across all years/months it appears in, instead of a fresh id per
  # row. That's what gives `pixel_id + year` real within-pixel variation
  # to work with in feols, and what makes the baseline-NLCD join in
  # run_pixel_analysis() actually match rows across years instead of
  # only matching each row to itself.
  pixel_df <- pixel_df %>%
    group_by(export_id, longitude, latitude) %>%
    mutate(pixel_id = cur_group_id()) %>%
    ungroup()
  
  # 1. Project the coordinates directly as a matrix (extremely memory efficient)
  coords_matrix <- sf_project(
    from = st_crs(4326), 
    to = st_crs(5070), 
    pts = as.matrix(pixel_df[, c("longitude", "latitude")])
  )
  
  # 2. Append the projected coordinates back to the plain dataframe
  pixel_df$pixel_x <- coords_matrix[, 1]
  pixel_df$pixel_y <- coords_matrix[, 2]
  
  dc_coords <- dc_metric %>%
    mutate(dc_x = st_coordinates(.)[,1], dc_y = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  
  # 3. Base distance calculation on the plain dataframe (no sf object used)
  pixels_classified <- pixel_df %>%
    left_join(dc_coords, by = "export_id") %>%
    mutate(
      dist_to_dc = sqrt((pixel_x - dc_x)^2 + (pixel_y - dc_y)^2)
    )
  
  if (sensor %in% c("modis", "modis_daily")) {
    pixels_classified <- pixels_classified %>%
      group_by(export_id) %>%
      mutate(
        is_host_pixel = dist_to_dc == min(dist_to_dc, na.rm = TRUE),
        status = case_when(
          is_host_pixel ~ "Treatment",
          dist_to_dc >= min_control_m & dist_to_dc <= max_control_m ~ "Control",
          TRUE ~ "Exclude" 
        )
      ) %>%
      ungroup()
    
  } else if (sensor %in% c("landsat", "landsat_daily", "landsat_monthly")) {
    pixels_classified <- pixels_classified %>%
      mutate(
        status = case_when(
          dist_to_dc >= min_treat_m & dist_to_dc <= max_treat_m ~ "Treatment",
          dist_to_dc >= min_control_m & dist_to_dc <= max_control_m ~ "Control",
          TRUE ~ "Exclude"
        )
      )
  }
  
  pixels_classified <- pixels_classified %>% filter(status != "Exclude")
  return(pixels_classified)
}

filter_elevation_controls <- function(classified_pixels, threshold_m = 50) {
  
  dc_baselines <- classified_pixels %>%
    filter(status == "Treatment") %>%
    st_drop_geometry() %>%
    group_by(export_id) %>%
    summarize(dc_base_elev = mean(Elevation, na.rm = TRUE), .groups = "drop")
  
  filtered_pixels <- classified_pixels %>%
    left_join(dc_baselines, by = "export_id") %>%
    mutate(
      elev_diff = abs(Elevation - dc_base_elev),
      keep_pixel = case_when(
        status == "Treatment" ~ TRUE, 
        status == "Control" & elev_diff <= threshold_m ~ TRUE, 
        TRUE ~ FALSE 
      )
    ) %>%
    filter(keep_pixel == TRUE) %>%
    select(-dc_base_elev, -elev_diff, -keep_pixel)
  
  return(filtered_pixels)
}

# ==============================================================================
# assign_treatment_intensity: counts, per pixel per YEAR, how many DCs from
# the full operational inventory have that pixel within their treatment
# radius AND are operational (year >= year_operational). Overlap becomes
# dose instead of discard. n_treating = 0,1,2,... ; time-varying.
# ==============================================================================
assign_treatment_intensity <- function(pixels_classified, master_dcs_sf,
                                       treat_radius_m = 600) {
  master_proj <- master_dcs_sf %>%
    st_transform(5070) %>%
    mutate(mx = st_coordinates(.)[,1], my = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  
  dcs_known <- master_proj %>%
    filter(!is.na(year_operational)) %>%
    select(master_year_op = year_operational, mx, my)
  dcs_unknown <- master_proj %>%
    filter(is.na(year_operational)) %>%
    select(mx, my)
  
  px <- pixels_classified %>%
    distinct(longitude, latitude, pixel_x, pixel_y)
  
  # --- known-year coverage -> time-varying dose ---
  nn <- RANN::nn2(as.matrix(dcs_known[, c("mx","my")]),
                  as.matrix(px[, c("pixel_x","pixel_y")]),
                  k = min(15, nrow(dcs_known)),
                  searchtype = "radius", radius = treat_radius_m)
  pairs <- tibble(px_row = rep(seq_len(nrow(px)), each = ncol(nn$nn.idx)),
                  dc_idx = as.vector(t(nn$nn.idx))) %>%
    filter(dc_idx > 0) %>%
    transmute(longitude = px$longitude[px_row],
              latitude  = px$latitude[px_row],
              master_year_op = dcs_known$master_year_op[dc_idx])
  coverage <- pairs %>%
    group_by(longitude, latitude) %>%
    summarise(covering_years  = list(sort(master_year_op)),
              first_treat_year = min(master_year_op), .groups = "drop")
  
  # --- unknown-year coverage -> permanent contamination flag ---
  if (nrow(dcs_unknown) > 0) {
    nnu <- RANN::nn2(as.matrix(dcs_unknown[, c("mx","my")]),
                     as.matrix(px[, c("pixel_x","pixel_y")]),
                     k = min(15, nrow(dcs_unknown)),
                     searchtype = "radius", radius = treat_radius_m)
    px$near_unknown_dc <- rowSums(nnu$nn.idx > 0) > 0
  } else {
    px$near_unknown_dc <- FALSE
  }
  
  # --- n_treating on the distinct pixel-year grid, then join back ---
  grid <- pixels_classified %>%
    distinct(longitude, latitude, year) %>%
    left_join(coverage, by = c("longitude", "latitude")) %>%
    mutate(n_treating = map2_int(covering_years, year,
                                 ~ { if (is.null(.x) || length(.x) == 0) 0L else sum(.x <= .y) })) %>%
    select(longitude, latitude, year, n_treating, first_treat_year)
  
  pixels_classified %>%
    left_join(grid, by = c("longitude", "latitude", "year")) %>%
    left_join(px %>% select(longitude, latitude, near_unknown_dc),
              by = c("longitude", "latitude"))
}

drop_conflicted_cells <- function(pixels_classified, treat_only = TRUE) {
  # Physical pixel = (longitude, latitude) rounded upstream to 6dp.
  # treat_only = TRUE  -> drop only pixels in >=2 facilities' TREATMENT rings
  # treat_only = FALSE -> also drop treatment-for-A / control-for-B pixels
  roles <- pixels_classified %>%
    distinct(longitude, latitude, export_id, status) %>%
    group_by(longitude, latitude) %>%
    summarise(n_fac   = n_distinct(export_id),
              n_treat = sum(status == "Treatment"),
              .groups = "drop") %>%
    filter(n_fac > 1,
           if (treat_only) n_treat > 1 else n_treat >= 1)
  
  n_before <- nrow(pixels_classified)
  out <- pixels_classified %>%
    anti_join(roles %>% select(longitude, latitude),
              by = c("longitude", "latitude"))
  message("drop_conflicted_cells: removed ",
          format(n_before - nrow(out), big.mark=","), " of ",
          format(n_before, big.mark=","), " rows (",
          nrow(roles), " conflicted pixels)")
  return(out)
}

# ==============================================================================
# Fixed-effects specifications — pass any of these as fe_spec.
# The outcome is relative_lst (pixel LST minus same-DC-period control mean),
# so every spec is a double difference.
#
#   AVAILABLE SPECS                       ABSORBS (beyond pixel invariants)
#   "pixel_id + year"                     national annual shocks        [twfe]
#   "pixel_id + year^month"               + seasonal scene composition
#   "pixel_id + NAMELSAD^year"            + county-level annual shocks
#   "pixel_id + year^export_id"           + DC-specific annual shocks   [interactive]
#   "pixel_id + year^month^export_id"     + DC-specific seasonal shocks (heavy)
#   "pixel_id + year^sensor"
#
#   MAIN LADDER (report these three):
#     1. "pixel_id + year"
#     2. "pixel_id + NAMELSAD^year"
#     3. "pixel_id + year^export_id"
#   Robustness: "pixel_id + year^month"; run the ^month^export_id spec only
#   if specs 2 and 3 disagree.
#
#   Notes:
#   - NAMELSAD (or county_id) requires the county column in the isolation-set
#     CSVs; it is joined to pixels in assign_treatment_zones().
#   - month specs require a month column (all monthly products; derive from
#     date_yyyymmdd for daily). Not available for annual Landsat.
#   - "pixel_id + year + NAMELSAD" is NOT valid: county is time-invariant
#     per pixel and is absorbed by pixel_id. County must interact with time.
# ==============================================================================

run_spatial_model <- function(panel_data, fe_spec = "pixel_id + year",
                              is_event_study = FALSE, ref_year = -1,
                              use_nlcd_control = FALSE, sensor = "landsat", use_construction = FALSE, cluster_var = "export_id") {
  
  # Validate: every FE variable must exist in the panel
  fe_vars <- unique(unlist(strsplit(gsub(" ", "", fe_spec), "[+^]")))
  missing <- setdiff(fe_vars, names(panel_data))
  if (length(missing) > 0) {
    stop("fe_spec uses column(s) not in panel: ",
         paste(missing, collapse = ", "),
         ". Check the county join / month column for sensor = ", sensor)
  }
  
  if (is_event_study) {
    panel_data <- panel_data %>% filter(!is.na(relative_year))
    treat_term <- paste0("i(relative_year, treat_assignment, ref = ", ref_year, ")")
  } else {
    # construction_period absorbs rel years -3..-1 so treated_post is
    # identified against the clean pre-period (rel year <= -4) only when flag is used.
    treat_term <- if(use_construction) "treated_post + construction_period" else "treated_post"
  }
  
  covariate_term <- ""
  if (use_nlcd_control) {
    nlcd_cols <- grep("^base_NLCD_\\d{2}$", names(panel_data), value = TRUE)
    covariate_terms <- paste0("i(year, ", nlcd_cols, ")", collapse = " + ")
    covariate_term <- paste0(" + ", covariate_terms)
  }
  
  form_str <- paste0("relative_lst ~ ", treat_term, covariate_term, " | ", fe_spec)
  if (!cluster_var %in% names(panel_data))
    stop("cluster_var '", cluster_var, "' not in panel — did campus_id survive the joins?")
  feols(as.formula(form_str), data = panel_data,
        cluster = as.formula(paste0("~", cluster_var)))
}

run_pixel_analysis <- function(raw_csv_data, dc_points, 
                               min_treat_m = 0, max_treat_m = 600, 
                               min_control_m = 1000, max_control_m = 7500,
                               fe_spec = 'pixel_id + year', use_power_builtout = FALSE,
                               filter_elev = TRUE, elev_threshold = 50,
                               is_event_study = FALSE, ref_year = -1,
                               use_nlcd_control = FALSE, use_intensity = FALSE, master_dcs = NULL,
                               sensor = "landsat", drop_conflicts = FALSE, use_construction = FALSE, cluster_var = "export_id") {
  
  if (use_power_builtout) {
    dc_points <- dc_points %>% filter(!is.na(power_builtout))
  }
  
  classified_data <- assign_treatment_zones(
    raw_csv_data, dc_points, 
    min_treat_m, max_treat_m, min_control_m, max_control_m, 
    sensor = sensor
  )
  
  if (use_intensity) {
    if (is.null(master_dcs)) stop("use_intensity requires master_dcs")
    classified_data <- assign_treatment_intensity(
      classified_data, master_dcs, treat_radius_m = 600)   # contamination radius fixed at 600
    if (!"near_unknown_dc" %in% names(classified_data))
      classified_data$near_unknown_dc <- FALSE
    # STATIC drop: control pixel within 600m of ANY inventory DC, any year
    classified_data <- classified_data %>%
      filter(!(status == "Control" &
                 (!is.na(first_treat_year) | near_unknown_dc)))
  }
  
  if(drop_conflicts){
    classified_data <- drop_conflicted_cells(classified_data, treat_only = TRUE)
  }
  
  if (filter_elev) {
    classified_data <- filter_elevation_controls(classified_data, threshold_m = elev_threshold)
  }
  
  panel_data <- build_did_panel(classified_data, use_power_builtout, use_intensity = use_intensity, sensor = sensor)
  
  # If construction dummy is FALSE, but the baseline is anchored earlier than -1 
  # (e.g., ref_year = -4), we must physically drop the transition years from the 
  # treated group to prevent them from averaging into the clean baseline.
  if (!use_construction && ref_year < -1) {
    panel_data <- panel_data %>%
      filter(!(status == "Treatment" & relative_year >= ref_year+1 & relative_year < 0))
  }
  
  zone_counts <- panel_data %>%
    filter(!is.na(relative_lst)) %>%
    group_by(status) %>%
    summarise(n_px_obs = n(),
              n_pixels = n_distinct(pixel_id),
              n_dcs    = n_distinct(export_id), .groups = "drop")
  n_t <- zone_counts %>% filter(status == "Treatment")
  n_c <- zone_counts %>% filter(status == "Control")
  
  if (use_nlcd_control) {
    baseline_nlcd <- panel_data %>%
      filter(relative_year == ref_year) %>%
      select(pixel_id, contains("NLCD")) %>%
      rename_with(~paste0("base_", .), contains("NLCD")) %>%
      distinct(pixel_id, .keep_all = TRUE)
    
    panel_data <- panel_data %>%
      left_join(baseline_nlcd, by = "pixel_id") %>%
      drop_na(contains("base_NLCD"))
  }
  
  model_results <- run_spatial_model(
    panel_data, fe_spec = fe_spec, is_event_study = is_event_study, ref_year = ref_year, 
    use_nlcd_control = use_nlcd_control, sensor = sensor, use_construction = use_construction,
    cluster_var = cluster_var
  )
  
  return(list(
    model = model_results,
    data = panel_data,
    counts = tibble(
      n_treat_obs = n_t$n_px_obs %||% 0L, n_ctrl_obs = n_c$n_px_obs %||% 0L,
      n_treat_px  = n_t$n_pixels %||% 0L, n_ctrl_px  = n_c$n_pixels %||% 0L,
      n_treat_dcs = n_t$n_dcs   %||% 0L,  n_ctrl_dcs = n_c$n_dcs   %||% 0L,
      ratio_px    = (n_t$n_pixels %||% 0L) / max(1L, n_c$n_pixels %||% 0L))
  ))
}

analyze_model_significance <- function(rast_data, dc_points, 
                                       radius_list,       
                                       control_radii,     
                                       fe_specs = c("pixel_id + year"), 
                                       use_power_flags = c(TRUE, FALSE),
                                       drop_conflicts_flags = c(FALSE), 
                                       use_nlcd_control = FALSE, 
                                       ref_year = -1,
                                       sensor = "landsat", use_construction = c(TRUE, FALSE)) { 
  
  all_results <- list()
  
  # Added conflict flag to the parameter grid
  params <- expand.grid(
    radius_idx = seq_along(radius_list),
    control_idx = seq_along(control_radii),
    m_type = fe_specs,
    power = use_power_flags,
    conflict = drop_conflicts_flags,
    construction = use_construction
  )
  
  for (i in 1:nrow(params)) {
    rad <- radius_list[[params$radius_idx[i]]]
    con <- control_radii[[params$control_idx[i]]]
    pwr <- params$power[i]
    mod <- as.character(params$m_type[i])
    conf <- params$conflict[i]
    constr <- params$construction[i]
    
    print(paste("Model =", mod, "| Power =", pwr, 
                "| Treat =", rad[1], "-", rad[2], 
                "| Control =", con[1], "-", con[2],
                "| Drop Conflicts =", conf,
                "| Sensor =", sensor,
                "| Construction Period =", constr))
    
    res <- run_pixel_analysis(
      raw_csv_data = rast_data,
      dc_points = dc_points,
      min_treat_m = rad[1], max_treat_m = rad[2],
      min_control_m = con[1], max_control_m = con[2],
      fe_spec = mod,
      use_power_builtout = pwr,
      use_nlcd_control = use_nlcd_control, 
      ref_year = ref_year,
      sensor = sensor,
      drop_conflicts = conf,
      use_construction = constr
    )
    
    ct <- summary(res$model)$coeftable
    
    # Recorded the drop_conflicts state in the output tibble
    if ("treated_post" %in% rownames(ct)) {
      coef_row <- ct["treated_post", ]
      all_results[[i]] <- tibble(
        fe_spec = mod, use_power = pwr, drop_conflicts = conf,
        min_treat = rad[1], max_treat = rad[2],
        min_control = con[1], max_control = con[2],
        estimate = coef_row["Estimate"],
        std_error = coef_row["Std. Error"],
        p_val = coef_row["Pr(>|t|)"]
      )
    } else {
      print("Warning: treated_post dropped from model.")
      all_results[[i]] <- tibble(
        fe_spec = mod, use_power = pwr, drop_conflicts = conf,
        min_treat = rad[1], max_treat = rad[2],
        min_control = con[1], max_control = con[2],
        estimate = NA, std_error = NA, p_val = NA
      )
    }
    
    # ==========================================
    # CRITICAL MEMORY MANAGEMENT FOR MASSIVE DATA
    # Force R to delete the massive 'res' object 
    # before starting the next loop iteration.
    # ==========================================
    rm(res, ct)
    gc()
  } 
  
  return(bind_rows(all_results))
}


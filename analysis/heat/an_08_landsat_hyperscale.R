# an_08_prelim_hyperscale.R — hyperscale 10 at 30m, binary vs intensity
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
options(bitmapType = "cairo")

source(here("analysis","heat","load_hyperscale_panel.R"))
d <- load_hyperscale_panel()
pixel_data <- d$pixel_data
dc_points  <- d$dc_points
master_ops <- d$master_ops

# --- Data Sanity Check Printout (keep yours, it's good) ---
total_obs <- nrow(pixel_data)
unique_pixels <- pixel_data %>% distinct(export_id, longitude, latitude) %>% nrow()
cat("\n==== Dataset Dimensions ====\n")
cat("Total Panel Observations (Rows):", format(total_obs, big.mark=","), "\n")
cat("Distinct Physical 30m Pixels:   ", format(unique_pixels, big.mark=","), "\n")
print(count(pixel_data, sensor))
cat("Facilities:", nrow(dc_points), "| seam cohort:",
    sum(dc_points$seam_cohort), "\n")

# Define the precise spatial rings to test
spatial_rings <- list(
  c(300, 600),  # Outer Campus
  c(0, 300),    # Aggregate Core
  c(0, 600)     # Total Campus Footprint
)

# Build a parameter grid to safely iterate through every combination
params <- expand.grid(
  ring_idx = seq_along(spatial_rings),
  intens = c(FALSE, TRUE),
  use_construction = c(FALSE, TRUE), 
  mod_fe = c("pixel_id + year", "pixel_id + year^export_id", "pixel_id + year^month"),
  stringsAsFactors = FALSE
)
params$cluster_var <- "export_id"
params <- bind_rows(params,
                    params %>% filter(ring_idx == 1, mod_fe == "pixel_id + year") %>%   # 300-600 primary
                      mutate(cluster_var = "campus_id"))

out <- list()

# Hardcoded to exact GEE extraction bounds
CTRL_MIN <- 1000
CTRL_MAX <- 1500 

REF_YEAR <- -4

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

cat("\n==== Starting Expanded Spatial Ring Loop ====\n")
cat("Total models queued:", nrow(params), "\n\n")

for (i in 1:nrow(params)) {
  # Extract parameters for this iteration
  r_idx  <- params$ring_idx[i]
  ring   <- spatial_rings[[r_idx]]
  intens <- params$intens[i]
  constr_flag <- params$use_construction[i] 
  mod_fe <- params$mod_fe[i]
  
  cat(sprintf("[%d/%d] Ring: %s-%sm | Intens: %s | Construct: %s | FE: %s\n", 
              i, nrow(params), ring[1], ring[2], intens, constr_flag, mod_fe))
  
  res <- tryCatch({
    run_pixel_analysis(
      raw_csv_data = pixel_data, 
      dc_points = dc_points,
      min_treat_m = ring[1], 
      max_treat_m = ring[2], 
      min_control_m = CTRL_MIN, 
      max_control_m = CTRL_MAX,
      fe_spec = mod_fe, 
      use_power_builtout = FALSE,
      filter_elev = FALSE,          # Hardcoded OFF (no impact)
      elev_threshold = 50, 
      is_event_study = FALSE,
      ref_year = REF_YEAR,
      use_intensity = intens, 
      master_dcs = if (intens) master_ops else NULL,
      sensor = "landsat_monthly",
      use_construction = constr_flag
    )
  }, error = function(e) { 
    message(" -> SKIP: ", conditionMessage(e))
    return(NULL) 
  })
  
  if (is.null(res)) next
  
  ct <- summary(res$model)$coeftable
  counts_df <- res$counts
  
  for (tm in intersect(c("treated_post", "construction_period"), rownames(ct))) {
    out[[length(out) + 1]] <- tibble(
      ring_min = ring[1],
      ring_max = ring[2],
      ring_label = paste0(ring[1], "-", ring[2], "m"),
      intensity = intens, 
      use_construction = constr_flag, 
      ref_year = REF_YEAR,
      fe_spec = mod_fe,
      term = tm,
      estimate = ct[tm, "Estimate"], 
      se = ct[tm, "Std. Error"],
      p_value = ct[tm, "Pr(>|t|)"], 
      n_obs = res$model$nobs
    ) %>% bind_cols(counts_df)
  }
  
  # CRITICAL MEMORY MANAGEMENT
  rm(res, ct)
  gc()
}

results <- bind_rows(out)

cat("\n\n==== Results: Baseline Models (use_construction = FALSE) ====\n")
print(knitr::kable(
  results %>% 
    filter(use_construction == FALSE) %>% 
    arrange(fe_spec, ref_year, intensity, ring_min, ring_max, desc(term)), 
  digits = 3
))

cat("\n\n==== Results: Construction Models (use_construction = TRUE) ====\n")
print(knitr::kable(
  results %>% 
    filter(use_construction == TRUE) %>% 
    arrange(fe_spec, ref_year, intensity, ring_min, ring_max, desc(term)), 
  digits = 3
))

write_csv(results, here("results", "hyperscale_30m_full_expanded.csv"))
cat("\n==== Done ====\n")
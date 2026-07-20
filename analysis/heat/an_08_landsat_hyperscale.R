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
# campus-level clustering robustness at the primary ring only.
# NOTE: ring_idx == 3 is c(0, 600) in THIS script's spatial_rings ordering
# (the all-DC script orders its list differently -- 0-600 is index 4 there).
params <- bind_rows(params,
                    params %>% filter(ring_idx == 3, mod_fe == "pixel_id + year") %>%
                      mutate(cluster_var = "campus_id"))

# ── Roster scopes (bind only when intens = TRUE; inert for binary rows) ──
#  treat_scope : which facilities count toward n_treating dose
#  contam_scope: which facilities' proximity disqualifies control pixels
# Preferred spec for the whole grid: hs-only dose, all dcs drop.
params$treat_scope  <- "hs_only"
params$contam_scope <- "all"

# Remaining 3 roster combos at the headline cell only (0-600, intensity,
# construction, baseline FE, export_id clustering), so the 4-way comparison
# holds everything else fixed.
combo_rows <- expand.grid(treat_scope  = c("all", "hs_only"),
                          contam_scope = c("all", "hs_only"),
                          stringsAsFactors = FALSE) %>%
  filter(!(treat_scope == "hs_only" & contam_scope == "all")) %>%  # already in grid
  mutate(ring_idx = 3L, intens = TRUE, use_construction = TRUE,
         mod_fe = "pixel_id + year", cluster_var = "export_id")
params <- bind_rows(params, combo_rows)

# Roster lookup for the scope flags. capacity_type comes from
# clean01_datacenter.csv (same filter contamination_eval.R uses).
stopifnot("capacity_type" %in% names(master_ops))
MASTER_SETS <- list(
  all     = master_ops,
  hs_only = master_ops %>% filter(capacity_type == "Hyperscaler"))
cat("Roster sizes | all:", nrow(MASTER_SETS$all),
    "| hs_only:", nrow(MASTER_SETS$hs_only), "\n")
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
  clust <- params$cluster_var[i]
  t_scope <- params$treat_scope[i]
  c_scope <- params$contam_scope[i]
  
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
      master_dcs = if (intens) MASTER_SETS[[t_scope]] else NULL,
      sensor = "landsat_monthly",
      use_construction = constr_flag,
      cluster_var = clust,
      contamination_master = if (intens) MASTER_SETS[[c_scope]] else NULL
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
      cluster_var = clust,
      treat_scope = if (intens) t_scope else NA_character_,
      contam_scope = if (intens) c_scope else NA_character_,
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
# an_08_seasonal.R — seasonal heterogeneity at the headline cell (hyperscale)
# Motivation: (a) waste-heat + solar-loading physics predicts stronger daytime
# LST effects in summer; (b) bounds how much scene-season composition can
# explain historical estimate differences across extraction vintages.
# PREDICTION (stated before running): summer > pooled > winter.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
options(bitmapType = "cairo")
setFixest_nthreads(8); data.table::setDTthreads(8)

d <- load_hyperscale_panel()

# Safety net: enforce the uniform window even if the loader trim is absent.
pixel_data <- d$pixel_data
cat("Post-trim rows:", format(nrow(pixel_data), big.mark=","),
    "| max year:", max(pixel_data$year), "\n")

dc_points  <- d$dc_points
master_ops <- d$master_ops
MASTER_SETS <- list(
  all     = master_ops,
  hs_only = master_ops %>% filter(capacity_type == "Hyperscaler"))

# ── Headline configuration: ONE cell, held fixed across seasons ─────────
# Matches the current grid's headline (0-600, intensity, construction dummy,
# baseline FE). Roster scopes fixed at all/all so the season contrast is the
# ONLY thing varying across rows.
RING <- c(0, 600); CTRL_MIN <- 1000; CTRL_MAX <- 1500
REF_YEAR <- -4; FE <- "pixel_id + year"
T_SCOPE <- "all"; C_SCOPE <- "all"

SEASONS <- list(
  pooled   = 1:12,
  summer   = 6:8,
  winter   = c(12, 1, 2),
  shoulder = c(3, 4, 5, 9, 10, 11))

out <- list()
for (s in names(SEASONS)) {
  mons <- SEASONS[[s]]
  dat  <- pixel_data %>% filter(month %in% mons)
  cat(sprintf("[%s] months: %s | rows: %s\n", s,
              paste(mons, collapse=","), format(nrow(dat), big.mark=",")))
  
  res <- tryCatch(
    run_pixel_analysis(
      raw_csv_data = dat, dc_points = dc_points,
      min_treat_m = RING[1], max_treat_m = RING[2],
      min_control_m = CTRL_MIN, max_control_m = CTRL_MAX,
      fe_spec = FE, use_power_builtout = FALSE,
      filter_elev = FALSE, elev_threshold = 50, is_event_study = FALSE,
      ref_year = REF_YEAR, use_intensity = TRUE,
      master_dcs = MASTER_SETS[[T_SCOPE]],
      sensor = "landsat_monthly", use_construction = TRUE,
      cluster_var = "export_id",
      contamination_master = MASTER_SETS[[C_SCOPE]]),
    error = function(e) { message(" -> SKIP: ", conditionMessage(e)); NULL })
  if (is.null(res)) next
  
  ct <- summary(res$model)$coeftable
  for (tm in intersect(c("treated_post","construction_period"), rownames(ct))) {
    out[[length(out)+1]] <- tibble(
      season = s, months = paste(mons, collapse=","),
      ring_min = RING[1], ring_max = RING[2],
      intensity = TRUE, use_construction = TRUE,
      treat_scope = T_SCOPE, contam_scope = C_SCOPE,
      fe_spec = FE, ref_year = REF_YEAR, term = tm,
      estimate = ct[tm,"Estimate"], se = ct[tm,"Std. Error"],
      p_value = ct[tm,"Pr(>|t|)"], n_obs = res$model$nobs
    ) %>% bind_cols(res$counts)
  }
  rm(res, ct, dat); gc()
}

results <- bind_rows(out)
cat("\n==== Seasonal Heterogeneity: Headline Cell ====\n")
print(knitr::kable(results %>% arrange(term, factor(season, names(SEASONS))),
                   digits = 3))
write_csv(results, here("results","hyperscale_seasonal_headline.csv"))
cat("==== Done ====\n")
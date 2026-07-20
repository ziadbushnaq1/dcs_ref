# an_08_summer_boot.R — wild cluster bootstrap (Webb) for the SUMMER
# headline cell, hyperscale sample. Conventional CRSE gave p = 0.041 at
# 17 clusters; that count is below the reliability threshold for CRSE,
# so this p-value is the one to report for the summer result.
library(tidyverse); library(sf); library(fixest); library(here)
library(duckdb); library(fwildclusterboot)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
setFixest_nthreads(8); data.table::setDTthreads(8)
dqrng::dqset.seed(42)   # boottest reproducibility

d <- load_hyperscale_panel()
MASTER_SETS <- list(all = d$master_ops)

summer_data <- d$pixel_data %>% filter(month %in% 6:8)
cat("Summer rows:", format(nrow(summer_data), big.mark=","), "\n")

res <- run_pixel_analysis(
  raw_csv_data = summer_data, dc_points = d$dc_points,
  min_treat_m = 0, max_treat_m = 600,
  min_control_m = 1000, max_control_m = 1500,
  fe_spec = "pixel_id + year", filter_elev = FALSE,
  ref_year = -4, use_intensity = TRUE,
  master_dcs = MASTER_SETS$all,
  sensor = "landsat_monthly", use_construction = TRUE,
  cluster_var = "export_id",
  contamination_master = MASTER_SETS$all,
  contam_timing = "static")

ct <- summary(res$model)$coeftable
cat("\nConventional CRSE (full FE model):\n"); print(ct["treated_post", ])

# ---- free everything the bootstrap doesn't need ----
pan <- res$data %>%
  filter(!is.na(relative_lst)) %>%
  select(relative_lst, treated_post, construction_period,
         pixel_id, year, export_id)
rm(res, summer_data, d); gc()

# ---- FWL: demean by pixel and year, then bootstrap an FE-free model ----
dm <- fixest::demean(
  pan %>% select(relative_lst, treated_post, construction_period),
  pan %>% select(pixel_id, year)) %>%
  as_tibble() %>%
  mutate(export_id = pan$export_id)
rm(pan); gc()

m_dm <- feols(relative_lst ~ treated_post + construction_period,
              data = dm, cluster = ~export_id)
# Sanity check: FWL guarantees this equals the FE model's coefficient
cat("\nDemeaned-model estimate (must match above):",
    round(coef(m_dm)["treated_post"], 4), "\n")

bt <- boottest(m_dm, param = "treated_post",
               clustid = "export_id", B = 9999, type = "webb")
cat("\nWild cluster bootstrap (Webb, B=9999):\n")
print(summary(bt))

write_csv(tibble(
  cell = "summer_0-600_intens_constr", season = "summer",
  estimate = ct["treated_post","Estimate"],
  se_conventional = ct["treated_post","Std. Error"],
  p_conventional  = ct["treated_post","Pr(>|t|)"],
  p_wild_webb = bt$p_val,
  ci_lo = bt$conf_int[1], ci_hi = bt$conf_int[2],
  n_clusters = 17),
  here("results","bootstrap_summer_headline.csv"))
cat("Saved: results/bootstrap_summer_headline.csv\n")
rm(dm, m_dm); gc()
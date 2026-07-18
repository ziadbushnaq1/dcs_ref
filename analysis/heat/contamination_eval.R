# contamination_eval.R — counts ONLY, no models. Roster-driven sample.
library(tidyverse); library(sf); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))

d  <- load_hyperscale_panel()
px <- d$pixel_data %>%
  distinct(export_id, longitude, latitude, Elevation) %>%
  mutate(year = 2025)          # worst-case coverage year

master_full <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                        show_col_types = FALSE) %>%
  filter(stage == "Operational") %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

MASTERS <- list(
  same_asset  = d$dc_points,
  all_known   = master_full %>% filter(!is.na(year_operational)),
  all_incl_na = master_full,
  hs_only     = master_full %>% filter(capacity_type == "Hyperscaler"))

RINGS <- list(c(0,150,1000,1500), c(0,300,1000,1500), c(300,600,1000,1500))

out <- list()
for (ring in RINGS) {
  cls <- assign_treatment_zones(px, d$dc_points, ring[1], ring[2],
                                ring[3], ring[4], sensor = "landsat")
  for (mname in names(MASTERS)) {
    ci <- assign_treatment_intensity(cls, MASTERS[[mname]],
                                     treat_radius_m = 600)   # fixed contamination radius
    if (!"near_unknown_dc" %in% names(ci)) ci$near_unknown_dc <- FALSE
    
    out[[paste(ring[2], mname)]] <- ci %>%
      group_by(export_id) %>%
      summarise(
        n_treat        = sum(status == "Treatment"),
        n_treat_multi  = sum(status == "Treatment" & n_treating > 1),
        n_ctrl         = sum(status == "Control"),
        n_ctrl_dirty   = sum(status == "Control" &
                               (n_treating > 0 | near_unknown_dc)),   # exact union
        .groups = "drop") %>%
      mutate(ring = paste0(ring[1],"-",ring[2],"m"), master = mname,
             ctrl_after_drop = n_ctrl - n_ctrl_dirty,
             pct_ctrl_lost   = 100 * n_ctrl_dirty / pmax(1, n_ctrl),
             ratio_additive  = n_treat / pmax(1, ctrl_after_drop),
             ctrl_dead       = ctrl_after_drop == 0)
    rm(ci); gc()
  }
  rm(cls); gc()
}

results <- bind_rows(out)
write_csv(results, here("results","contamination_eval_per_dc.csv"))
summary_tab <- results %>% group_by(ring, master) %>%
  summarise(dcs = n(), dcs_ctrl_dead = sum(ctrl_dead),
            med_pct_ctrl_lost = median(pct_ctrl_lost),
            med_ratio = median(ratio_additive), .groups = "drop")
print(knitr::kable(summary_tab, digits = 1))
write_csv(summary_tab, here("results","contamination_eval_summary.csv"))
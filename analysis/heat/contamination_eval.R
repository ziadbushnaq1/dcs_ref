# contamination_eval.R — counts ONLY, no models.
# For each master-list option x ring config: per-DC treated/control pixel
# counts under each mechanism, so you can pick before running an_08.
library(tidyverse); library(sf); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))

ASSET_FILE <- here("data","processed","landsat_all146","landsat_all146_obs30m_l89.csv")
HS <- c(411,412,648,664,2949,2950,2998,3012,3051,3052)

# One distinct-pixel snapshot is enough — contamination is geometric.
# Coverage evaluated at 2025 (max operational neighbors = worst case).
con <- dbConnect(duckdb())
px <- dbGetQuery(con, glue::glue("
  SELECT DISTINCT export_id, longitude, latitude, Elevation
  FROM read_csv('{ASSET_FILE}', ignore_errors=true)"))
dbDisconnect(con)
px$year <- 2025

dc_all <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"),
                   show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

master_full <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                        show_col_types = FALSE) %>%
  filter(stage == "Operational") %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

MASTERS <- list(
  same_asset  = dc_all,
  all_known   = master_full %>% filter(!is.na(year_operational)),
  all_incl_na = master_full,
  hs_only     = master_full %>% filter(capacity_type == "Hyperscaler"))

RINGS <- list(c(0,150,1000,1500), c(0,300,1000,1500), c(300,600,1000,1500))

out <- list()
for (ring in RINGS) {
  cls <- assign_treatment_zones(px, dc_all, ring[1], ring[2], ring[3], ring[4],
                                sensor = "landsat")
  for (mname in names(MASTERS)) {
    ci <- assign_treatment_intensity(cls, MASTERS[[mname]],
                                     treat_radius_m = ring[2])
    if (!"near_unknown_dc" %in% names(ci)) ci$near_unknown_dc <- FALSE
    
    tab <- ci %>%
      group_by(export_id) %>%
      summarise(
        n_treat          = sum(status == "Treatment"),
        n_treat_multi    = sum(status == "Treatment" & n_treating > 1),
        n_ctrl           = sum(status == "Control"),
        n_ctrl_covered   = sum(status == "Control" & n_treating > 0),
        n_ctrl_unknown   = sum(status == "Control" & near_unknown_dc),
        .groups = "drop") %>%
      mutate(
        ring   = paste0(ring[1],"-",ring[2],"m"),
        master = mname,
        # mechanism outcomes:
        ctrl_after_drop      = n_ctrl - pmax(n_ctrl_covered, n_ctrl_unknown*0) -
          (n_ctrl_unknown - pmin(n_ctrl_unknown, n_ctrl_covered)),
        ctrl_after_drop      = n_ctrl - n_ctrl_covered,          # covered only
        ctrl_after_drop_full = n_ctrl -
          (n_ctrl_covered + n_ctrl_unknown -
             pmin(n_ctrl_covered, n_ctrl_unknown)),                # covered OR unknown (approx union)
        treat_after_drop     = n_treat - n_treat_multi,
        pct_ctrl_lost        = 100 * (1 - ctrl_after_drop / pmax(1, n_ctrl)),
        pct_treat_lost       = 100 * (1 - treat_after_drop / pmax(1, n_treat)),
        ratio_additive       = n_treat / pmax(1, ctrl_after_drop),  # your hybrid
        ctrl_dead            = ctrl_after_drop == 0)
    out[[paste(ring[2], mname)]] <- tab
    rm(ci); gc()
  }
  rm(cls); gc()
}

results <- bind_rows(out)
write_csv(results, here("results","contamination_eval_per_dc.csv"))

# Decision summary: per option, how many DCs become unusable?
summary_tab <- results %>%
  group_by(ring, master) %>%
  summarise(dcs = n(),
            dcs_ctrl_dead   = sum(ctrl_dead),
            med_pct_ctrl_lost  = median(pct_ctrl_lost),
            med_pct_treat_lost = median(pct_treat_lost),
            med_ratio_additive = median(ratio_additive), .groups = "drop")
print(knitr::kable(summary_tab, digits = 1))
write_csv(summary_tab, here("results","contamination_eval_summary.csv"))
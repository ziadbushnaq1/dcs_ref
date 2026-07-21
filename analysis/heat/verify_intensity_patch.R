# verify_intensity_patch.R — confirms the unknown-DC contamination flag is live.
# Expected: stopifnot passes, and both "controls near unknown" and
# "controls near known" print NONZERO.
library(tidyverse); library(sf); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))

# guard: exactly one definition of the function must exist
src_lines <- readLines(here("analysis","heat","an_01_functions.R"))
n_defs <- sum(grepl("assign_treatment_intensity <- function", src_lines))
cat("Function definitions found:", n_defs, "\n")
stopifnot(n_defs == 1)

d <- load_hyperscale_panel()
px1 <- d$pixel_data %>%
  distinct(export_id, longitude, latitude, Elevation) %>%
  mutate(year = 2025)
rm(d) ; gc()   # keep only what's needed — the full panel isn't

dc_points <- read_csv(here("data","data_final","hyperscale_roster.csv"),
                      show_col_types = FALSE) %>%
  filter(has_event_time) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

master_ops <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                       show_col_types = FALSE) %>%
  filter(stage == "Operational") %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
cat("Master inventory:", nrow(master_ops), "operational |",
    sum(is.na(master_ops$year_operational)), "unknown-year\n")

cls <- assign_treatment_zones(px1, dc_points, 300, 600, 1000, 1500,
                              sensor = "landsat")
ci  <- assign_treatment_intensity(cls, master_ops, treat_radius_m = 600)

stopifnot("near_unknown_dc" %in% names(ci))
cat("\n==== VERIFICATION ====\n")
cat("near_unknown_dc TRUE (all pixels):", sum(ci$near_unknown_dc), "\n")
cat("controls near unknown-year DC:   ",
    sum(ci$status == "Control" & ci$near_unknown_dc), "\n")
cat("controls near known-year DC:     ",
    sum(ci$status == "Control" & ci$n_treating > 0), "\n")
cat("controls dropped by static rule: ",
    sum(ci$status == "Control" & (ci$n_treating > 0 | ci$near_unknown_dc)), "\n")

per_dc <- ci %>% filter(status == "Control") %>%
  group_by(export_id) %>%
  summarise(n_ctrl = n(),
            dirty  = sum(n_treating > 0 | near_unknown_dc),
            pct    = round(100 * dirty / n_ctrl, 1), .groups = "drop") %>%
  arrange(desc(pct))
print(knitr::kable(per_dc, caption = "Per-facility control contamination (both sources)"))
cat("==== DONE ====\n")
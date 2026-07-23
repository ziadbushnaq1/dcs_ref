# ==============================================================================
# fig_ref_year_justification.R
# Visual justification for ref_year = -4: NLCD land-cover trajectories in the
# treatment ring around data-center opening, pooled across Landsat assets.
#
# Shows: barren + developed shares rise from rel_year -3, barren peaks at
# -1/0 (construction), while rel years <= -4 are flat (clean baseline).
# Control-ring panel shows no such pattern (placebo).
#
# Run:  Rscript analysis/heat/fig_ref_year_justification.R
# ==============================================================================

library(tidyverse)
library(here)

ASSETS <- c("landsat_iso2500m", "landsat_iso5000m",
            "landsat_iso7500m", "landsat_iso10000m")

nlcd_cols <- paste0("NLCD_", c(11,12,21,22,23,24,31,41,42,43,
                               51,52,71,72,73,74,81,82,90,95))

# ── Load one deduplicated pixel-year table across assets ─────────────────
# Isolation sets are nested: a DC in iso10000m also appears in iso2500m.
# Keep each export_id from ONE asset only (first appearance, largest set
# first so every DC is included exactly once).
seen_ids <- integer(0)
px <- map_dfr(ASSETS, function(a) {
  message("Reading: ", a)
  d <- read_csv(
    here("data", "processed", a, "landsat_monthly_obs_lst_landcover.csv"),
    col_select = all_of(c("export_id", "longitude", "latitude",
                          "year", nlcd_cols)),
    show_col_types = FALSE) %>%
    distinct(export_id, longitude, latitude, year, .keep_all = TRUE) %>%
    filter(!is.na(NLCD_11), !export_id %in% seen_ids)
  seen_ids <<- union(seen_ids, unique(d$export_id))
  d
})
message("Unique DCs pooled: ", length(seen_ids))

# ── Zone classification (same geometry as the analysis) ──────────────────
dc <- map_dfr(ASSETS, ~ read_csv(
  here("data", "data_final", "isolation_sets", paste0(.x, ".csv")),
  show_col_types = FALSE)) %>%
  distinct(export_id, .keep_all = TRUE) %>%
  select(export_id, year_operational, dc_x = projected_x, dc_y = projected_y)

coords <- sf::sf_project(from = sf::st_crs(4326), to = sf::st_crs(5070),
                         pts = as.matrix(px[, c("longitude", "latitude")]))
px$pixel_x <- coords[, 1]; px$pixel_y <- coords[, 2]

traj <- px %>%
  left_join(dc, by = "export_id") %>%
  mutate(
    dist_to_dc = sqrt((pixel_x - dc_x)^2 + (pixel_y - dc_y)^2),
    zone = case_when(
      dist_to_dc <= 600                          ~ "Treatment ring (0\u2013600 m)",
      dist_to_dc >= 1000 & dist_to_dc <= 7500    ~ "Control ring (1000\u20137500 m)",
      TRUE ~ NA_character_),
    rel_year = year - year_operational,
    Developed  = NLCD_21 + NLCD_22 + NLCD_23 + NLCD_24,
    Barren     = NLCD_31,
    Vegetation = NLCD_41 + NLCD_42 + NLCD_43 + NLCD_71 + NLCD_81 + NLCD_82
  ) %>%
  filter(!is.na(zone), rel_year >= -6, rel_year <= 5) %>%
  pivot_longer(c(Developed, Barren, Vegetation),
               names_to = "class", values_to = "share") %>%
  group_by(zone, class, rel_year) %>%
  summarise(mean_share = mean(share),
            se = sd(share) / sqrt(n_distinct(px$export_id)),  # coarse DC-level se
            n_pixels = n(), .groups = "drop")

# Barren is an order of magnitude smaller than the others — index each class
# to its own rel_year <= -4 baseline so all three read on one axis, and the
# construction disturbance appears as departure from 1.
baselines <- traj %>%
  filter(rel_year <= -4) %>%
  group_by(zone, class) %>%
  summarise(base = mean(mean_share), .groups = "drop")

traj_idx <- traj %>%
  left_join(baselines, by = c("zone", "class")) %>%
  mutate(idx = mean_share / base)

# ── Figure ────────────────────────────────────────────────────────────────
p <- ggplot(traj_idx, aes(rel_year, idx, color = class)) +
  geom_vline(xintercept = -1, linetype = "dotted", color = "firebrick") +
  geom_vline(xintercept = -4, linetype = "dashed",  color = "steelblue") +
  geom_hline(yintercept = 1, color = "grey70") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ zone) +
  scale_color_manual(values = c(Developed = "grey30", Barren = "chocolate3",
                                Vegetation = "forestgreen")) +
  scale_x_continuous(breaks = -6:5) +
  labs(
    title  = "Land cover around data center opening",
    x = "Years relative to opening", y = "Share relative to initial proportions",
    color = NULL,
    caption = paste0("NLCD class shares indexed to their rel-year \u2264 \u22124 mean. Dashed blue: chosen reference (\u22124, pre-disturbance). Dotted red: old reference year (\u22121). Annual NLCD Collection 1.0")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.caption = element_text(size = 8))

dir.create(here("figures"), showWarnings = FALSE)
ggsave(here("figures", "ref_year_justification.png"), p,
       width = 12, height = 6, dpi = 150)
message("Saved: figures/ref_year_justification.png")

# Companion table for the appendix / slide notes
traj %>%
  filter(zone == "Treatment ring (0\u2013600 m)") %>%
  select(class, rel_year, mean_share, n_pixels) %>%
  pivot_wider(names_from = class, values_from = c(mean_share, n_pixels)) %>%
  write_csv(here("results", "ref_year_trajectory_treatment.csv"))
message("Saved: results/ref_year_trajectory_treatment.csv")
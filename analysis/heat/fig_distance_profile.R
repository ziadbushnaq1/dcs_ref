library(tidyverse); library(sf); library(here)
source(here("analysis", "heat", "an_01_functions.R"))

ASSET   <- "landsat_hyperscale_all"     # rerun with iso2500m / all146 later
BIN_W   <- 150
MAX_D   <- 5000

pixel_data <- read_csv(
  here("data","processed",ASSET,"landsat_monthly_obs_lst_landcover.csv"),
  col_select = c(export_id, longitude, latitude, year, month,
                 date_yyyymmdd, LST_Celsius, Elevation),
  show_col_types = FALSE)

best_dates <- pixel_data %>%
  group_by(export_id, year, month, date_yyyymmdd) %>%
  summarise(n_valid = sum(!is.na(LST_Celsius)), .groups="drop") %>%
  group_by(export_id, year, month) %>%
  filter(n_valid == max(n_valid)) %>% filter(date_yyyymmdd == min(date_yyyymmdd)) %>%
  ungroup() %>% select(export_id, year, month, date_yyyymmdd)
pixel_data <- semi_join(pixel_data, best_dates,
                        by = c("export_id","year","month","date_yyyymmdd"))
rm(best_dates); gc()

dc_points <- read_csv(here("data","data_final","isolation_sets",
                           paste0(ASSET,".csv")), show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

# Distances (reuse zone machinery's projection approach)
dc_xy <- dc_points %>%
  mutate(dc_x = st_coordinates(.)[,1], dc_y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>% select(export_id, year_operational, dc_x, dc_y)

xy <- sf::sf_project(st_crs(4326), st_crs(5070),
                     as.matrix(pixel_data[, c("longitude","latitude")]))
pixel_data$px <- xy[,1]; pixel_data$py <- xy[,2]

prof <- pixel_data %>%
  left_join(dc_xy, by = "export_id") %>%
  mutate(
    dist   = sqrt((px-dc_x)^2 + (py-dc_y)^2),
    dbin   = pmin(floor(dist / BIN_W) * BIN_W, MAX_D - BIN_W),
    relyr  = year - year_operational,
    period = case_when(relyr <= -4          ~ "Pre (rel \u2264 \u22124)",
                       relyr >=  1          ~ "Post (rel \u2265 +1)",
                       TRUE                 ~ NA_character_)   # drop construction/yr0
  ) %>%
  filter(dist <= MAX_D, !is.na(period), !is.na(LST_Celsius))

# Outcome (a): relative_lst vs same-DC-month FAR-FIELD mean (3000-5000m),
# so the near-field profile isn't differenced against itself
farfield <- prof %>% filter(dbin >= 3000) %>%
  group_by(export_id, year, month) %>%
  summarise(ff = mean(LST_Celsius), .groups = "drop")

prof <- prof %>% left_join(farfield, by = c("export_id","year","month")) %>%
  filter(!is.na(ff)) %>% mutate(rel = LST_Celsius - ff)

# DC-level means per bin-period first, THEN aggregate across DCs —
# so SD bands reflect between-facility variation, not pixel noise,
# and big campuses don't dominate.
plot_dat <- prof %>%
  group_by(export_id, period, dbin) %>%
  summarise(dc_mean = mean(rel), .groups = "drop") %>%
  group_by(period, dbin) %>%
  summarise(m = mean(dc_mean), sd = sd(dc_mean), n_dc = n(), .groups = "drop")

p <- ggplot(plot_dat, aes(dbin + BIN_W/2, m, color = period, fill = period)) +
  geom_ribbon(aes(ymin = m - sd, ymax = m + sd), alpha = .15, color = NA) +
  geom_line(linewidth = .9) + geom_point(size = 1.4) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dashed") +
  annotate("rect", xmin = 300, xmax = 600, ymin = -Inf, ymax = Inf,
           alpha = .07, fill = "steelblue") +
  scale_x_continuous(breaks = seq(0, MAX_D, 500)) +
  scale_color_manual(values = c("Pre (rel \u2264 \u22124)" = "grey40",
                                "Post (rel \u2265 +1)" = "firebrick")) +
  scale_fill_manual(values  = c("Pre (rel \u2264 \u22124)" = "grey40",
                                "Post (rel \u2265 +1)" = "firebrick")) +
  labs(title = paste("Thermal profile by distance:", ASSET),
       subtitle = paste0("LST relative to same-scene 3\u20135 km far field; ",
                         "Shaded: 300\u2013600 m analysis ring. ",
                         "Construction years (rel \u22123..0) excluded."),
       x = "Distance from facility (m)", y = "Relative LST (\u00b0C)",
       color = NULL, fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

ggsave(here("figures", paste0("distance_profile_", ASSET, ".png")),
       p, width = 12, height = 6, dpi = 150)
write_csv(plot_dat, here("results", paste0("distance_profile_", ASSET, ".csv")))
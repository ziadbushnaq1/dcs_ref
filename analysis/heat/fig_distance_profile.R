# fig_distance_profile.R — thermal profile vs distance, all-DC sample.
# Pre = rel year <= -4 (before construction); Post = rel year >= 0.
# Outcome is LST relative to the facility's own 1000-1500m control mean,
# matching the regression's counterfactual.
library(tidyverse); library(sf); library(here); library(duckdb)
options(bitmapType = "cairo")

BIN_W <- 100; MAX_D <- 1500

f <- here("data","processed","landsat_all146","landsat_all146_obs30m_l89.csv")
con <- dbConnect(duckdb()); dbExecute(con, "SET memory_limit='24GB'")
px <- dbGetQuery(con, glue::glue("
  WITH src AS (
    SELECT export_id, longitude, latitude, year, month, date_yyyymmdd,
           LST_Celsius, scene_cloud_cover
    FROM read_csv('{f}', ignore_errors=true)
    WHERE scene_cloud_cover < 30 AND LST_Celsius IS NOT NULL),
  counts AS (
    SELECT export_id, year, month, date_yyyymmdd, COUNT(*) AS n_valid
    FROM src GROUP BY 1,2,3,4),
  best AS (
    SELECT export_id, year, month, date_yyyymmdd FROM (
      SELECT *, ROW_NUMBER() OVER (PARTITION BY export_id, year, month
              ORDER BY n_valid DESC, date_yyyymmdd ASC) rk FROM counts) WHERE rk=1)
  SELECT src.* FROM src JOIN best USING (export_id, year, month, date_yyyymmdd)"))
dbDisconnect(con)

dcs <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"),
                show_col_types = FALSE)

# distance from each pixel to its own facility
px_sf <- px %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(5070)
xy <- st_coordinates(px_sf)
px <- px %>% mutate(px_x = xy[,1], px_y = xy[,2]) %>%
  left_join(dcs %>% select(export_id, projected_x, projected_y, year_operational),
            by = "export_id") %>%
  mutate(dist_m = sqrt((px_x - projected_x)^2 + (px_y - projected_y)^2),
         rel_year = year - year_operational)
rm(px_sf, xy); gc()

# control mean per facility-scene = the regression's counterfactual
ctrl <- px %>%
  filter(between(dist_m, 1000, 1500)) %>%
  group_by(export_id, year, month) %>%
  summarise(ctrl_lst = mean(LST_Celsius), .groups = "drop")

prof <- px %>%
  filter(dist_m <= MAX_D) %>%
  inner_join(ctrl, by = c("export_id","year","month")) %>%
  mutate(rel = LST_Celsius - ctrl_lst,
         period = case_when(rel_year <= -4 ~ "Pre-construction (rel \u2264 \u22124)",
                            rel_year >=  0 ~ "Operational (rel \u2265 0)",
                            TRUE ~ NA_character_),
         dbin = floor(dist_m / BIN_W) * BIN_W) %>%
  filter(!is.na(period))

# facility-level means first, so large facilities don't dominate
plot_dat <- prof %>%
  group_by(export_id, period, dbin) %>%
  summarise(dc_mean = mean(rel), .groups = "drop") %>%
  group_by(period, dbin) %>%
  summarise(m = mean(dc_mean), se = sd(dc_mean)/sqrt(n()),
            n_dc = n(), .groups = "drop")

p <- ggplot(plot_dat, aes(dbin + BIN_W/2, m, color = period, fill = period)) +
  annotate("rect", xmin = 0, xmax = 600, ymin = -Inf, ymax = Inf,
           alpha = .07, fill = "#7B2841") +
  annotate("rect", xmin = 1000, xmax = 1500, ymin = -Inf, ymax = Inf,
           alpha = .05, fill = "grey30") +
  geom_ribbon(aes(ymin = m - 1.96*se, ymax = m + 1.96*se),
              alpha = .18, color = NA) +
  geom_line(linewidth = .9) + geom_point(size = 1.5) +
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, MAX_D, 300)) +
  scale_color_manual(values = c("Pre-construction (rel \u2264 \u22124)" = "grey45",
                                "Operational (rel \u2265 0)" = "#7B2841")) +
  scale_fill_manual(values  = c("Pre-construction (rel \u2264 \u22124)" = "grey45",
                                "Operational (rel \u2265 0)" = "#7B2841")) +
  labs(title = "Thermal Profile by Distance from Data Center",
       caption = paste0("145 facilities. LST relative to each facility's own ",
                         "1000\u20131500m control ring. Shaded: treatment ring ",
                         "(0\u2013600m) and control ring (1000\u20131500m). 95% CIs."),
       x = "Distance from facility (m)", y = "Relative LST (\u00b0C)",
       color = NULL, fill = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title.position = "plot",
        plot.title = element_text(face = "bold", hjust = 0.5))

ggsave(here("figures","distance_profile_all.png"), p,
       width = 11.9, height = 5.9, dpi = 300, bg = "white")
write_csv(plot_dat, here("results","distance_profile_all.csv"))
cat("Saved: figures/distance_profile_all.png\n")
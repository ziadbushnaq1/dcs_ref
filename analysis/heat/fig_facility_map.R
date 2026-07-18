# fig_facility_map.R — per-facility pixel maps with treatment rings
library(tidyverse); library(sf); library(here); library(duckdb)
source(here("analysis","heat","load_hyperscale_panel.R"))
options(bitmapType = "cairo")

d <- load_hyperscale_panel()

# One distinct-pixel snapshot per facility
px <- d$pixel_data %>% distinct(export_id, longitude, latitude)
xy <- sf::sf_project(st_crs(4326), st_crs(5070),
                     as.matrix(px[, c("longitude","latitude")]))
px$x <- xy[,1]; px$y <- xy[,2]

dc_xy <- d$dc_points %>%
  mutate(dc_x = st_coordinates(.)[,1], dc_y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>% select(export_id, year_operational, dc_x, dc_y)

px <- px %>% left_join(dc_xy, by = "export_id") %>%
  mutate(dist = sqrt((x-dc_x)^2 + (y-dc_y)^2))

# Printout: pixel-extent audit per facility
audit <- px %>% group_by(export_id) %>%
  summarise(max_dist = max(dist), n_px = n(),
            n_in_600 = sum(dist <= 600), .groups = "drop") %>%
  left_join(dc_xy %>% select(export_id, year_operational), by = "export_id")
print(knitr::kable(audit, digits = 0,
                   caption = "Per-facility pixel extent (max_dist should be ~1500m = buffer)"))
write_csv(audit, here("results","facility_ring_audit.csv"))

# Map: faceted, rings at 300/600/1000/1500
ring_df <- expand_grid(export_id = unique(px$export_id),
                       r = c(300, 600, 1000, 1500)) %>%
  left_join(dc_xy, by = "export_id") %>%
  rowwise() %>%
  mutate(circle = list(tibble(theta = seq(0, 2*pi, length.out = 120),
                              cx = dc_x + r*cos(theta),
                              cy = dc_y + r*sin(theta)))) %>%
  unnest(circle)

p <- ggplot() +
  geom_point(data = px, aes((x - dc_x), (y - dc_y)), size = .1,
             color = "grey55", alpha = .5) +
  geom_path(data = ring_df, aes((cx - dc_x), (cy - dc_y),
                                group = r, color = factor(r)), linewidth = .5) +
  geom_point(data = dc_xy, aes(0, 0), color = "black", size = 1.5, shape = 3) +
  facet_wrap(~export_id, ncol = 4) +
  coord_equal() +
  scale_color_manual(values = c(`300` = "firebrick", `600` = "darkorange",
                                `1000` = "steelblue", `1500` = "grey40"),
                     name = "Ring (m)") +
  labs(title = "Hyperscale facilities: extracted pixels and candidate treatment rings",
       subtitle = "Facility-centered coordinates; cross = facility point; grey dots = extracted 30 m pixels",
       x = "meters east", y = "meters north") +
  theme_minimal() + theme(axis.text = element_text(size = 6))
ggsave(here("figures","facility_ring_map.png"), p, width = 13, height = 11, dpi = 150)
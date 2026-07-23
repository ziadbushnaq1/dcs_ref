# fig_lst_heatmap_final.R
library(tidyverse)
library(sf)
library(here)
library(duckdb)
options(bitmapType = "cairo")

# 1. Load the master panel to ensure L5 and L8/9 data are perfectly merged
source(here("analysis", "heat", "load_hyperscale_panel.R"))
d <- load_hyperscale_panel()

# 2. Select Facility 
target_id <- 3012

master_ops <- d$dc_points %>%
  filter(export_id == target_id) %>%
  st_transform(5070)

year_op <- master_ops$year_operational[1]

# 3. Generate the spatial rings (EPSG:5070)
rings <- bind_rows(
  st_buffer(master_ops, 1500) %>% mutate(ring = "1500m (Control)"),
  st_buffer(master_ops, 1000) %>% mutate(ring = "1000m (Control)"),
  st_buffer(master_ops, 600)  %>% mutate(ring = "600m (Halo)"),
  st_buffer(master_ops, 300)  %>% mutate(ring = "300m (Core)")
) %>%
  mutate(ring = factor(ring, levels = c("1500m (Control)", "1000m (Control)", "600m (Halo)", "300m (Core)")))

cat("Aggregating pixel data for facility", target_id, "...\n")

# 4. Filter pixels for our target and aggregate into Pre vs Post
pixels_sf <- d$pixel_data %>%
  filter(export_id == target_id) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(5070)

pixels_agg <- pixels_sf %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
    period = case_when(
      year <= year_op - 3 ~ "Pre-Construction",
      year >= year_op     ~ "Post-Operation",
      TRUE ~ "Exclude"
    )
  ) %>%
  st_drop_geometry() %>%
  filter(period != "Exclude") %>%
  mutate(period = factor(period, levels = c("Pre-Construction", "Post-Operation"))) %>%
  group_by(x, y, period) %>%
  summarise(mean_lst = mean(LST_Celsius, na.rm = TRUE), .groups = "drop")

# 5. Plot the Side-by-Side Heatmap
cat("Rendering maps...\n")
p <- ggplot() +
  # FIX 1: Use geom_point (shape 15) to bypass the spatial tile bug
  geom_point(data = pixels_agg, aes(x = x, y = y, color = mean_lst), shape = 15, size = 2.5) +
  # Overlay the treatment and control rings
  geom_sf(data = rings, fill = NA, color = "black", linewidth = 0.5, linetype = "dashed") +
  # FIX 2: Use fermenter to create discrete color bins like the reference map
  scale_color_fermenter(palette = "RdYlGn", direction = -1, n.breaks = 7, name = "Mean LST (°C)") +
  facet_wrap(~ period) +
  labs(
    title = paste("Localized Heat Effect: Hyperscale Facility", target_id),
    caption = paste("Opened in", year_op, "| Dashed outlines represent 300m, 600m, 1000m, and 1500m radii")
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 13, margin = margin(t = 12)),
    strip.text = element_text(size = 14, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.spacing = unit(2, "lines")
  ) +
  coord_sf(datum = NA)

ggsave(here("figures", paste0("heatmap_fac_", target_id, ".png")), p, width = 11.9, height = 5.9, dpi = 300, bg = "white")
cat("Success! Saved to figures/heatmap_fac_", target_id, ".png\n", sep="")
# fig_hyperscale_trend.R — Panel A: raw LST by band; Panel B: treat-minus-outer gap
library(tidyverse); library(here); library(duckdb); library(patchwork)
options(bitmapType = "cairo")

source(here("analysis","heat","load_hyperscale_panel.R")); d <- load_hyperscale_panel()

trend <- d$pixel_data

dc_xy <- d$dc_points

xy <- sf::sf_project(sf::st_crs(4326), sf::st_crs(5070),
                     as.matrix(trend[, c("longitude","latitude")]))
trend$px <- xy[,1]; trend$py <- xy[,2]

banded <- trend %>%
  left_join(dc_xy, by = "export_id") %>%
  mutate(dist = sqrt((px-dc_x)^2 + (py-dc_y)^2),
         band = case_when(dist < 600                 ~ "Facility (0\u2013600 m)",
                          dist >= 1000 & dist <= 1500 ~ "Outer (1000\u20131500 m)",
                          TRUE ~ NA_character_)) %>%
  filter(!is.na(band)) %>%
  # month-balanced: DC x band x year x month first, then months -> year
  group_by(export_id, band, year, month) %>%
  summarise(mo_mean = mean(LST_Celsius), .groups = "drop") %>%
  group_by(export_id, band, year) %>%
  summarise(dc_mean = mean(mo_mean), .groups = "drop")

# Panel A: pooled raw LST by band
pa_dat <- banded %>% group_by(band, year) %>%
  summarise(m = mean(dc_mean), se = sd(dc_mean)/sqrt(n()), .groups = "drop")
pA <- ggplot(pa_dat, aes(year, m, color = band, fill = band)) +
  geom_ribbon(aes(ymin = m-se, ymax = m+se), alpha = .12, color = NA) +
  geom_line(linewidth = .9) + geom_point(size = 1.4) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(title = "A. Mean LST by distance band", x = NULL, y = "LST (\u00b0C)",
       color = NULL, fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# Panel B: per-DC gap (facility minus outer), spaghetti + pooled
gap <- banded %>%
  pivot_wider(names_from = band, values_from = dc_mean) %>%
  rename(fac = starts_with("Facility"), outer = starts_with("Outer")) %>%
  filter(!is.na(fac), !is.na(outer)) %>%
  mutate(gap = fac - outer) 

gap_pool <- gap %>% group_by(year) %>%
  summarise(m = mean(gap), se = sd(gap)/sqrt(n()), .groups = "drop")
pB <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(data = gap, aes(year, gap, group = export_id),
            color = "grey75", linewidth = .4) +
  geom_ribbon(data = gap_pool, aes(year, ymin = m-se, ymax = m+se),
              alpha = .15, fill = "firebrick") +
  geom_line(data = gap_pool, aes(year, m), color = "firebrick", linewidth = 1.1) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(title = "B. Facility-minus-outer temperature gap",
       subtitle = "Grey: individual facilities; red: pooled mean \u00b1 SE",
       x = NULL, y = "\u0394LST (\u00b0C)") +
  theme_minimal()

p <- pA / pB + plot_annotation(
  title = "Hyperscale facilities: thermal trend, 2016\u20132025",
  caption = paste0("30 m Landsat 8/9, scene cloud cover <30% (applied at extraction), ",
                   "month-balanced annual means, n = 10 facilities."))
ggsave(here("figures","hyperscale_trend_bands.png"), p, width = 11, height = 9, dpi = 150)
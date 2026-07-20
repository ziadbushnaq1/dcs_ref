# audit_control_rings.R — hot-lobe screen for uninventoried neighbors
library(tidyverse); library(sf); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
d <- load_hyperscale_panel()

for (eid in sort(unique(d$dc_points$export_id))) {
  target <- d$dc_points %>% filter(export_id == eid)
  yop <- target$year_operational
  cls <- assign_treatment_zones(
    d$pixel_data %>% filter(export_id == eid), target,
    0, 600, 1000, 1500, sensor = "landsat")
  chg <- cls %>%
    filter(status == "Control") %>%
    mutate(period = if_else(year >= yop, "post", "pre")) %>%
    group_by(longitude, latitude, period) %>%
    summarise(m = mean(LST_Celsius, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = m) %>%
    mutate(delta = post - pre) %>% filter(!is.na(delta))
  p <- ggplot(chg, aes(longitude, latitude, color = delta)) +
    geom_point(size = .4) +
    scale_color_gradient2(low = "navy", mid = "grey90", high = "red",
                          limits = c(-4, 4), oob = scales::squish) +
    coord_equal() +
    labs(title = paste0("Control-ring LST change (post-pre): ", eid))
  ggsave(here("figures", paste0("audit_ctrl_", eid, ".png")),
         p, width = 8, height = 7, dpi = 130)
  rm(cls, chg); gc()
}
# check_seam_placebo.R — L5->L8 sensor-seam placebo.
# Facilities opening >= 2016: untreated across 2011->2013. Their treatment-zone
# relative LST must show NO step at the seam.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
options(bitmapType = "cairo")

d <- load_hyperscale_panel()

# Placebo cohort: opened 2016+ -> everything through 2015 is pre-treatment
placebo_ids <- d$dc_points %>% st_drop_geometry() %>%
  filter(year_operational >= 2019) %>% pull(export_id)
cat("Placebo cohort (opened >=2019):", length(placebo_ids), "facilities\n")
stopifnot(length(placebo_ids) >= 3)

cls <- assign_treatment_zones(
  d$pixel_data %>% filter(export_id %in% placebo_ids),
  d$dc_points %>% filter(export_id %in% placebo_ids),
  300, 600, 1000, 1500, sensor = "landsat")
pan <- build_did_panel(cls, sensor = "landsat_monthly") %>%
  filter(year <= 2018)        
pan_all <- build_did_panel(cls, sensor = "landsat_monthly")

# Figure: treatment-zone relative LST by year, seam marked
pd <- pan %>%
  filter(status == "Treatment", !is.na(relative_lst)) %>%
  group_by(export_id, year) %>%
  summarise(dc_mean = mean(relative_lst), .groups = "drop") %>%
  group_by(year) %>%
  summarise(m = mean(dc_mean), se = sd(dc_mean)/sqrt(n()), n_dc = n(),
            .groups = "drop")

p <- ggplot(pd, aes(year, m)) +
  annotate("rect", xmin = 2011.5, xmax = 2012.5, ymin = -Inf, ymax = Inf,
           alpha = .15, fill = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_ribbon(aes(ymin = m - se, ymax = m + se), alpha = .15, fill = "navy") +
  geom_line(color = "navy", linewidth = .9) + geom_point(size = 1.6) +
  labs(title = "Sensor-seam placebo: not-yet-treated facilities",
       subtitle = paste0("Treatment-zone relative LST, facilities opening \u22652016 (n=",
                         length(placebo_ids), "). Grey band: 2012 data gap (L5 ends, L8 begins). ",
                         "A level step across the band would indicate sensor-harmonization failure."),
       x = NULL, y = "Relative LST (\u00b0C)") +
  theme_minimal()
ggsave(here("figures","seam_placebo.png"), p, width = 10, height = 5.5, dpi = 150)

# Formal test: post-2013 indicator on the pre-treatment panel
seam_ctrl <- feols(relative_lst ~ i(sensor, ref = 5) | pixel_id,
                   data = pan_all %>% filter(status == "Control"),
                   cluster = ~export_id)
print('seam_ctrl\n')
print(summary(seam_ctrl)$coeftable)

seam_test <- feols(relative_lst ~ i(sensor, ref = 5) | pixel_id,
                   data = pan %>% filter(status == "Treatment"),
                   cluster = ~export_id)
cat("\nSeam step (sensor 8 vs 5, treatment pixels, pre-treatment years):\n")
print(summary(seam_test)$coeftable)
cat("\nPASS if the sensor-8 coefficient is small (|est| well under the",
    "0.35 headline) and insignificant.\n")
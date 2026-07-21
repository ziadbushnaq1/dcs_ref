# fig_custom_event_study.R
library(tidyverse)
library(sf)
library(fixest)
library(here)
library(duckdb)

options(bitmapType = "cairo")
setFixest_nthreads(8)
data.table::setDTthreads(8)

source(here("analysis", "heat", "an_01_functions.R"))
source(here("analysis", "heat", "load_hyperscale_panel.R"))

cat("Loading panel data...\n")
d <- load_hyperscale_panel()

# Run the headline model with is_event_study = TRUE
cat("Fitting event study model for headline specification...\n")
es_res <- run_pixel_analysis(
  raw_csv_data = d$pixel_data,
  dc_points = d$dc_points,
  min_treat_m = 0, max_treat_m = 600,
  min_control_m = 1000, max_control_m = 1500,
  fe_spec = "pixel_id + year",
  use_power_builtout = FALSE,
  use_intensity = TRUE,
  master_dcs = d$master_ops,
  contamination_master = d$master_full,
  contam_timing = "dynamic",
  contam_buffer_years = 0,
  is_event_study = TRUE,     # Activates the i(relative_year) fixest formula
  ref_year = -4,             # Sets year -4 as the baseline zero
  use_construction = FALSE,  # Ignored by fixest during event studies
  cluster_var = "export_id"
)

# Extract coefficients from the fixest model object
coefs <- as.data.frame(summary(es_res$model)$coeftable)
coefs$term <- rownames(coefs)

# Parse the relative years and filter for the event study terms
plot_data <- coefs %>%
  filter(grepl("relative_year::", term)) %>%
  mutate(
    # Extract the integer value of the relative year from the row name
    rel_year = as.numeric(str_extract(term, "-?\\d+")),
    estimate = Estimate,
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  )

# Manually add the reference year (fixest drops it, so it is exactly 0)
ref_row <- tibble(
  term = "reference",
  rel_year = -4,
  estimate = 0,
  conf.low = 0,
  conf.high = 0
)

# Bind, sort, and truncate the window for a clean plot
plot_data <- bind_rows(plot_data, ref_row) %>%
  arrange(rel_year) %>%
  filter(rel_year >= -5 & rel_year <= 5)

cat("Rendering plot...\n")
p <- ggplot(plot_data, aes(x = rel_year, y = estimate)) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_vline(xintercept = -4, color = "#d7191c", linetype = "dashed", linewidth = 0.8) +
  # Use error bars instead of a ribbon
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "#4C78A8", width = 0.2, linewidth = 0.8) +
  geom_line(color = "#4C78A8", linewidth = 1) +
  geom_point(color = "#4C78A8", size = 3) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  labs(
    title = "Event Study: LST Near Data Centers (0 to 600m)",
    subtitle = "Relative to year -4. Error bars indicate 95% confidence intervals.",
    x = "Years Relative to Operation",
    y = "Temperature Difference (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    panel.grid.minor.x = element_blank()
  )

ggsave(here("figures", "custom_event_study_headline.png"), p, width = 10, height = 6, dpi = 300, bg = "white")
cat("Success! Saved custom event study plot to figures/custom_event_study_headline.png\n")
# fig_event_study.R — event study, all-DC sample, headline specification.
# Reference year -4 (pre-construction), so relative years -8..-5 test parallel
# trends on genuinely undisturbed land and -3..-1 show the construction ramp.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
options(bitmapType = "cairo")
setFixest_nthreads(8); data.table::setDTthreads(8)

# ── Load all-DC panel ───────────────────────────────────────────────────
f <- here("data","processed","landsat_all146","landsat_all146_obs30m_l89.csv")
con <- dbConnect(duckdb()); dbExecute(con, "SET memory_limit='24GB'")
pixel_data <- dbGetQuery(con, glue::glue("
  WITH src AS (
    SELECT export_id, longitude, latitude, year, month, date_yyyymmdd,
           LST_Celsius, Emissivity, ST_uncertainty, Elevation, scene_cloud_cover
    FROM read_csv('{f}', ignore_errors=true)
    WHERE scene_cloud_cover < 30),
  counts AS (
    SELECT export_id, year, month, date_yyyymmdd,
           COUNT(*) FILTER (WHERE LST_Celsius IS NOT NULL) AS n_valid
    FROM src GROUP BY 1,2,3,4),
  best AS (
    SELECT export_id, year, month, date_yyyymmdd FROM (
      SELECT *, ROW_NUMBER() OVER (PARTITION BY export_id, year, month
              ORDER BY n_valid DESC, date_yyyymmdd ASC) rk FROM counts) WHERE rk=1)
  SELECT src.* FROM src JOIN best USING (export_id, year, month, date_yyyymmdd)"))
dbDisconnect(con)
cat("Panel rows:", format(nrow(pixel_data), big.mark=","), "\n")

dc_points_all <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"),
                          show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
master_full <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                        show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
master_ops <- master_full %>% filter(stage == "Operational")
master_oc  <- master_full %>% filter(stage %in% c("Operational","Under Construction"))

REF <- -4; WIN <- c(-8, 8)

# ── Headline spec, event-study form ─────────────────────────────────────
# use_construction = FALSE: the event-time dummies already trace the
# construction ramp year by year, so a pooled dummy would be collinear.
es <- run_pixel_analysis(
  raw_csv_data = pixel_data, dc_points = dc_points_all,
  min_treat_m = 0, max_treat_m = 600,
  min_control_m = 1000, max_control_m = 1500,
  fe_spec = "pixel_id + year", filter_elev = FALSE,
  is_event_study = TRUE, ref_year = REF,
  use_intensity = TRUE, master_dcs = master_ops,
  contamination_master = master_oc,
  contam_timing = "dynamic", contam_buffer_years = 0,
  sensor = "landsat_monthly", use_construction = FALSE,
  cluster_var = "export_id")

ct <- as.data.frame(summary(es$model)$coeftable) %>%
  rownames_to_column("term")

plot_data <- ct %>%
  filter(grepl("relative_year::", term)) %>%
  mutate(rel_year = as.numeric(str_extract(term, "-?\\d+")),
         estimate = Estimate,
         conf.low  = Estimate - 1.96 * `Std. Error`,
         conf.high = Estimate + 1.96 * `Std. Error`) %>%
  bind_rows(tibble(rel_year = REF, estimate = 0,
                   conf.low = 0, conf.high = 0)) %>%
  filter(between(rel_year, WIN[1], WIN[2])) %>%
  arrange(rel_year)

# Pre-trend test: are the clean pre-period coefficients jointly zero?
pre <- plot_data %>% filter(rel_year < REF)
cat("\nClean pre-period (rel <", REF, "): mean =",
    round(mean(pre$estimate), 3), "| max |coef| =",
    round(max(abs(pre$estimate)), 3), "\n")

p <- ggplot(plot_data, aes(rel_year, estimate)) +
  annotate("rect", xmin = -3.5, xmax = -0.5, ymin = -Inf, ymax = Inf,
           fill = "#7B2841", alpha = 0.06) +
  annotate("text", x = -2, y = Inf, label = "construction", vjust = 1.6,
           size = 3.2, color = "#7B2841") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = REF + 0.5, linetype = "dashed",
             color = "grey35", linewidth = 0.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, linewidth = 0.45, color = "black") +
  geom_point(size = 1.6, color = "black") +
  scale_x_continuous(breaks = seq(WIN[1], WIN[2], 2)) +
  labs(title = "Event Study: Land Surface Temperature, 145 Data Centers",
       caption = paste0("Relative to year ", REF,
                         " (pre-construction baseline). 95% CI, facility-clustered."),
       x = "Years Relative to Opening", y = "Temperature Effect (\u00b0C)") +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, margin = margin(t = 12)),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", color = "black", size = 11))

ggsave(here("figures","event_study_all.png"), p,
       width = 11.9, height = 5.9, dpi = 300, bg = "white")
write_csv(plot_data, here("results","event_study_all.csv"))
cat("Saved: figures/event_study_all.png\n")
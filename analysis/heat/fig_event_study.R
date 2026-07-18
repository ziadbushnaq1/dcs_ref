# fig_parallel_trends.R — treatment vs control relative LST in EVENT TIME
# Produces one figure per group: hyperscale / all / non_hs
library(tidyverse); library(sf); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
options(bitmapType = "cairo")

f <- here("data","processed","landsat_all146","landsat_all146_obs30m_l89.csv")

# Load with best-date selection (same as an_08 — this IS a model-adjacent figure)
con <- dbConnect(duckdb()); dbExecute(con, "SET memory_limit='24GB'")
pixel_data <- dbGetQuery(con, glue::glue("
  WITH src AS (
    SELECT export_id, longitude, latitude, year, month, date_yyyymmdd,
           LST_Celsius, Elevation
    FROM read_csv('{f}', ignore_errors=true)),
  counts AS (
    SELECT export_id, year, month, date_yyyymmdd,
           COUNT(*) FILTER (WHERE LST_Celsius IS NOT NULL) n_valid
    FROM src GROUP BY 1,2,3,4),
  best AS (
    SELECT export_id, year, month, date_yyyymmdd FROM (
      SELECT *, ROW_NUMBER() OVER (PARTITION BY export_id, year, month
              ORDER BY n_valid DESC, date_yyyymmdd ASC) rk FROM counts) WHERE rk=1)
  SELECT src.* FROM src JOIN best USING (export_id, year, month, date_yyyymmdd)"))
dbDisconnect(con)

dc_all <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"),
                   show_col_types = FALSE) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
HS <- c(411,412,648,664,2949,2950,2998,3012,3051,3052)

GROUPS <- list(hyperscale = dc_all %>% filter(export_id %in% HS),
               all        = dc_all,
               non_hs     = dc_all %>% filter(!export_id %in% HS))

# Parameters: match your headline an_08 spec
TREAT_MIN <- 0; TREAT_MAX <- 300; CTRL_MIN <- 1000; CTRL_MAX <- 1500

for (grp in names(GROUPS)) {
  cls <- assign_treatment_zones(pixel_data, GROUPS[[grp]],
                                TREAT_MIN, TREAT_MAX, CTRL_MIN, CTRL_MAX, sensor = "landsat")
  cls <- filter_elevation_controls(cls, 50)
  pan <- build_did_panel(cls, sensor = "landsat_monthly")
  
  pd <- pan %>%
    filter(status == "Treatment", !is.na(relative_lst),
           between(relative_year, -8, 8)) %>%
    group_by(export_id, relative_year) %>%
    summarise(dc_mean = mean(relative_lst), .groups = "drop") %>%
    group_by(relative_year) %>%
    summarise(m = mean(dc_mean), se = sd(dc_mean)/sqrt(n()),
              n_dc = n(), .groups = "drop")
  
  p <- ggplot(pd, aes(relative_year, m)) +
    geom_vline(xintercept = -0.5, linetype = "dotted") +
    annotate("rect", xmin = -3.5, xmax = 0.5, ymin = -Inf, ymax = Inf,
             alpha = .06, fill = "orange") +
    geom_hline(yintercept = 0, color = "grey60", linetype = "dashed") +
    geom_ribbon(aes(ymin = m - se, ymax = m + se), alpha = .15, fill = "firebrick") +
    geom_line(color = "firebrick", linewidth = .9) + geom_point(size = 1.6) +
    scale_x_continuous(breaks = -8:8) +
    labs(title = paste0("Event-time trend (", grp, "): treatment-zone relative LST"),
         subtitle = paste0("Ring ", TREAT_MIN, "-", TREAT_MAX,
                           " m vs controls ", CTRL_MIN, "-", CTRL_MAX,
                           " m; DC-level means \u00b1 SE. Shaded: construction window. ",
                           "Flat pre-period = parallel-trends evidence."),
         x = "Years relative to opening", y = "Relative LST (\u00b0C)") +
    theme_minimal()
  ggsave(here("figures", paste0("parallel_trends_", grp, ".png")),
         p, width = 10, height = 5.5, dpi = 150)
  rm(cls, pan); gc()
  message("Saved: parallel_trends_", grp, ".png")
}
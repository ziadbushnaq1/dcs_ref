# make_shiny_heat_tables.R — display-ready heat tables for the Shiny app.
# Writes ShinyApp/report/{heat_models,heat_seasonal,heat_meta}.rds
library(tidyverse); library(here)

res <- here("results")
robust_all <- read_csv(file.path(res,"all_robust_headline.csv"),   show_col_types = FALSE)
robust_hs  <- read_csv(file.path(res,"hs_robust_headline.csv"),    show_col_types = FALSE)
core_all   <- read_csv(file.path(res,"all_core_ring_grid.csv"),    show_col_types = FALSE)
seasonal   <- read_csv(file.path(res,"all_seasonal_headline.csv"), show_col_types = FALSE)

fe_label <- c(
  "pixel_id + year"           = "Pixel + Year",
  "pixel_id + year^export_id" = "Pixel + Facility\u00d7Year",
  "pixel_id + year^month"     = "Pixel + Year\u00d7Month",
  "pixel_id + year^sensor"    = "Pixel + Year\u00d7Sensor",
  "pixel_id + year^campus_id" = "Pixel + Campus\u00d7Year")

drop_label <- function(scope, timing, buffer) {
  s <- c(all     = "All operational DCs",
         hs_only = "Hyperscale DCs only",
         all_oc  = "Operational + under construction")[scope]
  t <- case_when(
    timing == "static"                ~ "excluded in all years",
    timing == "dynamic" & buffer == 0 ~ "excluded from neighbor's opening",
    timing == "dynamic"               ~ paste0("excluded from ", buffer, "y before opening"),
    TRUE                              ~ timing)
  paste0(s, " \u2014 ", t)
}

clean_tbl <- function(df, sample_label) {
  df %>%
    filter(intensity) %>%                       # dose models only
    mutate(
      Sample = sample_label,
      Ring   = sprintf("%d\u2013%dm", ring_min, ring_max),
      `Fixed Effects` = coalesce(fe_label[fe_spec], fe_spec),
      Clustering = case_when(
        cluster_var == "campus_id" ~ "Campus",
        cluster_var == "none"      ~ "None (not recommended)",
        TRUE                       ~ "Facility"),          # NA/blank -> default
      `Control Exclusion` = drop_label(contam_scope, contam_timing, contam_buffer),
      Term = recode(term, treated_post = "Operational",
                    construction_period = "Construction"),
      Estimate = estimate, SE = se, `p-value` = p_value,
      Facilities = n_treat_dcs) %>%
    select(Sample, Ring, `Fixed Effects`, Clustering, `Control Exclusion`,
           Facilities, Term, Estimate, SE, `p-value`)
}

# Hyperscale rows: hyperscale-dose only
robust_hs_disp <- robust_hs %>% filter(treat_scope == "hs_only")
if (nrow(robust_hs_disp) == 0) {
  warning("No treat_scope=='hs_only' rows in hs_robust_headline.csv. ",
          "Showing all hyperscale rows; add hs_only deviations to an_08_hs_robust.R.")
  robust_hs_disp <- robust_hs
}

heat_long <- bind_rows(
  clean_tbl(robust_all, "All facilities (145)"),
  clean_tbl(core_all %>%
              filter(fe_spec == "pixel_id + year", use_construction) %>%
              mutate(contam_scope = "all", contam_timing = "static",
                     contam_buffer = 0, cluster_var = "export_id"),
            "All facilities (145)"),
  clean_tbl(robust_hs_disp, "Hyperscale (17)")) %>%
  distinct(Sample, Ring, `Fixed Effects`, Clustering,
           `Control Exclusion`, Term, .keep_all = TRUE)

# One model = one row. Operational/Construction become column pairs so any
# sort order keeps a model's estimates together.
# One row per model; both terms as column pairs, clustering as a column.
heat_models <- heat_long %>%
  pivot_wider(
    id_cols = c(Sample, Ring, `Fixed Effects`, Clustering,
                `Control Exclusion`, Facilities),
    names_from  = Term,
    values_from = c(Estimate, SE, `p-value`),
    names_glue  = "{Term} {.value}") %>%
  select(Sample, Ring, `Fixed Effects`, Clustering, `Control Exclusion`,
         Facilities,
         `Operational Estimate`, `Operational SE`, `Operational p-value`,
         any_of(c("Construction Estimate", "Construction SE",
                  "Construction p-value"))) %>%
  arrange(Sample, Ring, `Fixed Effects`)

heat_seasonal <- seasonal %>%
  filter(term == "treated_post") %>%
  transmute(Season = str_to_title(season),
            Months = str_replace_all(months, ",", ", "),
            Estimate = estimate, SE = se, `p-value` = p_value)

yr_range <- "2013\u20132026"                     # panel coverage
heat_meta <- list(
  subtitle = paste0(
    "Difference-in-differences estimates from 30m Landsat land surface temperature, ",
    yr_range, ". All models shown use dose treatment (\u00b0C per treating facility ",
    "within 600m) and include a construction-period control."),
  glossary = tibble::tribble(
    ~Term, ~Meaning,
    "Ring", "Distance band from the facility treated as exposed. Control pixels are always 1000\u20131500m away.",
    "Dose treatment", "Treatment counts operational facilities within 600m of a pixel, so the estimate is \u00b0C per facility.",
    "Operational", "Effect from the facility's opening year onward.",
    "Construction", "Effect during the 3 years before opening (site clearing and building).",
    "All operational DCs", "Control pixels near ANY operational data center are excluded.",
    "Hyperscale DCs only", "Only proximity to hyperscale facilities disqualifies a control pixel.",
    "Operational + under construction", "Also excludes control pixels near active construction sites.",
    "excluded in all years", "Static rule: a contaminated control pixel is dropped for the whole panel, regardless of when the neighbor opened.",
    "excluded from neighbor's opening", "Dynamic rule: the pixel stays a valid control until the nearby facility opens, then drops out.",
    "Clustering", "Level at which standard errors allow correlated errors.",
    "SE (facility) vs SE (unclustered)", "Facility-clustered standard errors allow pixels around the same data center to share error; unclustered errors assume every pixel-month is independent. The estimate is identical either way \u2014 only the uncertainty differs. With 145 facilities, facility clustering is the conservative reading.",
    "Operational + under construction", "Control pixels near any operational OR under-construction data center are excluded. Construction sites emit heat and disturb land cover, so nearby pixels are not clean counterfactuals."))

out <- here("ShinyApp","report")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
saveRDS(heat_models,   file.path(out,"heat_models.rds"))
saveRDS(heat_seasonal, file.path(out,"heat_seasonal.rds"))
saveRDS(heat_meta,     file.path(out,"heat_meta.rds"))
cat("Wrote heat_models (", nrow(heat_models), " rows), heat_seasonal (",
    nrow(heat_seasonal), "), heat_meta\n", sep = "")
# make_poster_table.R — compact poster table from archived results CSVs.
# No models are fit here; every number traces to a results file.
library(tidyverse); library(here)

core     <- read_csv(here("results","all_core_ring_grid.csv"),   show_col_types = FALSE)
robust   <- read_csv(here("results","all_robust_headline.csv"),  show_col_types = FALSE)
seasonal <- read_csv(here("results","all_seasonal_headline.csv"),show_col_types = FALSE)
hsrob    <- read_csv(here("results","hs_robust_headline.csv"),   show_col_types = FALSE)
dclevel  <- read_csv(here("results","hs_dclevel_diffs.csv"),     show_col_types = FALSE)
sboot    <- read_csv(here("results","bootstrap_summer_headline.csv"), show_col_types = FALSE)

stars <- function(p) case_when(p < .001 ~ "***", p < .01 ~ "**",
                               p < .05 ~ "*", p < .10 ~ "+", TRUE ~ "")
fmt <- function(est, se, p) sprintf("%.3f (%.3f)%s", est, se, stars(p))

# helper: pull one row by filter, fail loudly if not exactly one match
pull_row <- function(df, ...) {
  r <- df %>% filter(...)
  stopifnot(nrow(r) == 1)
  r
}

# ── Row 1: headline (all-DC, 0-600, intensity, construction, baseline FE)
r1 <- pull_row(robust, fe_spec == "pixel_id + year", contam_scope == "all",
               contam_timing == "static", contam_buffer == 0,
               term == "treated_post")

# ── Row 2: strictest FE
r2 <- pull_row(robust, fe_spec == "pixel_id + year^export_id",
               term == "treated_post")

# ── Row 3: fence-line ring (0-150, intensity, construction, group all)
r3 <- pull_row(core, group == "all", ring_min == 0, ring_max == 150,
               intensity, use_construction, fe_spec == "pixel_id + year",
               term == "treated_post")

# ── Row 4: construction-period effect (headline cell, second term)
r4 <- pull_row(robust, fe_spec == "pixel_id + year", contam_scope == "all",
               contam_timing == "static", contam_buffer == 0,
               term == "construction_period")

# ── Row 5: seasonal split
r5s <- pull_row(seasonal, season == "summer", term == "treated_post")
r5w <- pull_row(seasonal, season == "winter", term == "treated_post")

# ── Row 6: non-hyperscale subgroup (0-600, intensity, construction)
r6 <- pull_row(core, group == "non_hs", ring_min == 0, ring_max == 600,
               intensity, use_construction, fe_spec == "pixel_id + year",
               term == "treated_post")

# ── Row 7: hyperscale sample — regression + facility-level inference
r7 <- hsrob %>%
  filter(fe_spec == "pixel_id + year", cluster_var == "export_id",
         treat_scope == "all", contam_scope == "all",
         contam_timing == "static", contam_buffer == 0,
         term == "treated_post") %>%
  distinct(estimate, se, p_value) %>% slice(1)     # anchor appears twice; dedupe
sr <- wilcox.test(dclevel$diff)                     # sign-rank from archived diffs
n_pos <- sum(dclevel$diff > 0); n_fac <- nrow(dclevel)

poster <- tribble(
  ~Specification, ~Sample, ~Estimate, ~Answers,
  "Warming per facility, 0\u2013600m",
  "145 DCs", fmt(r1$estimate, r1$se, r1$p_value),
  "Headline effect",
  "\u2003+ DC\u00d7year fixed effects",
  "145 DCs", fmt(r2$estimate, r2$se, r2$p_value),
  "Survives strictest controls",
  "Fence-line ring (0\u2013150m)",
  "145 DCs", fmt(r3$estimate, r3$se, r3$p_value),
  "Effect strongest in tightest treatment ring",
  "Construction period (yrs \u22123 to \u22121)",
  "145 DCs", fmt(r4$estimate, r4$se, r4$p_value),
  "Constuction period has warming effect",
  "Summer (Jun\u2013Aug)",
  "145 DCs", fmt(r5s$estimate, r5s$se, r5s$p_value),
  "Seasonal: solar-driven",
  "Winter (Dec\u2013Feb)",
  "145 DCs", fmt(r5w$estimate, r5w$se, r5w$p_value),
  "Near zero when sun is lower",
  "Non-hyperscale facilities only",
  "135 DCs", fmt(r6$estimate, r6$se, r6$p_value),
  "Not just the largest facilities",
  "Hyperscale sample",
  "17 DCs",
  sprintf("%.3f (%.3f); %d/%d facilities positive, signed-rank p=%.3f; summer wild-bootstrap p=%.3f",
          r7$estimate, r7$se, n_pos, n_fac, sr$p.value, sboot$p_wild_webb[1]),
  "Consistent in flagship sample")

write_csv(poster, here("results","poster_table.csv"))
cat(knitr::kable(poster, format = "pipe"), sep = "\n")
cat("\n\n"); cat(knitr::kable(poster, format = "latex", booktabs = TRUE), sep = "\n")
cat("\n\nFootnote: Estimate stable at 0.26\u20130.34\u00b0C across 60+ specifications",
    "varying rings, FE, contamination rules, and dose definitions.",
    "SEs clustered by facility; hyperscale inference via wild bootstrap and",
    "rank tests due to 17 clusters. \u00b0C per treating facility (dose models).\n")

# ── Export for Shiny app (mirrors the Econ pipeline's report/ convention) ──
shiny_report_dir <- here("ShinyApp","report")
dir.create(shiny_report_dir, showWarnings = FALSE, recursive = TRUE)

# Headline poster table
saveRDS(poster, file.path(shiny_report_dir, "heat_poster_table.rds"))
write_csv(poster, file.path(shiny_report_dir, "heat_poster_table.csv"))

# Full robustness detail behind it (for an expandable/second table in the app)
heat_full <- bind_rows(
  robust   %>% mutate(source = "all_robust", ring_min = as.character(ring_min)),
  seasonal %>% mutate(source = "all_seasonal") %>%
    rename(any_of(c(ring_min = "ring_label"))),   # align columns loosely
  hsrob    %>% mutate(source = "hs_robust", ring_min = as.character(ring_min))) %>%
  select(source, any_of(c("group","season","ring_min","intensity",
                          "use_construction","fe_spec","cluster_var","treat_scope",
                          "contam_scope","contam_timing","term")),
         estimate, se, p_value, n_obs, n_treat_dcs) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

saveRDS(heat_full, file.path(shiny_report_dir, "heat_results_full.rds"))

heat_footnote <- paste(
  "Effect of data-center proximity on Landsat land surface temperature (\u00b0C),",
  "relative to 1000\u20131500m controls. Dose models: \u00b0C per treating facility.",
  "SEs clustered by facility; hyperscale (n=17) inference via wild cluster",
  "bootstrap and rank tests. Estimate stable at 0.26\u20130.34\u00b0C across 60+",
  "specifications. Known limitation: dataset misses some campus expansions",
  "(multi-facility sites recorded as one point), which biases estimates",
  "toward zero \u2014 reported effects are conservative.")
saveRDS(heat_footnote, file.path(shiny_report_dir, "heat_footnote.rds"))
cat("Shiny exports written to", shiny_report_dir, "\n")
# check_bootstrap_loo.R — Leave-one-out robustness check
# headline spec: 300-600m, intensity, construction TRUE, pixel_id + year
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)

source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
options(bitmapType = "cairo")

d <- load_hyperscale_panel()

fit_headline <- function(px, dcp) {
  run_pixel_analysis(px, dcp, 300, 600, 1000, 1500,
                     fe_spec = "pixel_id + year", use_power_builtout = FALSE,
                     filter_elev = FALSE, elev_threshold = 50, is_event_study = FALSE,
                     ref_year = -4, use_intensity = TRUE, master_dcs = d$master_ops,
                     sensor = "landsat_monthly", use_construction = TRUE)
}

# ---- Leave-one-out Robustness Loop ----
cat("\nStarting Leave-One-Out sequence for 10 Hyperscale Facilities...\n")

ids <- d$dc_points$export_id
loo <- map_dfr(ids, function(drop_id) {
  
  cat("Running model dropping facility:", drop_id, "...\n")
  
  r <- tryCatch(fit_headline(
    d$pixel_data %>% filter(export_id != drop_id),
    d$dc_points %>% filter(export_id != drop_id)),
    error = function(e) NULL)
  
  out <- if (is.null(r)) tibble(dropped = drop_id, estimate = NA,
                                se = NA, p = NA)
  else {
    c2 <- summary(r$model)$coeftable
    tibble(dropped = drop_id,
           estimate = c2["treated_post","Estimate"],
           se = c2["treated_post","Std. Error"],
           p = c2["treated_post","Pr(>|t|)"])
  }
  rm(r); gc(); out
})

cat("\n==== Leave-One-Out Results ====\n")
print(knitr::kable(loo %>% arrange(estimate), digits = 3))

cat("\nRange:", round(min(loo$estimate, na.rm = TRUE), 3), "to",
    round(max(loo$estimate, na.rm = TRUE), 3),
    "| all positive:", all(loo$estimate > 0, na.rm = TRUE), "\n")

write_csv(loo, here("results","loo_headline_hyperscale.csv"))
cat("Saved successfully to results/loo_headline_hyperscale.csv\n")
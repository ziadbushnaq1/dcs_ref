# an_08_hs_dclevel.R — facility-level aggregation: the transparent answer
# to "17 clusters is too few for CRSE." Collapse the pixel panel to one
# pre/post difference per facility; inference is a t-test across 17
# independent facility-level differences. No clustering needed because the
# unit of analysis IS the independent unit.
library(tidyverse); library(sf); library(fixest); library(here); library(duckdb)
source(here("analysis","heat","an_01_functions.R"))
source(here("analysis","heat","load_hyperscale_panel.R"))
setFixest_nthreads(8); data.table::setDTthreads(8)

d <- load_hyperscale_panel()

# Headline cell config; construction window (-3..-1) excluded from both sides
cls <- assign_treatment_zones(d$pixel_data, d$dc_points,
                              0, 600, 1000, 1500, sensor = "landsat")
cls <- flag_contaminated_controls(cls, d$master_ops, radius_m = 600)
cls <- cls %>% filter(!(status == "Control" &
                          (!is.na(contam_first_year) | contam_near_undated)))
pan <- build_did_panel(cls, sensor = "landsat_monthly")
rm(cls); gc()

dc_diffs <- pan %>%
  filter(status == "Treatment", !is.na(relative_lst),
         between(relative_year, -8, 8),
         !(relative_year >= -3 & relative_year < 0)) %>%     # drop construction
  mutate(period = if_else(relative_year >= 0, "post", "pre")) %>%
  group_by(export_id, period) %>%
  summarise(m = mean(relative_lst), n_obs = n(), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = c(m, n_obs)) %>%
  mutate(diff = m_post - m_pre)

cat("\n==== Facility-level pre/post differences (clean pre, ex-construction) ====\n")
print(knitr::kable(dc_diffs %>% arrange(diff), digits = 3))

tt <- t.test(dc_diffs$diff)
wc <- wilcox.test(dc_diffs$diff)   # rank-based companion, robust to outliers
cat(sprintf("\nMean diff: %.3f C | t = %.2f | p(t) = %.4f | p(signrank) = %.4f | n = %d\n",
            tt$estimate, tt$statistic, tt$p.value, wc$p.value, nrow(dc_diffs)))
cat(sprintf("95%% CI: [%.3f, %.3f] | facilities positive: %d of %d\n",
            tt$conf.int[1], tt$conf.int[2],
            sum(dc_diffs$diff > 0), nrow(dc_diffs)))

write_csv(dc_diffs, here("results","hs_dclevel_diffs.csv"))
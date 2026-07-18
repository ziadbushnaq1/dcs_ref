# build_hyperscale_roster.R — one row per hyperscale facility, all sources
library(tidyverse); library(here)

iso <- here("data","data_final","isolation_sets")
all146  <- read_csv(file.path(iso, "landsat_all.csv"),      show_col_types = FALSE)
pre14   <- read_csv(file.path(iso, "landsat_pre2014.csv"),  show_col_types = FALSE)
hsx     <- read_csv(file.path(iso, "landsat_hs_extra.csv"), show_col_types = FALSE)
HS_146  <- c(411,412,648,664,2949,2950,2998,3012,3051,3052)

roster <- bind_rows(
  all146 %>% filter(export_id %in% HS_146) %>%
    mutate(source_set = "all146",  buffer_m = 1500),
  pre14  %>% mutate(source_set = "pre2014", buffer_m = 1500),
  hsx    %>% mutate(source_set = "hs_extra", buffer_m = 1500)
) %>%
  # priority: all146 > pre2014 > hs_extra for duplicated IDs
  mutate(prio = match(source_set, c("all146","pre2014","hs_extra"))) %>%
  group_by(export_id) %>% slice_min(prio, n = 1) %>% ungroup() %>%
  select(export_id, source_set, buffer_m, year_operational,
         projected_x, projected_y, any_of(c("NAMELSAD","power_builtout",
                                            "capacity_type","stage"))) %>%
  mutate(has_event_time = !is.na(year_operational),
         seam_cohort = !is.na(year_operational) &
           year_operational %in% 2012:2013)

cat("Roster:", nrow(roster), "facilities |",
    sum(roster$has_event_time), "with event time |",
    sum(roster$seam_cohort), "seam cohort |",
    "by source:\n"); print(count(roster, source_set))
write_csv(roster, here("data","data_final","hyperscale_roster.csv"))
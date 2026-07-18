# check_lst_variation.R — is there real within-ring, within-scene LST variation?
library(tidyverse); library(here); library(duckdb)

f1 <- here("data","processed","landsat_all146","landsat_all146_obs30m_unified.csv")
HS <- read_csv(here("data","data_final","hyperscale_roster.csv"), show_col_types=FALSE) %>% filter(has_event_time) %>% pull(export_id)

con <- dbConnect(duckdb()); dbExecute(con, "SET memory_limit='24GB'")

# 1. Within-scene, within-DC pixel SD — the "is there variation" question.
#    If median within-scene SD is near the ~0.003K quantization, concern is real.
wi <- dbGetQuery(con, glue::glue("
  SELECT export_id, sensor, date_yyyymmdd,
         STDDEV(LST_Celsius) AS sd_px,
         COUNT(*) AS n_px,
         MAX(LST_Celsius) - MIN(LST_Celsius) AS range_px
  FROM read_csv('{f1}', ignore_errors=true)
  WHERE export_id IN ({paste(HS, collapse=',')}) AND LST_Celsius IS NOT NULL
  GROUP BY 1,2,3"))
cat('Within-scene pixel SD by sensor:\n')
wi %>% 
  filter(n_px > 1) %>% 
  group_by(sensor) %>%
  summarise(
    median_sd = median(sd_px, na.rm = TRUE), 
    p10 = quantile(sd_px, 0.1, na.rm = TRUE),
    p90 = quantile(sd_px, 0.9, na.rm = TRUE), 
    median_range = median(range_px, na.rm = TRUE)
  ) %>% 
  print()

# 2. Distinct-value density — detects L5 resampling duplication directly:
#    distinct values per 16 pixels ~1 means pure block resampling.
dv <- dbGetQuery(con, glue::glue("
  SELECT sensor, COUNT(*) AS n, COUNT(DISTINCT ROUND(LST_Celsius, 3)) AS n_distinct
  FROM read_csv('{f1}', ignore_errors=true)
  WHERE export_id = {HS[1]} AND LST_Celsius IS NOT NULL
  GROUP BY sensor"))
print(dv %>% mutate(distinct_ratio = n_distinct / n))

# 3. Variance decomposition of the model's actual outcome scale:
#    how much of relative-LST variance is between-pixel (identifying, absorbed
#    into pixel FE) vs within-pixel-over-time (what identifies treated_post)?
dbDisconnect(con)
library(knitr)
library(tidyverse)
library(sf)
library(tigris)
library(tidygeocoder)
library(here)

raw_dat <- read.csv(here('data', 'data_raw', 'raw_datacenter.csv'))

dat_complete <- raw_dat %>% filter(!is.na(latitude) & !is.na(longitude))
dat_missing <- raw_dat %>% filter(is.na(latitude) | is.na(longitude))

dat_fixed <- dat_missing %>%
  mutate(full_address = paste(address, city, state, country, sep = ", ")) %>%
  geocode(address = full_address, method = 'osm') %>%
  mutate(latitude = lat, longitude = long) %>%
  select(-full_address, -lat, -long)

raw_dat_geocoded <- bind_rows(dat_complete, dat_fixed) %>% drop_na(latitude, longitude)

data_centers <- raw_dat_geocoded %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
dc_projected <- st_transform(data_centers, crs = 5070)

dist_matrix <- st_distance(dc_projected)
diag(dist_matrix) <- NA
dc_projected$dist_to_nearest_dc_m <- as.numeric(apply(dist_matrix, 1, min, na.rm = TRUE))

us_counties <- counties(year = 2025)
us_counties <- st_transform(us_counties, st_crs(dc_projected)) %>% select(NAMELSAD)
dc_projected <- st_join(dc_projected, us_counties, join = st_within)

data_centers_01 <- dc_projected %>% mutate(export_id = row_number())

# Extract projected coordinates and drop the geometry list-column
data_centers_01_export <- data_centers_01 %>%
  mutate(
    projected_x = st_coordinates(.)[,1],
    projected_y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

write.csv(data_centers_01_export, here("data", "data_final", "clean01_datacenter.csv"), row.names = FALSE)
library(tidyverse); library(sf); library(here)

inv <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                show_col_types = FALSE)
analysis_ids <- read_csv(here("data","data_final","isolation_sets","landsat_all.csv"),
                         show_col_types = FALSE)$export_id

# recover lon/lat if only projected coords are present
if (!all(c("longitude","latitude") %in% names(inv))) {
  ll <- inv %>% st_as_sf(coords = c("projected_x","projected_y"), crs = 5070) %>%
    st_transform(4326) %>% st_coordinates()
  inv$longitude <- ll[,1]; inv$latitude <- ll[,2]
}

dc_inventory <- inv %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  transmute(export_id, capacity_type, stage, year_operational,
            longitude, latitude,
            in_analysis = export_id %in% analysis_ids)

saveRDS(dc_inventory, here("ShinyApp","report","dc_inventory.rds"))
cat("Wrote", nrow(dc_inventory), "facilities |",
    sum(dc_inventory$in_analysis), "in analysis\n")
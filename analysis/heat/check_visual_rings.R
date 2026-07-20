library(tidyverse)
library(sf)
library(leaflet)
library(here)

# 1. Load Hyperscale (Target) Facilities
roster <- read_csv(here("data","data_final","hyperscale_roster.csv"),
                   show_col_types = FALSE) %>%
  filter(has_event_time)

hyperscale_facilities <- roster %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

# 2. Load ALL Operational Facilities (Background)
master_ops <- read_csv(here("data","data_final","clean01_datacenter.csv"),
                       show_col_types = FALSE) %>%
  filter(stage %in% c("Operational", "Under Construction")) %>%
  st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)

# 3. Create the spatial buffers for ONLY the hyperscale facilities
ring_300  <- st_buffer(hyperscale_facilities, dist = 300)
ring_600  <- st_buffer(hyperscale_facilities, dist = 600)
ring_1000 <- st_buffer(hyperscale_facilities, dist = 1000)
ring_1500 <- st_buffer(hyperscale_facilities, dist = 1500)

# 4. Transform EVERYTHING to WGS84 (EPSG:4326) which leaflet requires
facilities_wgs <- st_transform(hyperscale_facilities, 4326)
all_ops_wgs    <- st_transform(master_ops, 4326) # Background layer
r300_wgs       <- st_transform(ring_300, 4326)
r600_wgs       <- st_transform(ring_600, 4326)
r1000_wgs      <- st_transform(ring_1000, 4326)
r1500_wgs      <- st_transform(ring_1500, 4326)

# 5. Render the interactive satellite map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # Draw the rings from largest to smallest so they stack correctly
  addPolygons(data = r1500_wgs, color = "blue", weight = 1, 
              fillOpacity = 0.1, group = "1500m (Control)") %>%
  
  addPolygons(data = r1000_wgs, color = "green", weight = 1, 
              fillOpacity = 0.1, group = "1000m (Control)") %>%
  
  addPolygons(data = r600_wgs, color = "orange", weight = 2, 
              fillOpacity = 0.1, group = "600m (Halo)") %>%
  
  addPolygons(data = r300_wgs, color = "red", weight = 2, 
              fillOpacity = 0.2, group = "300m (Core)") %>%
  
  # NEW: Add all data centers as background points (Magenta)
  # (Removed campus_id to prevent the eval error)
  addCircleMarkers(data = all_ops_wgs, color = "magenta",
                   radius = 4, fillOpacity = 0.8, weight = 1,
                   group = "All Data Centers",
                   popup = ~paste0("<b>ID: ", export_id, "</b>",
                                   "<br>Opened: ", year_operational,
                                   "<br>Type: ", capacity_type)) %>%
  
  # Add hyperscale markers on top (White)
  addCircleMarkers(data = facilities_wgs, color = "white",
                   radius = 6, fillOpacity = 1, weight = 2,
                   group = "Hyperscale (Target)",
                   popup = ~paste0("<b>HYPERSCALE ID: ", export_id, "</b>",
                                   "<br>Opened: ", year_operational,
                                   "<br>Source: ", source_set)) %>%
  
  # Add a toggle menu to turn specific layers on and off
  addLayersControl(overlayGroups = c("300m (Core)", "600m (Halo)", 
                                     "1000m (Control)", "1500m (Control)",
                                     "All Data Centers", "Hyperscale (Target)"),
                   options = layersControlOptions(collapsed = FALSE))
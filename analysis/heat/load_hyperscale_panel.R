# load_hyperscale_panel.R — shared loader: stacked L5+L8/9 hyperscale panel
# Defines: pixel_data, dc_points, master_ops, HS_ALL
load_hyperscale_panel <- function() {
  roster <- readr::read_csv(here("data","data_final","hyperscale_roster.csv"),
                            show_col_types = FALSE) %>%
    dplyr::filter(has_event_time)
  
  ids <- split(roster$export_id, roster$source_set)
  f <- list(
    all146   = here("data","processed","landsat_all146","landsat_all146_obs30m_unified.csv"),
    pre2014  = here("data","processed","landsat_pre2014","landsat_pre2014_obs30m_unified.csv"),
    hs_extra = here("data","processed","landsat_hs_extra","landsat_hs_extra_obs30m_l89.csv"))
  
  cols <- "export_id, longitude, latitude, year, month, date_yyyymmdd,
           LST_Celsius, Emissivity, ST_uncertainty, Elevation,
           scene_cloud_cover, sensor"
  
  arms <- purrr::imap_chr(ids, function(id_vec, src) {
    if (length(id_vec) == 0) return(NA_character_)
    glue::glue("SELECT {cols}
                FROM read_csv('{f[[src]]}', ignore_errors=true, union_by_name=true)
                WHERE export_id IN ({paste(id_vec, collapse=',')})
                  AND scene_cloud_cover < 30")
  }) %>% purrr::discard(is.na) %>% paste(collapse = "\nUNION ALL BY NAME\n")
  
  con <- dbConnect(duckdb::duckdb()); DBI::dbExecute(con, "SET memory_limit='24GB'")
  pixel_data <- DBI::dbGetQuery(con, glue::glue("
    WITH src AS ({arms}),
    counts AS (
      SELECT export_id, year, month, date_yyyymmdd,
             COUNT(*) FILTER (WHERE LST_Celsius IS NOT NULL) n_valid
      FROM src GROUP BY 1,2,3,4),
    best AS (
      SELECT export_id, year, month, date_yyyymmdd FROM (
        SELECT *, ROW_NUMBER() OVER (PARTITION BY export_id, year, month
                ORDER BY n_valid DESC, date_yyyymmdd ASC) rk FROM counts) WHERE rk=1)
    SELECT src.* FROM src JOIN best USING (export_id, year, month, date_yyyymmdd)"))
  DBI::dbDisconnect(con)
  
  
  cat("Loader: cloud filter <30 | rows:",
      format(nrow(pixel_data), big.mark = ","), "\n")
  print(dplyr::count(pixel_data, sensor))
  # audit line — panel composition should match the roster exactly
  got <- sort(unique(pixel_data$export_id))
  missing <- setdiff(roster$export_id, got)
  cat("Panel facilities:", length(got), "of", nrow(roster), "expected.",
      if (length(missing)) paste("MISSING:", paste(missing, collapse=", ")) else "", "\n")
  
  dc_points <- roster %>%
    sf::st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
  
  master_ops <- readr::read_csv(here("data","data_final","clean01_datacenter.csv"),
                                show_col_types = FALSE) %>%
    dplyr::filter(stage == "Operational") %>%
    sf::st_as_sf(coords = c("projected_x","projected_y"), crs = 5070)
  
  list(pixel_data = pixel_data, dc_points = dc_points,
       master_ops = master_ops, roster = roster)
}
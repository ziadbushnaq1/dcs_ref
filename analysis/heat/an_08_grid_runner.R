# an_08_grid_runner.R — shared ring-grid engine for all an_08 scripts.
# Scripts build a params tibble, load data, and call run_ring_grid().
# Flag vocabulary (identical across samples):
#   grp           which analysis roster (names of `groups` list)
#   ring_idx      index into spatial_rings
#   intens        FALSE = binary treated_post; TRUE = n_treating dose
#   use_construction  add construction_period dummy for years -3..-1
#   mod_fe        fixest FE spec string
#   cluster_var   "export_id" | "campus_id"
#   treat_scope   dose roster: "all" | "hs_only"   (intensity rows only)
#   contam_scope  control-drop roster: "all" | "hs_only"   (intensity rows only)
#   contam_timing "static" (ever near, dated+undated) |
#                 "static_dated" (ever near dated only) |
#                 "dynamic" (per-year from first dated opening - buffer)
#   contam_buffer years before opening to start dynamic drop (0 or 3)

normalize_params <- function(params, groups) {
  defaults <- tibble::tibble(
    grp = names(groups)[1], cluster_var = "export_id",
    treat_scope = "all", contam_scope = "all",
    contam_timing = "static", contam_buffer = 0L)
  for (col in names(defaults))
    if (!col %in% names(params)) params[[col]] <- defaults[[col]][1]
  params
}

print_grid_manifest <- function(params, spatial_rings, constants) {
  cat("\n==================== GRID MANIFEST ====================\n")
  cat("Constants:\n")
  for (nm in names(constants)) cat(sprintf("  %-12s %s\n", nm, constants[[nm]]))
  cat("Rings:\n")
  for (i in seq_along(spatial_rings))
    cat(sprintf("  [%d] %d-%dm\n", i, spatial_rings[[i]][1], spatial_rings[[i]][2]))
  cat("\nParameter rows (", nrow(params), " models ):\n", sep = "")
  print(knitr::kable(
    params %>% mutate(ring = map_chr(ring_idx, ~sprintf("%d-%dm",
                                                        spatial_rings[[.x]][1], spatial_rings[[.x]][2]))) %>%
      select(grp, ring, everything(), -ring_idx)))
  cat("=======================================================\n\n")
}

run_ring_grid <- function(params, pixel_data, groups, master_sets,
                          spatial_rings, ctrl_min, ctrl_max, ref_year,
                          out_csv, sensor = "landsat_monthly") {
  params <- normalize_params(params, groups)
  print_grid_manifest(params, spatial_rings, list(
    CTRL_MIN = ctrl_min, CTRL_MAX = ctrl_max, REF_YEAR = ref_year,
    PANEL_END = max(pixel_data$year), N_ROWS = nrow(pixel_data),
    OUT_CSV = out_csv))
  
  out <- list()
  for (i in seq_len(nrow(params))) {
    p    <- params[i, ]
    ring <- spatial_rings[[p$ring_idx]]
    cat(sprintf(
      "[%d/%d] %s | %d-%dm | intens=%s constr=%s | FE=%s | cl=%s | dose=%s drop=%s/%s(b%d)\n",
      i, nrow(params), p$grp, ring[1], ring[2], p$intens, p$use_construction,
      p$mod_fe, p$cluster_var, p$treat_scope, p$contam_scope,
      p$contam_timing, p$contam_buffer))
    
    res <- tryCatch(
      run_pixel_analysis(
        raw_csv_data = pixel_data, dc_points = groups[[p$grp]],
        min_treat_m = ring[1], max_treat_m = ring[2],
        min_control_m = ctrl_min, max_control_m = ctrl_max,
        fe_spec = p$mod_fe, use_power_builtout = FALSE,
        filter_elev = FALSE, elev_threshold = 50, is_event_study = FALSE,
        ref_year = ref_year, use_intensity = p$intens,
        master_dcs = if (p$intens) master_sets[[p$treat_scope]] else NULL,
        sensor = sensor, use_construction = p$use_construction,
        cluster_var = p$cluster_var,
        contamination_master = if (p$intens) master_sets[[p$contam_scope]] else NULL,
        contam_timing = p$contam_timing,
        contam_buffer_years = p$contam_buffer),
      error = function(e) { message(" -> SKIP: ", conditionMessage(e)); NULL })
    if (is.null(res)) next
    
    ct <- summary(res$model)$coeftable
    for (tm in intersect(c("treated_post","construction_period"), rownames(ct))) {
      out[[length(out)+1]] <- tibble(
        group = p$grp, ring_min = ring[1], ring_max = ring[2],
        ring_label = paste0(ring[1],"-",ring[2],"m"),
        intensity = p$intens, use_construction = p$use_construction,
        fe_spec = p$mod_fe, cluster_var = p$cluster_var,
        treat_scope   = if (p$intens) p$treat_scope   else NA_character_,
        contam_scope  = if (p$intens) p$contam_scope  else NA_character_,
        contam_timing = if (p$intens) p$contam_timing else NA_character_,
        contam_buffer = if (p$intens) p$contam_buffer else NA_integer_,
        ref_year = ref_year, term = tm,
        estimate = ct[tm,"Estimate"], se = ct[tm,"Std. Error"],
        p_value = ct[tm,"Pr(>|t|)"], n_obs = res$model$nobs
      ) %>% bind_cols(res$counts)
    }
    rm(res, ct); gc()
  }
  
  # Drop exact duplicate runs (safety net)
  results <- bind_rows(out) %>% distinct()
  
  spec_keys <- c("group","ring_min","ring_max","ring_label","intensity",
                 "use_construction","fe_spec","treat_scope","contam_scope",
                 "contam_timing","contam_buffer","term")
  
  base <- results %>%
    filter(cluster_var == "export_id") %>%
    select(all_of(spec_keys), n_obs, n_treat_dcs,
           estimate, se_facility = se, p_facility = p_value)
  
  unclust <- results %>%
    filter(cluster_var == "none") %>%
    select(all_of(spec_keys), se_none = se, p_none = p_value)
  
  campus <- results %>%
    filter(cluster_var == "campus_id") %>%
    select(all_of(spec_keys), se_campus = se, p_campus = p_value)
  
  results_wide <- base %>%
    left_join(unclust, by = spec_keys) %>%
    left_join(campus,  by = spec_keys) %>%
    pivot_wider(
      id_cols = setdiff(spec_keys, "term"),
      names_from = term,
      values_from = c(estimate, se_facility, p_facility,
                      se_none, p_none, se_campus, p_campus),
      names_glue = "{term}: {.value}") %>%
    arrange(fe_spec, contam_scope, contam_timing)
  
  print(knitr::kable(results_wide, digits = 3))
  
  cat("\n==== Compact ====\n")
  print(knitr::kable(results %>%
                       filter(term == "treated_post") %>%
                       select(fe_spec, contam_scope, contam_timing, contam_buffer,
                              cluster_var, estimate, se, p_value, n_treat_dcs),
                     digits = 3))
  
  # Save the raw/long format to CSV so make_shiny_heat_tables.R still works
  write_csv(results, out_csv)
  cat("Saved:", out_csv, "\n")
  invisible(results)
}
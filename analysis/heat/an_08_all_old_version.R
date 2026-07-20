  constr_flag <- params$use_construction[i]
  mod_fe <- params$mod_fe[i]
  cat(sprintf("[%d/%d] Group: %s | Ring: %d-%dm | Intens: %s | Construction: %s | FE: %s\n",
              i, nrow(params), grp, ring[1], ring[2], intens, constr_flag, mod_fe))
  
  res <- tryCatch(
    run_pixel_analysis(
      raw_csv_data = pixel_data, dc_points = GROUPS[[grp]],
      min_treat_m = ring[1], max_treat_m = ring[2],
      min_control_m = CTRL_MIN, max_control_m = CTRL_MAX,
      fe_spec = mod_fe, use_power_builtout = FALSE,
      filter_elev = FALSE, elev_threshold = 50, is_event_study = FALSE,
      ref_year = REF_YEAR, use_intensity = intens,
      master_dcs = if (intens) master_ops else NULL,
      sensor = "landsat_monthly",
      use_construction = constr_flag),
    error = function(e) { message(" -> SKIP: ", conditionMessage(e)); NULL })
  if (is.null(res)) next
  
  ct <- summary(res$model)$coeftable
  counts_df <- res$counts
  
  for (tm in intersect(c("treated_post","construction_period"), rownames(ct))) {
    out[[length(out)+1]] <- tibble(
      group = grp, 
      ring_min = ring[1],
      ring_max = ring[2],
      ring_label = paste0(ring[1],"-",ring[2],"m"),
      intensity = intens,
      use_construction = constr_flag,
      fe_spec = mod_fe, 
      ref_year = REF_YEAR,
      term = tm,
      estimate = ct[tm,"Estimate"], 
      se = ct[tm,"Std. Error"],
      p_value = ct[tm,"Pr(>|t|)"], 
      n_obs = res$model$nobs
    ) %>% bind_cols(counts_df) 
  }
  rm(res, ct); gc()
}

results <- bind_rows(out)

cat("\n\n==== Results: Baseline Models (use_construction = FALSE) ====\n")
print(knitr::kable(
  results %>% 
    filter(use_construction == FALSE) %>% 
    arrange(group, fe_spec, ref_year, intensity, ring_min, ring_max, desc(term)), 
  digits = 3
))

cat("\n\n==== Results: Construction Models (use_construction = TRUE) ====\n")
print(knitr::kable(
  results %>% 
    filter(use_construction == TRUE) %>% 
    arrange(group, fe_spec, ref_year, intensity, ring_min, ring_max, desc(term)), 
  digits = 3
))

write_csv(results, here("results","all145_30m_ring_grid.csv"))
cat("==== Done ====\n")
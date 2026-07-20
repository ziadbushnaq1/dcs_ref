      use_power_builtout = FALSE,
      filter_elev = FALSE,          # Hardcoded OFF (no impact)
      elev_threshold = 50, 
      is_event_study = FALSE,
      ref_year = REF_YEAR,
      use_intensity = intens, 
      master_dcs = if (intens) master_ops else NULL,
      sensor = "landsat_monthly",
      use_construction = constr_flag
    )
  }, error = function(e) { 
    message(" -> SKIP: ", conditionMessage(e))
    return(NULL) 
  })
  
  if (is.null(res)) next
  
  ct <- summary(res$model)$coeftable
  counts_df <- res$counts
  
  for (tm in intersect(c("treated_post", "construction_period"), rownames(ct))) {
    out[[length(out) + 1]] <- tibble(
      ring_min = ring[1],
      ring_max = ring[2],
      ring_label = paste0(ring[1], "-", ring[2], "m"),
      intensity = intens, 
      use_construction = constr_flag, 
      ref_year = REF_YEAR,
      fe_spec = mod_fe,
      term = tm,
      estimate = ct[tm, "Estimate"], 
      se = ct[tm, "Std. Error"],
      p_value = ct[tm, "Pr(>|t|)"], 
      n_obs = res$model$nobs
    ) %>% bind_cols(counts_df)
  }
  
  # CRITICAL MEMORY MANAGEMENT
  rm(res, ct)
  gc()
}

results <- bind_rows(out)

cat("\n\n==== Results: Baseline Models (use_construction = FALSE) ====\n")
print(knitr::kable(
  results %>% 
    filter(use_construction == FALSE) %>% 
    arrange(fe_spec, ref_year, intensity, ring_min, ring_max, desc(term)), 
  digits = 3
))

cat("\n\n==== Results: Construction Models (use_construction = TRUE) ====\n")
print(knitr::kable(
  results %>% 
    filter(use_construction == TRUE) %>% 
    arrange(fe_spec, ref_year, intensity, ring_min, ring_max, desc(term)), 
  digits = 3
))

write_csv(results, here("results", "hyperscale_30m_full_expanded.csv"))
cat("\n==== Done ====\n")
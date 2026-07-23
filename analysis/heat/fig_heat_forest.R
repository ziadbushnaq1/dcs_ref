# fig_robustness_forest.R
library(tidyverse)
library(here)

options(bitmapType = "cairo")

# 1. Load the results
cat("Loading all_robust_headline.csv...\n")
raw_res <- read_csv(here("results", "all_robust_headline.csv"), show_col_types = FALSE)

# 2. Filter for Operational effect, drop unclustered models, and create clean labels
plot_data <- raw_res %>%
  filter(term == "treated_post", cluster_var != "none") %>%
  mutate(
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    
    # Translate model parameters into plain-English poster labels
    model_label = case_when(
      # Add contam_buffer == 0 to isolate the true anchor
      fe_spec == "pixel_id + year" & cluster_var == "export_id" & contam_timing == "dynamic" & contam_scope == "all_oc" & contam_buffer == 0 ~ "Main Model Specification",
      
      contam_timing == "static" & cluster_var == "export_id" ~ "Static Control Exclusion",
      contam_buffer == 3 ~ "Dynamic Exclusion (3-year buffer)",
      fe_spec == "pixel_id + year^export_id" ~ "Facility × Year Fixed Effects",
      fe_spec == "pixel_id + year^month" ~ "Year × Month Fixed Effects",
      contam_scope == "all" ~ "Exclude Operational DCs Only",
      contam_scope == "hs_only" ~ "Exclude Hyperscale DCs Only",
      TRUE ~ paste("Other:", fe_spec)
    ),
    
    # Flag the main model so we can color it differently on the poster
    is_main = ifelse(model_label == "Main Model Specification", "Main", "Robustness Check")
  )

# 3. Order the y-axis so the Main Model is at the very top, and the rest are sorted by effect size
plot_data <- plot_data %>%
  arrange(is_main == "Main", estimate) %>%
  mutate(model_label = fct_inorder(model_label))

# 4. Generate the plot
cat("Rendering forest plot...\n")
p <- ggplot(plot_data, aes(x = estimate, y = model_label, color = is_main)) +
  # Add the zero-effect reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 1) +
  
  # Add the confidence interval bars
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, linewidth = 1.2) +
  
  # Add the point estimates
  geom_point(size = 4.5) +
  
  scale_color_manual(values = c("Main" = "#d7191c", "Robustness Check" = "#4C78A8")) +
  
  
  
  # Set custom x-axis ticks every 0.1 degrees Celsius based on data range
  scale_x_continuous(
    breaks = seq(
      floor(min(plot_data$ci_low) * 10) / 10, 
      ceiling(max(plot_data$ci_high) * 10) / 10, 
      by = 0.1
    )
  )  +
  
  # Clean up labels featuring ring specifications in the subtitle
  labs(
    title = str_wrap("Land Surface Temperature Effect", width = 42),
    subtitle = str_wrap("Operational effect (\u00b0C per facility) comparing 0-600m treatment rings to 1000-1500m control zones across alternative specifications. Error bars show 95% CI.", width = 100),
    x = "Estimated Temperature Effect (\u00b0C)",
    y = NULL 
  ) +
  
  # Poster-friendly theming
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    
    # Center the title/subtitle over the ENTIRE plot area, not just the panel
    plot.title.position = "plot",
    
    # Give the outer edges breathing room so text doesn't clip
    plot.margin = margin(t = 25, r = 25, b = 10, l = 20, unit = "pt"),
    
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 5), color = "gray30"),
    
    # Matched "Main Model Specification" here so the y-axis text bolds correctly
    axis.text.y = element_text(face = ifelse(plot_data$model_label == "Main Model Specification", "bold", "plain"), color = "black"),
    
    axis.text.x = element_text(color = "black"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )

# 5. Save the output
out_file <- here("figures", "heat_forest_plot.png")
ggsave(out_file, p, width = 11, height = 7, dpi = 300, bg = "white")
cat("Success! Saved forest plot to", out_file, "\n")
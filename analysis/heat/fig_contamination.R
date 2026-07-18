# fig_contamination.R — Visualizing Spatial Contamination & Overlap
library(tidyverse)
library(here)
options(bitmapType = "cairo")

# --- 1. Load the Evaluation Data ---
df <- read_csv(here("results", "facility_contamination_ratios.csv"), show_col_types = FALSE)

# ==============================================================================
# PLOT 1: TREATMENT ZONE OVERLAP (STACKED BAR CHART)
# ==============================================================================
treat_data <- df %>%
  mutate(
    clean_treat_px = drop_treat_px,
    multi_treated_px = additive_treat_px - drop_treat_px 
  ) %>%
  group_by(ring) %>%
  summarise(
    Clean_Treat = sum(clean_treat_px),
    Multi_Treated = sum(multi_treated_px),
    .groups = "drop"
  ) %>%
  mutate(ring = factor(ring, levels = c("0_to_300", "300_to_600", "0_to_600"), 
                       labels = c("0-300m Core", "300-600m Halo", "0-600m Campus"))) %>%
  pivot_longer(cols = c(Clean_Treat, Multi_Treated), names_to = "Category", values_to = "Pixels")

p_treat <- ggplot(treat_data, aes(x = ring, y = Pixels, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  scale_fill_manual(values = c("Clean_Treat" = "#2c3e50", "Multi_Treated" = "#e74c3c"),
                    labels = c("Clean (Intensity = 1)", "Overlapping (Intensity > 1)")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Treatment Zone Overlap",
       subtitle = "Pixels experiencing multi-facility heat intensity",
       x = "Spatial Ring", y = "Total Pixels", fill = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"))

ggsave(here("results", "fig_treatment_overlap.png"), plot = p_treat, width = 8, height = 6, dpi = 300)
cat("Treatment bar chart saved to results/fig_treatment_overlap.png\n")


# ==============================================================================
# PLOT 2: CONTROL HYGIENE (PIE CHART)
# ==============================================================================
# We filter to just ONE ring (0_to_600) because the 1000m-1500m control boundary 
# is identical for all tests. Summing them all would triple-count the data.
control_data <- df %>%
  filter(ring == "0_to_600") %>%
  summarise(
    Clean_Control = sum(clean_control_px),
    Contaminated_Control = sum(raw_control_px - clean_control_px)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Pixels") %>%
  mutate(
    Fraction = Pixels / sum(Pixels),
    # Create a nice label with the raw count and the percentage
    Label = paste0(scales::comma(Pixels), "\n(", scales::percent(Fraction, accuracy = 1), ")")
  )

p_control <- ggplot(control_data, aes(x = "", y = Pixels, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Clean_Control" = "#27ae60", "Contaminated_Control" = "#f39c12"),
                    labels = c("Clean Control (Kept)", "Contaminated (Dropped)")) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  labs(title = "Control Contamination Tracking",
       subtitle = "Contaminated pixels in the 1000-1500m control rings",
       fill = "") +
  theme_void(base_size = 14) +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(here("results", "fig_control_hygiene_pie.png"), plot = p_control, width = 8, height = 6, dpi = 300)
cat("Control pie chart saved to results/fig_control_hygiene_pie.png\n")
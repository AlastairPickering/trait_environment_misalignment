rm(list = ls())

library(dggridR)
library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(maps)
library(cowplot)
library(scales) 

# Construct the hex grid
hex_grid <- dgconstruct(res = 7)

# Read raw bootstrap runs data and filter
raw_data <- read_csv("data/precomputed/predicted_all_runs_ssp370.csv") %>%
  filter(lon > -175, lon < -50, lat > 20, lat < 70)

# Assign each point to a hex cell
raw_data$grid_id <- dgGEO_to_SEQNUM(hex_grid, raw_data$lon, raw_data$lat)$seqnum

# Compute mean TEM per hex cell per bootstrap run
hex_by_run <- raw_data %>%
  group_by(grid_id, bootstrap_run) %>%
  summarise(TEM = mean(traits_oor_SD3, na.rm = TRUE), .groups = "drop")

# Save intermediate data
write_csv(hex_by_run, "output/ssp370_hexbin_by_run.csv")

# Aggregate across all runs for summary stats
hex_stats <- hex_by_run %>%
  group_by(grid_id) %>%
  summarise(
    TEM_mean = mean(TEM, na.rm = TRUE),
    TEM_std  = sd(TEM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_TEM_mean = TEM_mean,
    TEM_cv       = ifelse(TEM_mean == 0, 0, TEM_std / TEM_mean)
  )

# Save aggregated stats
write_csv(hex_stats, "output/ssp370_hexbin_stats.csv")

# Create polygon grid for plotting
poly_grid <- dgcellstogrid(hex_grid, hex_stats$grid_id)
poly_grid <- merge(poly_grid, hex_stats, by.x = "seqnum", by.y = "grid_id")

countries <- map_data("world") %>%
  filter(long > -175, long < -50, lat > 20, lat < 70)

# Function to create a standardised plot, with optional custom breaks/labels
create_plot <- function(poly_data, metric_col, metric_label, plot_title, use_custom_breaks = FALSE) {
  ggplot() +
    geom_sf(data = poly_data, aes(fill = .data[[metric_col]]), colour = NA) +
    geom_polygon(
      data = countries, aes(x = long, y = lat, group = group),
      fill = NA, colour = "black", linewidth = 0.5
    ) +
    scale_fill_viridis_c(
      option    = "C",
      direction = 1,
      name      = metric_label,
      na.value  = "grey50",
      breaks    = if (use_custom_breaks) seq(0, 100, by = 25) else waiver(),
      labels    = if (use_custom_breaks) label_number(suffix = "", accuracy = 1) else waiver()
    ) +
    coord_sf(xlim = c(-175, -50), ylim = c(20, 70)) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid       = element_blank(),
      plot.margin      = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      plot.title       = element_text(size = 12, face = "bold", margin = ggplot2::margin(t = 10, b = 10)),
      legend.title     = element_text(size = 12),
      legend.text      = element_text(size = 12),
      axis.title       = element_text(size = 12),
      axis.text        = element_text(size = 12)
    ) +
    ggtitle("")
}

#  TEM map
plot_TEM <- create_plot(
  poly_grid,
  "pct_TEM_mean",
  "TEM traits (Maximum 24 traits)",
  "Spatial Projections of Trait-Environment Misalignment (TEM)",
  use_custom_breaks = FALSE
)

# CV plot
poly_grid$cv_class <- cut(
  poly_grid$TEM_cv,
  breaks = c(0, 0.2, 0.4, 0.6, 0.8, Inf),
  labels = c("<0.2", "0.2–0.4", "0.4–0.6", "0.6–0.8", "0.8+"),
  include.lowest = TRUE
)

cv_plot <- ggplot() +
  geom_sf(data = poly_grid, aes(fill = cv_class), colour = NA) +
  geom_polygon(
    data = countries, aes(x = long, y = lat, group = group),
    fill = NA, colour = "black", linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "<0.2"    = "#2166ac",
      "0.2–0.4" = "#67a9cf",
      "0.4–0.6" = "#d1e5f0",
      "0.6–0.8" = "#fddbc7",
      "0.8+"    = "#b2182b"
    ),
    name = "Sampling Uncertainty"
  ) +
  coord_sf(xlim = c(-175, -50), ylim = c(20, 70)) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid       = element_blank(),
    plot.margin      = unit(c(0.5, 0.5, 1, 0.5), "cm"),
    plot.title       = element_text(size = 12, face = "bold", margin = ggplot2::margin(t = 10, b = 10)),
    legend.title     = element_text(size = 12),
    legend.text      = element_text(size = 12),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 12)
  ) +
  ggtitle("")

# Stack both plots vertically
combined_plot <- plot_grid(
  plot_TEM,
  cv_plot,
  ncol = 1,
  align = "hv",
  rel_heights = c(1, 1)
)

# Save combined plot
ggsave(
  "output/ssp370_hexbin_mean_cv_combined_tem.png",
  plot = combined_plot,
  dpi = 600,
  width = 12,
  height = 16,
  units = "in",
  limitsize = FALSE
)

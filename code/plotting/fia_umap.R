# Load required libraries
library(tidyverse)
library(ggplot2)
library(uwot)
library(patchwork)
library(maps)

# Read dataset and subset to North America, remove 'Other' groups
plot_data <- read_csv("data/precomputed/plot_and_abiotic_data_current.csv") %>%
  filter(lon > -175, lon < -50, lat > 20, lat < 70) %>%
  filter(!str_starts(FIA_group, "Other"))

# Remove FIA groups with fewer than 30 plots
group_counts  <- plot_data %>% count(FIA_group)
keep_groups   <- group_counts %>% filter(n >= 30) %>% pull(FIA_group)
plot_data     <- plot_data %>% filter(FIA_group %in% keep_groups)

# Manually define a distinct palette for each FIA_group
groups        <- sort(unique(plot_data$FIA_group))
manual_colors <- c(
  "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF",
  "#00FFFF", "#800000", "#808000", "#008000", "#800080",
  "#008080", "#000080", "#FFA500", "#A52A2A", "#8A2BE2",
  "#DEB887", "#5F9EA0", "#D2691E", "#9ACD32", "#FF1493",
  "#4B0082", "#7FFF00", "#DC143C"
)[seq_along(groups)]
group_palette <- setNames(manual_colors, groups)

# Country boundaries
countries <- map_data("world") %>%
  filter(long > -175, long < -50, lat > 20, lat < 70)

# Define point sizes and font sizes
grid_point_size   <- 0.5
dot_legend_size   <- 5
base_font_size    <- 14
title_font_size   <- 14
legend_title_size <- 14
legend_text_size  <- 14
axis_title_size   <- 14
axis_text_size    <- 14

# Single theme for all text
custom_theme <- theme_bw(base_size = base_font_size) +
  theme(
    plot.title   = element_text(size = title_font_size, face = "bold"),
    axis.title   = element_text(size = axis_title_size),
    axis.text    = element_text(size = axis_text_size),
    legend.title = element_text(size = legend_title_size),
    legend.text  = element_text(size = legend_text_size)
  )

# Top map of plot locations 
map1 <- ggplot() +
  geom_polygon(
    data   = countries,
    aes(x = long, y = lat, group = group),
    fill   = NA, colour = "black", size = 0.5
  ) +
  geom_point(
    data        = plot_data,
    aes(x = lon, y = lat, colour = FIA_group),
    size        = grid_point_size,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = group_palette,
    guide  = "none"
  ) +
  coord_fixed(1.3) +
  labs(
    title = "FIA Group at Plot Locations",
    x     = "Longitude",
    y     = "Latitude"
  ) +
  custom_theme

# UMAP projection of normalised traits
trait_names   <- c(
  'crown_height', 'tree_height', 'stem_diameter', 'leaf_thickness',
  'crown_diameter', 'conduit_diam.', 'stomatal_conduct.', 'root_depth',
  'leaf_n', 'leaf_p', 'leaf_k', 'leaf_area', 'specific_leaf_area',
  'leaf_vcmax', 'leaf_density', 'wood_density', 'bark_thickness',
  'seed_dry_mass', 'cold', 'shade', 'drought', 'water', 'fire_tol',
  'myco_association'
)
plot_complete <- plot_data %>% drop_na(all_of(trait_names))
trait_matrix  <- plot_complete %>% select(all_of(trait_names)) %>% scale()

set.seed(42)
umap_res <- uwot::umap(
  trait_matrix,
  n_neighbors   = 5,
  min_dist      = 0.2,
  metric        = "cosine",
  n_components  = 2
)

umap_df <- as_tibble(umap_res) %>%
  rename(UMAP1 = V1, UMAP2 = V2) %>%
  mutate(FIA_group = plot_complete$FIA_group)


# Bottom map: UMAP scatter with legend

map2 <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, colour = FIA_group)) +
  geom_point(size = grid_point_size) +
  scale_colour_manual(
    values = group_palette,
    name   = "FIA Group",
    guide  = guide_legend(
      override.aes = list(size = dot_legend_size),
      ncol         = 1
    )
  ) +
  labs(
    title = "UMAP Projection of Plots by Trait (Cosine)",
    x     = "UMAP1",
    y     = "UMAP2"
  ) +
  custom_theme +
  theme(
    legend.position  = "right",
    legend.direction = "vertical",
    legend.box       = "vertical"
  )

# Combine both maps in composite image
top_row    <- map1 + plot_spacer() + plot_layout(ncol = 2, widths = c(4, 1))
bottom_row <- map2 + plot_spacer() + plot_layout(ncol = 2, widths = c(4, 1))

gcombined <- top_row / bottom_row +
  plot_layout(heights = c(1, 1))


# Save the final figure

ggsave(
  filename = "output/fia_group_and_umap_maps.png",
  plot     = gcombined,
  dpi      = 600,
  width    = 16,
  height   = 20,
  units    = "in"
)

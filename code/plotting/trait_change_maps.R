# Clear environment
rm(list = ls())

# Load required libraries
library(dggridR)
library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(maps)
library(cowplot)

# clean trait names
clean_trait_name <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}

# Define trait groups
structural_traits         <- c('crown_height', 'tree_height', 'stem_diameter', 'leaf_thickness', 'crown_diameter')
hydraulic_traits          <- c('conduit_diam.', 'stomatal_conduct.', 'root_depth')
leaf_economics_traits     <- c('leaf_n', 'leaf_p', 'leaf_k', 'leaf_area', 'specific_leaf_area', 'leaf_vcmax', 'leaf_density')
woody_traits              <- c('wood_density', 'bark_thickness')
reproductive_traits       <- c('seed_dry_mass')
abiotic_tolerances_traits <- c('cold', 'shade', 'drought', 'water', 'fire_tol', 'myco_association')

trait_groups <- list(
  "Structural"     = structural_traits,
  "Hydraulic"      = hydraulic_traits,
  "Leaf Economics" = leaf_economics_traits,
  "Woody"          = woody_traits,
  "Tolerances"     = abiotic_tolerances_traits,
  "Reproductive"   = reproductive_traits
)

# colours 
group_colours <- c(
  "Structural"     = "#1f77b4",  
  "Hydraulic"      = "#2ca02c",  
  "Leaf Economics" = "#9467bd",  
  "Woody"          = "#e377c2",  
  "Tolerances"     = "#bcbd22",  
  "Reproductive"   = "#17becf"   
)

# Construct the hex grid 
hex_grid <- dgconstruct(res = 7)

# Read the trait change data
trait_data <- read_csv("data/precomputed/trait_bootstrap_change_ssp370.csv") %>%
  filter(lon > -175, lon < -50, lat > 20, lat < 70)

# Assign each point to a hex cell
trait_data$grid_id <- dgGEO_to_SEQNUM(hex_grid, trait_data$lon, trait_data$lat)$seqnum

# Filter trait change columns
trait_cols <- names(trait_data)[grepl("_change$", names(trait_data))]

# Compute mean trait change per hexbin
hex_stats <- trait_data %>%
  group_by(grid_id) %>%
  summarise(
    across(all_of(trait_cols), mean, na.rm = TRUE),
    min_lat      = min(lat, na.rm = TRUE),
    max_lat      = max(lat, na.rm = TRUE),
    min_lon      = min(lon, na.rm = TRUE),
    max_lon      = max(lon, na.rm = TRUE),
    sample_count = n(),
    pids         = paste(unique(pid), collapse = ";"),
    .groups      = "drop"
  )

write_csv(hex_stats, "trait_change_hexbin_summary.csv")

# Create polygon grid for plotting
poly_grid <- dgcellstogrid(hex_grid, hex_stats$grid_id)
poly_grid <- merge(poly_grid, hex_stats,
                   by.x = "seqnum", by.y = "grid_id")

# Load world map data
countries <- map_data("world") %>%
  filter(long > -175, long < -50, lat > 20, lat < 70)

# Define a single common fill scale
common_scale <- scale_fill_gradient2(
  low      = "blue",
  mid      = "white",
  high     = "orange",
  midpoint = 0,
  name     = "Mean Change",
  na.value = "grey50"
)

# Function to create individual hexbin heatmaps
create_plot <- function(poly_data, trait_col) {
  # Strip off "_change" to match trait_groups
  base_trait <- sub("_change$", "", trait_col)
  
  # Find group & its colour
  idx <- which(sapply(trait_groups, function(v) base_trait %in% v))
  if (length(idx) >= 1) {
    grp          <- names(trait_groups)[idx[1]]
    group_colour <- group_colours[[grp]]
  } else {
    warning("Trait '", trait_col, "' not matched; using black.")
    group_colour <- "black"
  }
  
  ggplot() +
    geom_sf(
      data = poly_data,
      aes(fill = .data[[trait_col]]),
      color = NA
    ) +
    geom_polygon(
      data = countries,
      aes(x = long, y = lat, group = group),
      fill = NA, color = "black", linewidth = 0.5
    ) +
    # use the shared scale
    common_scale +
    coord_sf(xlim = c(-175, -50), ylim = c(20, 70)) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid       = element_blank(),
      plot.margin      = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      plot.title = element_text(
        size   = 12,
        face   = "bold",
        colour = group_colour,
        margin = ggplot2::margin(t = 10, b = 10)
      ),
      legend.position = "none",
      legend.title    = element_text(size = 10),
      legend.text     = element_text(size = 10),
      axis.title      = element_blank(),
      axis.text       = element_blank()
    ) +
    ggtitle(clean_trait_name(trait_col))
}

# Generate the plots
plots <- lapply(trait_cols, function(trait) create_plot(poly_grid, trait))
legend_plot <- ggplot() +
  geom_sf(data = poly_grid,
          aes(fill = .data[[trait_cols[1]]]),
          color = NA) +
  common_scale +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 10),
    axis.text       = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank(),
    panel.background= element_blank()
  )

shared_legend <- get_legend(legend_plot)

# Combine maps + legend
n_traits <- length(trait_cols)
n_rows   <- ceiling(n_traits / 3)

map_grid <- plot_grid(
  plotlist = plots,
  ncol     = 3,
  nrow     = n_rows,
  align    = "hv"
)

final_plot <- plot_grid(
  map_grid, shared_legend,
  ncol         = 2,
  rel_widths   = c(1, 0.2)
)

# Save the final figure
ggsave(
  "output/trait_change_hexbin_combined_24.png",
  plot      = final_plot,
  dpi       = 600,
  width     = 20,              
  height    = 4 * n_rows,
  units     = "in",
  limitsize = FALSE
)

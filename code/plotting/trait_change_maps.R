rm(list = ls())

library(dggridR)
library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(maps)
library(cowplot)
library(scales)

ssp370_predictions_file <- "predicted_traits_ssp370_all_bootstraps.csv"
current_scaled_file <- "data/precomputed/current_traits_scaled.csv"
trait_change_file <- "data/precomputed/trait_bootstrap_change_ssp370.csv"
out_dir <- "output"

dir.create(dirname(trait_change_file), showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

clean_trait_name <- function(x) {
  x %>%
    str_replace("_change$", "") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}

structural_traits <- c(
  "crown_height",
  "tree_height",
  "stem_diameter",
  "crown_diameter",
  "root_depth"
)

hydraulic_traits <- c(
  "conduit_diam.",
  "stomatal_conduct."
)

leaf_economics_traits <- c(
  "leaf_n",
  "leaf_p",
  "leaf_k",
  "leaf_area",
  "specific_leaf_area",
  "leaf_vcmax",
  "leaf_density",
  "leaf_thickness"
)

woody_traits <- c(
  "wood_density",
  "bark_thickness"
)

reproductive_traits <- c(
  "seed_dry_mass"
)

abiotic_tolerances_traits <- c(
  "cold",
  "shade",
  "drought",
  "water",
  "fire_tol",
  "myco_association"
)

trait_groups <- list(
  "Structural" = structural_traits,
  "Hydraulic" = hydraulic_traits,
  "Leaf Economics" = leaf_economics_traits,
  "Woody" = woody_traits,
  "Tolerances" = abiotic_tolerances_traits,
  "Reproductive" = reproductive_traits
)

group_colours <- c(
  "Structural" = "#1f77b4",
  "Hydraulic" = "#2ca02c",
  "Leaf Economics" = "#9467bd",
  "Woody" = "#e377c2",
  "Tolerances" = "#bcbd22",
  "Reproductive" = "#17becf"
)

ssp370_predicted_df <- read_csv(
  ssp370_predictions_file,
  show_col_types = FALSE
) %>%
  mutate(
    pid = as.character(pid),
    bootstrap_run = as.integer(bootstrap_run)
  )

current_scaled_df <- read_csv(
  current_scaled_file,
  show_col_types = FALSE
) %>%
  mutate(pid = as.character(pid))

metadata_cols <- c(
  "pid",
  "lat",
  "lon",
  "FIA_group",
  "ECO_NAME",
  "BIOME",
  "SSP"
)

trait_base_cols <- setdiff(
  names(current_scaled_df),
  metadata_cols
)

trait_base_cols <- trait_base_cols[
  trait_base_cols %in% names(ssp370_predicted_df)
]

predicted_traits_df <- ssp370_predicted_df %>%
  select(
    pid,
    bootstrap_run,
    all_of(trait_base_cols)
  ) %>%
  rename_with(
    ~ paste0(.x, "_future"),
    all_of(trait_base_cols)
  )

current_traits_df <- current_scaled_df %>%
  select(
    pid,
    lat,
    lon,
    all_of(trait_base_cols)
  ) %>%
  rename_with(
    ~ paste0(.x, "_current"),
    all_of(trait_base_cols)
  )

trait_change_df <- predicted_traits_df %>%
  inner_join(
    current_traits_df,
    by = "pid"
  )

for (trait in trait_base_cols) {
  future_col <- paste0(trait, "_future")
  current_col <- paste0(trait, "_current")
  change_col <- paste0(trait, "_change")
  
  trait_change_df[[change_col]] <- (
    trait_change_df[[future_col]] -
      trait_change_df[[current_col]]
  )
}

trait_bootstrap_change_df <- trait_change_df %>%
  select(
    pid,
    bootstrap_run,
    lon,
    lat,
    all_of(paste0(trait_base_cols, "_change"))
  )

write_csv(
  trait_bootstrap_change_df,
  trait_change_file
)

hex_grid <- dgconstruct(res = 7)

trait_data <- read_csv(
  trait_change_file,
  show_col_types = FALSE
) %>%
  mutate(pid = as.character(pid)) %>%
  filter(
    lon > -175,
    lon < -50,
    lat > 20,
    lat < 70
  )

trait_data$grid_id <- dgGEO_to_SEQNUM(
  hex_grid,
  trait_data$lon,
  trait_data$lat
)$seqnum

trait_cols <- names(trait_data)[
  grepl("_change$", names(trait_data))
]

hex_stats <- trait_data %>%
  group_by(grid_id) %>%
  summarise(
    across(
      all_of(trait_cols),
      ~ mean(.x, na.rm = TRUE)
    ),
    min_lat = min(lat, na.rm = TRUE),
    max_lat = max(lat, na.rm = TRUE),
    min_lon = min(lon, na.rm = TRUE),
    max_lon = max(lon, na.rm = TRUE),
    sample_count = n(),
    n_plots = n_distinct(pid),
    n_bootstrap_runs = n_distinct(bootstrap_run),
    pids = paste(unique(pid), collapse = ";"),
    .groups = "drop"
  )

write_csv(
  hex_stats,
  file.path(out_dir, "trait_change_hexbin_summary.csv")
)

poly_grid <- dgcellstogrid(
  hex_grid,
  hex_stats$grid_id
)

poly_grid <- merge(
  poly_grid,
  hex_stats,
  by.x = "seqnum",
  by.y = "grid_id"
)

st_crs(poly_grid) <- 4326

countries <- map_data("world") %>%
  filter(
    long > -175,
    long < -50,
    lat > 20,
    lat < 70
  )

max_abs_change <- poly_grid %>%
  st_drop_geometry() %>%
  select(all_of(trait_cols)) %>%
  as.matrix() %>%
  abs() %>%
  max(na.rm = TRUE)

common_scale <- scale_fill_gradient2(
  low = "blue",
  mid = "white",
  high = "orange",
  midpoint = 0,
  limits = c(-max_abs_change, max_abs_change),
  oob = scales::squish,
  name = "Mean Change",
  na.value = "grey50"
)

create_plot <- function(poly_data, trait_col) {
  
  base_trait <- sub("_change$", "", trait_col)
  
  idx <- which(
    sapply(
      trait_groups,
      function(v) base_trait %in% v
    )
  )
  
  if (length(idx) >= 1) {
    grp <- names(trait_groups)[idx[1]]
    group_colour <- group_colours[[grp]]
  } else {
    warning("Trait '", trait_col, "' not matched; using black.")
    group_colour <- "black"
  }
  
  ggplot() +
    geom_sf(
      data = poly_data,
      aes(fill = .data[[trait_col]]),
      colour = NA
    ) +
    geom_polygon(
      data = countries,
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "black",
      linewidth = 0.5
    ) +
    common_scale +
    coord_sf(
      xlim = c(-175, -50),
      ylim = c(20, 70),
      expand = FALSE
    ) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      plot.title = element_text(
        size = 12,
        face = "bold",
        colour = group_colour,
        margin = ggplot2::margin(t = 10, b = 10)
      ),
      legend.position = "none",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.title = element_blank(),
      axis.text = element_blank()
    ) +
    ggtitle(clean_trait_name(trait_col))
}

plots <- lapply(
  trait_cols,
  function(trait) create_plot(poly_grid, trait)
)

legend_plot <- ggplot() +
  geom_sf(
    data = poly_grid,
    aes(fill = .data[[trait_cols[1]]]),
    colour = NA
  ) +
  common_scale +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )

shared_legend <- get_legend(legend_plot)

n_traits <- length(trait_cols)
n_rows <- ceiling(n_traits / 3)

map_grid <- plot_grid(
  plotlist = plots,
  ncol = 3,
  nrow = n_rows,
  align = "hv"
)

final_plot <- plot_grid(
  map_grid,
  shared_legend,
  ncol = 2,
  rel_widths = c(1, 0.2)
)

ggsave(
  filename = file.path(out_dir, "trait_change_hexbin_combined_24.png"),
  plot = final_plot,
  dpi = 600,
  width = 20,
  height = 4 * n_rows,
  units = "in",
  limitsize = FALSE
)
library(dggridR)
library(tidyverse)
library(ggplot2)
library(sf)
library(scales)
library(maps)

ssp370_predictions_file <- "data/precomputed/predicted_traits_ssp370_all_bootstraps.csv"
current_scaled_file <- "data/precomputed/current_traits_scaled.csv"
out_dir <- "output"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

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

pid_col <- "pid"
bootstrap_col <- "bootstrap_run"
lon_col <- "lon"
lat_col <- "lat"

metadata_cols <- c(
  "pid",
  "lat",
  "lon",
  "FIA_group",
  "ECO_NAME",
  "BIOME",
  "SSP"
)

trait_cols <- setdiff(
  names(current_scaled_df),
  metadata_cols
)

trait_cols <- trait_cols[
  trait_cols %in% names(ssp370_predicted_df)
]

predicted_traits_df <- ssp370_predicted_df %>%
  select(
    all_of(c(pid_col, bootstrap_col, trait_cols))
  ) %>%
  rename_with(
    ~ paste0(.x, "_pred"),
    all_of(trait_cols)
  )

current_traits_df <- current_scaled_df %>%
  select(
    all_of(c(pid_col, lon_col, lat_col, trait_cols))
  ) %>%
  rename_with(
    ~ paste0(.x, "_current_scaled"),
    all_of(trait_cols)
  )

mahal_joined_df <- predicted_traits_df %>%
  inner_join(
    current_traits_df,
    by = pid_col
  )

pred_cols <- paste0(trait_cols, "_pred")
current_scaled_cols <- paste0(trait_cols, "_current_scaled")

mahal_complete_df <- mahal_joined_df %>%
  filter(
    if_all(
      all_of(c(pred_cols, current_scaled_cols)),
      ~ is.finite(.x)
    )
  )

current_scaled_trait_matrix <- mahal_complete_df %>%
  select(all_of(current_scaled_cols)) %>%
  as.matrix()

predicted_trait_matrix <- mahal_complete_df %>%
  select(all_of(pred_cols)) %>%
  as.matrix()

difference_matrix <- predicted_trait_matrix - current_scaled_trait_matrix

regularisation <- 1e-6

current_scaled_covariance_source <- current_scaled_df %>%
  select(all_of(trait_cols)) %>%
  filter(
    if_all(
      everything(),
      ~ is.finite(.x)
    )
  ) %>%
  as.matrix()

covariance_matrix <- cov(
  current_scaled_covariance_source,
  use = "pairwise.complete.obs"
)

covariance_matrix <- covariance_matrix +
  diag(regularisation, nrow(covariance_matrix))

inverse_covariance_matrix <- solve(covariance_matrix)

mahal_values <- sqrt(
  rowSums(
    (difference_matrix %*% inverse_covariance_matrix) * difference_matrix
  )
)

mahal_by_bootstrap_df <- mahal_complete_df %>%
  mutate(mahalanobis_distance = mahal_values) %>%
  select(
    all_of(c(pid_col, bootstrap_col, lon_col, lat_col)),
    mahalanobis_distance
  )

mahal_summary_df <- mahal_by_bootstrap_df %>%
  group_by(
    pid,
    lon,
    lat
  ) %>%
  summarise(
    mahal_mean = mean(mahalanobis_distance, na.rm = TRUE),
    mahal_sd = sd(mahalanobis_distance, na.rm = TRUE),
    mahal_cov_plot_level = if_else(
      mahal_mean == 0,
      0,
      mahal_sd / mahal_mean
    ),
    .groups = "drop"
  ) %>%
  mutate(
    mahal_cov_plot_level = if_else(
      is.finite(mahal_cov_plot_level),
      mahal_cov_plot_level,
      NA_real_
    )
  )

write_csv(
  mahal_by_bootstrap_df,
  file.path(out_dir, "mahalanobis_distance_ssp370_by_bootstrap.csv")
)

write_csv(
  mahal_summary_df,
  file.path(out_dir, "sampling_uncertainty_cov_plot_summary.csv")
)

res <- 7
xlim <- c(-175, -50)
ylim <- c(20, 70)
width <- 12
height <- 8
dpi <- 300

hex_grid <- dgconstruct(res = res)

countries <- map_data("world") %>%
  filter(
    long > xlim[1],
    long < xlim[2],
    lat > ylim[1],
    lat < ylim[2]
  )

process_hexbin <- function(input_df, hex_grid, value_col) {
  ll <- input_df %>%
    mutate(value = .data[[value_col]]) %>%
    filter(
      lon > xlim[1],
      lon < xlim[2],
      lat > ylim[1],
      lat < ylim[2],
      is.finite(lon),
      is.finite(lat),
      is.finite(value)
    )
  
  ll$grid_id <- dgGEO_to_SEQNUM(
    hex_grid,
    ll$lon,
    ll$lat
  )$seqnum
  
  hex_sum <- ll %>%
    group_by(grid_id) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  poly_grid <- dgcellstogrid(
    hex_grid,
    hex_sum$grid_id
  )
  
  poly_grid <- merge(
    poly_grid,
    hex_sum,
    by.x = "seqnum",
    by.y = "grid_id"
  )
  
  st_crs(poly_grid) <- 4326
  
  poly_grid
}

uncertainty_scale <- scale_fill_manual(
  values = c(
    "0.00–0.05" = "#FFFFFF",
    "0.05–0.10" = "#E6F4FF",
    "0.10–0.15" = "#BFE7FF",
    "0.15–0.20" = "#80CCFF",
    "0.20–0.30" = "#339DFF",
    "0.30+"     = "#0066CC"
  ),
  breaks = c(
    "0.00–0.05",
    "0.05–0.10",
    "0.10–0.15",
    "0.15–0.20",
    "0.20–0.30",
    "0.30+"
  ),
  drop = FALSE,
  na.value = "white",
  name = "Sampling uncertainty"
)

uncertainty_map_theme <- theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    plot.title = element_blank()
  )

make_uncertainty_map <- function(hex_sf, countries_df) {
  ggplot() +
    geom_sf(
      data = hex_sf,
      aes(fill = uncertainty_bin),
      colour = NA
    ) +
    geom_polygon(
      data = countries_df,
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "black",
      linewidth = 0.5
    ) +
    uncertainty_scale +
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    labs(
      x = "long",
      y = "lat",
      title = ""
    ) +
    uncertainty_map_theme
}

hex_cov <- process_hexbin(
  input_df = mahal_summary_df,
  hex_grid = hex_grid,
  value_col = "mahal_cov_plot_level"
)

hex_cov <- hex_cov %>%
  mutate(
    uncertainty_bin = cut(
      value,
      breaks = c(
        0.00,
        0.05,
        0.10,
        0.15,
        0.20,
        0.30,
        Inf
      ),
      labels = c(
        "0.00–0.05",
        "0.05–0.10",
        "0.10–0.15",
        "0.15–0.20",
        "0.20–0.30",
        "0.30+"
      ),
      include.lowest = TRUE,
      right = FALSE
    )
  )

write_csv(
  st_drop_geometry(hex_cov),
  file.path(out_dir, "sampling_uncertainty_cov_hex_summary.csv")
)

hex_cov_bin_counts <- hex_cov %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  count(uncertainty_bin, name = "n_hexes") %>%
  mutate(
    percentage = 100 * n_hexes / sum(n_hexes)
  )

hex_cov_distribution <- hex_cov %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  summarise(
    n_hexes = n(),
    min = min(value, na.rm = TRUE),
    p05 = quantile(value, 0.05, na.rm = TRUE),
    p25 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    p75 = quantile(value, 0.75, na.rm = TRUE),
    p95 = quantile(value, 0.95, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  )

print(as_tibble(hex_cov_bin_counts), n = Inf)
print(as_tibble(hex_cov_distribution), n = Inf)

write_csv(
  hex_cov_bin_counts,
  file.path(out_dir, "sampling_uncertainty_cov_hex_bin_counts.csv")
)

write_csv(
  hex_cov_distribution,
  file.path(out_dir, "sampling_uncertainty_cov_hex_distribution.csv")
)

p_cov <- make_uncertainty_map(
  hex_sf = hex_cov,
  countries_df = countries
)

ggsave(
  filename = file.path(out_dir, "map_sampling_uncertainty_cov.png"),
  plot = p_cov,
  dpi = dpi,
  width = width,
  height = height,
  units = "in"
)
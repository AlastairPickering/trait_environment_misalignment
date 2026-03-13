library(dggridR)
library(tidyverse)
library(ggplot2)
library(sf)
library(scales)
library(viridis)

export_mahal_maps <- function(
    ssp370_file = "data/precomputed/mahalanobis_distance_all_mc.csv",
    baseline_file = "data/precomputed/mahalanobis_distance_baseline_mc.csv",
    out_dir = "output",
    delta_mode = c("mean", "median"),
    res = 7,
    xlim = c(-175, -50),
    ylim = c(20, 70),
    width = 12,
    height = 8,
    dpi = 300
) {
  
  delta_mode <- match.arg(delta_mode)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # helpers
  process_hexbin <- function(file_path, hex_grid, value_col) {
    
    ll <- read_csv(file_path, show_col_types = FALSE) %>%
      mutate(value = .data[[value_col]]) %>%
      filter(lon > xlim[1], lon < xlim[2], lat > ylim[1], lat < ylim[2])
    
    ll$grid_id <- dgGEO_to_SEQNUM(hex_grid, ll$lon, ll$lat)$seqnum
    
    hex_sum <- ll %>%
      group_by(grid_id) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    poly_grid <- dgcellstogrid(hex_grid, hex_sum$grid_id)
    poly_grid <- merge(poly_grid, hex_sum, by.x = "seqnum", by.y = "grid_id")
    
    if (is.na(st_crs(poly_grid))) st_crs(poly_grid) <- 4326
    
    poly_grid
  }
  
  # Flexible scale for each map
  tem_scale <- function(x, title) {
    
    ramp <- tryCatch(
      viridis::viridis(256, option = "turbo", direction = 1, begin = 0.05, end = 1),
      error = function(e) viridis::viridis(256, option = "inferno", direction = 1, begin = 0.05, end = 1)
    )
    
    vals_vec <- x$value
    vals_vec <- vals_vec[is.finite(vals_vec)]
    
    if (length(vals_vec) == 0) {
      lims <- c(0, 1)
      brks <- c(0, 0.5, 1)
    } else {
      lims <- range(vals_vec, na.rm = TRUE)
      
      if (diff(lims) == 0) {
        lims <- c(lims[1], lims[2] + 1e-6)
        brks <- lims[1]
      } else {
        step <- ceiling((lims[2] - lims[1]) / 4)
        brks <- seq(floor(lims[1]), ceiling(lims[2]), by = step)      }
    }
    
    vals <- scales::rescale(
      c(lims[1], seq(lims[1], lims[2], length.out = 256)),
      from = lims
    )
    
    scale_fill_gradientn(
      colours  = c("#FFFFFF", ramp),
      values   = vals,
      limits   = lims,
      breaks   = brks,
      oob      = scales::squish,
      na.value = "grey92",
      name     = title
    )
  }
  
  map_theme <- theme_bw() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 8)
    )
  
  make_map <- function(hex_sf, countries_df, scale_obj, title_text) {
    ggplot() +
      geom_sf(data = hex_sf, aes(fill = value), colour = NA) +
      geom_polygon(
        data = countries_df,
        aes(x = long, y = lat, group = group),
        fill = NA, colour = "black", linewidth = 0.5
      ) +
      scale_obj +
      coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggtitle(title_text) +
      map_theme
  }
  
  # inputs
  hex_grid <- dgconstruct(res = res)
  
  countries <- map_data("world") %>%
    filter(long > xlim[1], long < xlim[2], lat > ylim[1], lat < ylim[2])
  
  # surfaces
  hex_min  <- process_hexbin(ssp370_file, hex_grid, "mahal_min")
  hex_mean <- process_hexbin(ssp370_file, hex_grid, "mahal_mean")
  hex_med  <- process_hexbin(ssp370_file, hex_grid, "mahal_p50")
  
  # baseline surface
  hex_baseline <- process_hexbin(baseline_file, hex_grid, "mahal_min")
  
  # baseline − min
  baseline_delta_sf <- hex_min %>%
    transmute(seqnum, geometry, min_val = value) %>%
    left_join(
      st_drop_geometry(hex_baseline) %>% transmute(seqnum, base_val = value),
      by = "seqnum"
    ) %>%
    mutate(
      value = base_val - min_val,
      value = pmax(value, 0)
    )
  
  # Min map
  p_min <- make_map(
    hex_sf = hex_min,
    countries_df = countries,
    scale_obj = tem_scale(hex_min, "Minimum Mahalanobis Distance"),
    title_text = "SSP370: Minimum Mahalanobis Distance: Current vs Predicted 24 Trait Means"
  )
  
  ggsave(
    filename = file.path(out_dir, "map_mahalanobis_min.png"),
    plot = p_min, dpi = dpi, width = width, height = height, units = "in"
  )
  
  # Mean map
  p_mean <- make_map(
    hex_sf = hex_mean,
    countries_df = countries,
    scale_obj = tem_scale(hex_mean, "Mean Mahalanobis Distance"),
    title_text = "SSP370: Mean Mahalanobis Distance: Current vs Predicted 24 Trait Means"
  )
  
  ggsave(
    filename = file.path(out_dir, "map_mahalanobis_mean.png"),
    plot = p_mean, dpi = dpi, width = width, height = height, units = "in"
  )
  
  # Median map
  p_med <- make_map(
    hex_sf = hex_med,
    countries_df = countries,
    scale_obj = tem_scale(hex_med, "Median Mahalanobis Distance"),
    title_text = "SSP370: Median Mahalanobis Distance: Current vs Predicted 24 Trait Means"
  )
  
  ggsave(
    filename = file.path(out_dir, "map_mahalanobis_median.png"),
    plot = p_med, dpi = dpi, width = width, height = height, units = "in"
  )
  
  # baseline map
  p_baseline <- make_map(
    hex_sf = hex_baseline,
    countries_df = countries,
    scale_obj = tem_scale(hex_baseline, "TEM risk"),
    title_text = ""
  )
  
  ggsave(
    filename = file.path(out_dir, "map_mahalanobis_baseline.png"),
    plot = p_baseline, dpi = dpi, width = width, height = height, units = "in"
  )
  
  # delta map
  base_label <- "(Actual CWMs - Minimum Mahal)"
  base_title <- "SSP370: (Actual CWMs - Minimum Mahal): Current vs Predicted 24 Trait Means"
  
  p_base <- make_map(
    hex_sf = baseline_delta_sf,
    countries_df = countries,
    scale_obj = tem_scale(baseline_delta_sf, base_label),
    title_text = base_title
  )
  
  ggsave(
    filename = file.path(out_dir, "map_delta_baseline_minus_min.png"),
    plot = p_base, dpi = dpi, width = width, height = height, units = "in"
  )
  
  invisible(list(
    min = p_min,
    mean = p_mean,
    median = p_med,
    baseline = p_baseline,
    baseline_delta = p_base
  ))
}

export_mahal_maps(delta_mode = "mean")
library(dggridR)
library(tidyverse)
library(ggplot2)
library(sf)
library(scales)
library(viridis)

export_mahal_min_ssp_map <- function(
    ssp126_file = "data/precomputed/mahalanobis_distance_all_mc_ssp126.csv",
    ssp370_file = "data/precomputed/mahalanobis_distance_all_mc_ssp370.csv",
    ssp585_file = "data/precomputed/mahalanobis_distance_all_mc_ssp585.csv",
    out_dir = "output",
    res = 7,
    xlim = c(-175, -50),
    ylim = c(20, 70),
    width = 12,
    height = 24,
    dpi = 300,
    trim_probs = c(0, 0.99)
) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  process_hexbin <- function(file_path, hex_grid, value_col, scenario) {
    
    ll <- read_csv(file_path, show_col_types = FALSE) %>%
      mutate(value = .data[[value_col]]) %>%
      filter(
        lon > xlim[1], lon < xlim[2],
        lat > ylim[1], lat < ylim[2]
      )
    
    ll$grid_id <- dgGEO_to_SEQNUM(hex_grid, ll$lon, ll$lat)$seqnum
    
    hex_sum <- ll %>%
      group_by(grid_id) %>%
      summarise(
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    poly_grid <- dgcellstogrid(hex_grid, hex_sum$grid_id)
    poly_grid <- merge(poly_grid, hex_sum, by.x = "seqnum", by.y = "grid_id")
    
    if (is.na(st_crs(poly_grid))) {
      st_crs(poly_grid) <- 4326
    }
    
    poly_grid %>%
      mutate(scenario = scenario)
  }
  
  tem_scale <- function(x, title, trim_probs = c(0.01, 0.99)) {
    
    ramp <- tryCatch(
      viridis::viridis(
        256,
        option = "turbo",
        direction = 1,
        begin = 0.05,
        end = 1
      ),
      error = function(e) {
        viridis::viridis(
          256,
          option = "inferno",
          direction = 1,
          begin = 0.05,
          end = 1
        )
      }
    )
    
    vals_vec <- x$value
    vals_vec <- vals_vec[is.finite(vals_vec)]
    
    if (length(vals_vec) == 0) {
      lims <- c(0, 1)
      brks <- c(0, 0.5, 1)
    } else {
      lims <- as.numeric(
        quantile(
          vals_vec,
          probs = trim_probs,
          na.rm = TRUE,
          names = FALSE
        )
      )
      
      if (!all(is.finite(lims)) || diff(lims) == 0) {
        lims <- range(vals_vec, na.rm = TRUE)
      }
      
      if (diff(lims) == 0) {
        lims <- c(lims[1], lims[2] + 1e-6)
        brks <- lims[1]
      } else {
        brks <- pretty(lims, n = 5)
        brks <- brks[brks >= lims[1] & brks <= lims[2]]
      }
    }
    
    scale_fill_gradientn(
      colours  = ramp,
      limits   = lims,
      breaks   = brks,
      oob      = scales::squish,
      na.value = "white",
      name     = title
    )
  }
  
  map_theme <- theme_bw() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 8),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(fill = "white", colour = "black"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank()
    )
  
  hex_grid <- dgconstruct(res = res)
  
  countries <- map_data("world") %>%
    filter(
      long > xlim[1], long < xlim[2],
      lat > ylim[1], lat < ylim[2]
    )
  
  hex_min_ssp126 <- process_hexbin(
    file_path = ssp126_file,
    hex_grid = hex_grid,
    value_col = "mahal_min",
    scenario = "SSP126"
  )
  
  hex_min_ssp370 <- process_hexbin(
    file_path = ssp370_file,
    hex_grid = hex_grid,
    value_col = "mahal_min",
    scenario = "SSP370"
  )
  
  hex_min_ssp585 <- process_hexbin(
    file_path = ssp585_file,
    hex_grid = hex_grid,
    value_col = "mahal_min",
    scenario = "SSP585"
  )
  
  hex_all <- bind_rows(
    hex_min_ssp126,
    hex_min_ssp370,
    hex_min_ssp585
  ) %>%
    mutate(
      scenario = factor(
        scenario,
        levels = c("SSP126", "SSP370", "SSP585")
      )
    )
  
  trim_summary <- hex_all %>%
    st_drop_geometry() %>%
    filter(is.finite(value)) %>%
    summarise(
      lower_trim = quantile(value, trim_probs[1], na.rm = TRUE),
      upper_trim = quantile(value, trim_probs[2], na.rm = TRUE)
    )
  
  tem_summary <- hex_all %>%
    st_drop_geometry() %>%
    group_by(scenario) %>%
    summarise(
      mean_tem_risk = mean(value, na.rm = TRUE),
      median_tem_risk = median(value, na.rm = TRUE),
      n_hexes = sum(is.finite(value)),
      .groups = "drop"
    ) %>%
    arrange(scenario)
  
  monotonic_check <- tem_summary %>%
    summarise(
      mean_is_monotonic = all(diff(mean_tem_risk) >= 0),
      median_is_monotonic = all(diff(median_tem_risk) >= 0)
    )
  
  print(tem_summary)
  print(monotonic_check)
  print(trim_summary)
  
  p <- ggplot() +
    geom_sf(data = hex_all, aes(fill = value), colour = NA) +
    geom_polygon(
      data = countries,
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "black",
      linewidth = 0.5
    ) +
    tem_scale(
      hex_all,
      "TEM risk (std. dev.)",
      trim_probs = trim_probs
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    facet_wrap(~ scenario, ncol = 1) +
    ggtitle("Minimum Mahalanobis Distance: Current vs Predicted 24 Trait Means") +
    map_theme
  
  ggsave(
    filename = file.path(out_dir, "map_mahalanobis_min_ssp126_ssp370_ssp585.png"),
    plot = p,
    dpi = dpi,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
  
  invisible(list(
    map = p,
    ssp126 = hex_min_ssp126,
    ssp370 = hex_min_ssp370,
    ssp585 = hex_min_ssp585,
    combined = hex_all,
    tem_summary = tem_summary,
    monotonic_check = monotonic_check,
    trim_summary = trim_summary
  ))
}

results <- export_mahal_min_ssp_map()

results$tem_summary
results$monotonic_check
results$trim_summary
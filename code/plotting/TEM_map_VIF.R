rm(list = ls())

library(dggridR)
library(tidyverse)
library(ggplot2)
library(viridis)

# Function to process a dataset and return a hexbin grid with summarised values
process_hexbin <- function(file_path, hex_grid) {
  # Read the data and filter coordinates
  ll <- read_csv(file_path) %>%
    mutate(value = total_FV_VIF) %>%
    filter(lon > -175, lon < -50, lat > 20, lat < 70)
  
  # Assign a grid id to each lat/long
  ll$grid_id <- dgGEO_to_SEQNUM(hex_grid, ll$lon, ll$lat)$seqnum
  
  # Summarise the lat long values by the hex grid number
  hex_sum <- ll %>% group_by(grid_id) %>% summarise(value = mean(value, na.rm = TRUE))
  
  # Convert the hex grid to a polygon
  poly_grid <- dgcellstogrid(hex_grid, hex_sum$grid_id)
  
  # Merge the summary values into the polygons
  poly_grid <- merge(poly_grid, hex_sum, by.x = "seqnum", by.y = "grid_id")
  
  # Return the hexagonal grid with the value column
  return(poly_grid)
}

# Set up the hex grid resolution
hex_grid <- dgconstruct(res = 7)

# Specify the file for SSP370
ssp370_file <- "data/precomputed/ssp370_with_FV_VIF.csv"

# Process SSP370 data
hex_data <- process_hexbin(ssp370_file, hex_grid)

# Normalize the values for plotting
hex_data$normalized_value <- (hex_data$value - min(hex_data$value, na.rm = TRUE)) / 
  (max(hex_data$value, na.rm = TRUE) - min(hex_data$value, na.rm = TRUE))

# Read country boundaries
countries <- map_data("world") %>%
  filter(long > -175, long < -50, lat > 20, lat < 70)

# Create the plot
plot <- ggplot() +
  geom_sf(data = hex_data, aes(fill = value), color = NA) +
  geom_polygon(data = countries, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(
    option = "C",
    direction = 1,
    name = "Mean TEM per Plot"
  ) +
  theme_bw() +
  ggtitle("SSP370: Mean VIF-weighted TEM Per Plot") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Save the plot as a high-resolution PNG file
ggsave(
  "output/VIF_TEM.png",
  plot = plot,
  dpi = 300,
  width = 12,
  height = 12,
  units = "in"
)

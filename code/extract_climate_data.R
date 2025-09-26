# This code extracts the CHELSA BIO climate information from .tif files for each
# SSP and merges it into the forest trait and static geographic variables created 
# in the forest_trait_means_creation.R file. The output of this code forms the basis
# of the main model code functional_vulnerability.ipynb

rm(list = ls())

# Load libraries
library(terra)
library(tidyverse)

# Read in lat/lon data from CSV and rename to match expected column names
traits_df <- read_csv("data/precomputed/forest_trait_means.csv")

# Extract only the lon/lat columns, ensuring correct column names for terra
ll <- traits_df %>%
  dplyr::select(lon, lat)

# Define the list of folders containing the tif files
folders <- c("current", "ssp126", "ssp370", "ssp585")

# Loop through each folder to extract and save the data
for (folder in folders) {
  
  # Construct the file path for the current folder
  tif_path <- file.path("data", "climate", folder)
  
  # Get the list of all raster files in the specified directory
  tifs <- list.files(path = tif_path, pattern = "\\.tif$", full.names = TRUE)
  
  # Check that tif files were found; if not, issue a warning and continue to the next folder
  if (length(tifs) == 0) {
    warning(paste("No tif files found in folder:", folder))
    next
  }
  
  # Initialise the data frame 'dt' with the first raster file's extracted values
  dt <- terra::extract(rast(tifs[1]), ll)
  dt <- as_tibble(dt)
  
  # Iterate through the rest of the tifs and add extracted data to 'dt'
  for (f in tifs[-1]) {
    raster_values <- terra::extract(rast(f), ll)
    dt <- dt %>% bind_cols(as_tibble(raster_values) %>% dplyr::select(-ID))  # Exclude "ID" to avoid duplicate columns
  }
  
  # Remove the 'ID' column
  dt <- dt %>% dplyr::select(-ID)
  
  # Combine the extracted raster data with the original traits data frame based on row order
  export_df <- bind_cols(traits_df, dt)
  
  # Construct the output file path in 'data/precomputed'
  output_filename <- file.path("data", "precomputed", paste0("plot_and_abiotic_data_", folder, ".csv"))
  
  # Write the combined data to a new CSV file
  write_csv(export_df, output_filename)
  
  cat("Exported data for folder", folder, "to", output_filename, "\n")
}

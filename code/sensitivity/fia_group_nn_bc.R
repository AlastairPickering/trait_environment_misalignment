library(tidyverse)
library(vegan)

# Read species proportions: rows = pid, columns = species proportions
species_comp <- read_csv("data/precomputed/species_per_plot_filtered.csv") %>%
  mutate(pid = as.character(pid))

# Read plot metadata with FIA_group
plot_meta <- read_csv("data/precomputed/plot_and_abiotic_data_current.csv") %>%
  mutate(pid = as.character(pid)) %>%
  select(pid, FIA_group, lon, lat)

# Join FIA_group onto species composition and define country
comp_df <- species_comp %>%
  left_join(plot_meta, by = "pid") %>%
  filter(!is.na(FIA_group)) %>%
  filter(!str_starts(FIA_group, "Other")) %>%
  mutate(country = if_else(str_starts(pid, "p"), "US", "Canada"))

# Species columns are everything except metadata
metadata_cols <- c("pid", "FIA_group", "lon", "lat", "country")
species_cols <- setdiff(names(comp_df), metadata_cols)

# Ensure species columns are numeric and missing values are zero
comp_df <- comp_df %>%
  mutate(across(all_of(species_cols), ~ replace_na(as.numeric(.x), 0)))

# Remove plots with no species composition
comp_df <- comp_df %>%
  rowwise() %>%
  mutate(total_species_prop = sum(c_across(all_of(species_cols)), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(total_species_prop > 0) %>%
  select(-total_species_prop)

# Create metadata table
meta <- comp_df %>%
  mutate(row_id = row_number()) %>%
  select(row_id, pid, FIA_group, country, lon, lat)

# Community matrix
species_mat <- comp_df %>%
  select(all_of(species_cols)) %>%
  as.matrix()

# Bray-Curtis distance matrix
bc_dist <- as.matrix(vegdist(species_mat, method = "bray"))

# FIA groups available in the U.S. data
us_group_counts <- meta %>%
  filter(country == "US") %>%
  count(FIA_group, name = "n_us")

# Canadian plots can be validated where their assigned FIA group exists in U.S. data
canada_valid_groups <- us_group_counts %>%
  filter(n_us > 0) %>%
  pull(FIA_group)

# U.S. baseline requires at least two U.S. plots in the same FIA group
us_valid_groups <- us_group_counts %>%
  filter(n_us > 1) %>%
  pull(FIA_group)

canadian_ids <- meta %>%
  filter(country == "Canada", FIA_group %in% canada_valid_groups) %>%
  pull(row_id)

us_ids <- meta %>%
  filter(country == "US", FIA_group %in% us_valid_groups) %>%
  pull(row_id)

# Canadian plots: nearest U.S. compositional analogue within same assigned FIA group
canada_nn_bc <- map_dfr(canadian_ids, function(i) {
  group_i <- meta$FIA_group[i]
  
  us_same <- meta %>%
    filter(country == "US", FIA_group == group_i) %>%
    pull(row_id)
  
  tibble(
    pid = meta$pid[i],
    country = "Canada",
    FIA_group = group_i,
    lon = meta$lon[i],
    lat = meta$lat[i],
    nearest_same_group_bc = min(bc_dist[i, us_same], na.rm = TRUE)
  )
})

# U.S. baseline: nearest other U.S. plot within the same FIA group
us_nn_bc <- map_dfr(us_ids, function(i) {
  group_i <- meta$FIA_group[i]
  
  us_same_other <- meta %>%
    filter(
      country == "US",
      FIA_group == group_i,
      row_id != i
    ) %>%
    pull(row_id)
  
  tibble(
    pid = meta$pid[i],
    country = "US",
    FIA_group = group_i,
    lon = meta$lon[i],
    lat = meta$lat[i],
    nearest_same_group_bc = min(bc_dist[i, us_same_other], na.rm = TRUE)
  )
})

# Combine Canada validation and U.S. baseline
nn_bc <- bind_rows(canada_nn_bc, us_nn_bc)

# Overall summaries
overall_nn_summary <- nn_bc %>%
  group_by(country) %>%
  summarise(
    n_plots = n(),
    median_nearest_bc = median(nearest_same_group_bc, na.rm = TRUE),
    q25_nearest_bc = quantile(nearest_same_group_bc, 0.25, na.rm = TRUE),
    q75_nearest_bc = quantile(nearest_same_group_bc, 0.75, na.rm = TRUE),
    mean_nearest_bc = mean(nearest_same_group_bc, na.rm = TRUE),
    .groups = "drop"
  )

# Group-level summaries
group_nn_summary <- nn_bc %>%
  group_by(FIA_group, country) %>%
  summarise(
    n_plots = n(),
    median_nearest_bc = median(nearest_same_group_bc, na.rm = TRUE),
    q25_nearest_bc = quantile(nearest_same_group_bc, 0.25, na.rm = TRUE),
    q75_nearest_bc = quantile(nearest_same_group_bc, 0.75, na.rm = TRUE),
    mean_nearest_bc = mean(nearest_same_group_bc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(FIA_group, country)

group_nn_summary_wide <- group_nn_summary %>%
  select(
    FIA_group, country, n_plots,
    median_nearest_bc, q25_nearest_bc, q75_nearest_bc,
    mean_nearest_bc
  ) %>%
  pivot_wider(
    names_from = country,
    values_from = c(
      n_plots,
      median_nearest_bc,
      q25_nearest_bc,
      q75_nearest_bc,
      mean_nearest_bc
    )
  )

overall_nn_summary
group_nn_summary_wide
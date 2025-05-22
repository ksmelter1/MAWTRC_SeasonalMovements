#'---
#' title: Seasonal Movements of Wild Turkeys in the Mid-Atlantic Region
#' author: "K. Smelter, F. Buderman"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: df.Winter.Rdata
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script downloads movement data associated with nesting hens for NSD calculation
#' **Last Updated**: 5/21/25
#' 

################################################################################
## Load Packages

#' Vector of package names
packages <- c("purrr",
              "lubridate",
              "ggplot2",
              "move2",
              "tidyverse",
              "sf",
              "stringr")

#' Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package name
lapply(packages, load_packages)

################################################################################
## Load in Data

dat.5c <- readRDS("Data Management/RData/Pennsylvania/GPS Data/5C/GPS.5c.RDS")

#' Create an sf object from lon/lat
df_sf <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # WGS84

#' Transform to UTM (EPSG:32618 is UTM Zone 18N)
df_sf_utm <- st_transform(df_sf, crs = 32618)

#' Extract UTM coordinates
df_coords <- df_sf_utm %>%
  mutate(
    utm_x = st_coordinates(.)[, 1],
    utm_y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# Calculate daily displacement in meters
df_displacement <- df_coords %>%
  arrange(individual_local_identifier, timestamp) %>%
  group_by(individual_local_identifier) %>%
  mutate(
    displacement = sqrt((utm_x - lag(utm_x))^2 + (utm_y - lag(utm_y))^2),
    date = as_date(timestamp)
  ) %>%
  ungroup()

#' Recalculate NSD in meters²
df_nsd_meters <- df_displacement %>%
  arrange(individual_local_identifier, timestamp) %>%
  group_by(individual_local_identifier) %>%
  mutate(
    x0 = first(utm_x),
    y0 = first(utm_y),
    nsd_m = (utm_x - x0)^2 + (utm_y - y0)^2,
    date = as_date(timestamp)
  ) %>%
  ungroup()

# Merge NSD and displacement into one summary per day
mean_nsd_per_day_m <- df_displacement %>%
  group_by(individual_local_identifier, date) %>%
  summarise(
    mean_nsd_m = mean((utm_x - first(utm_x))^2 + (utm_y - first(utm_y))^2, na.rm = TRUE),
    mean_disp_m = mean(displacement, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    days_numeric = yday(date),
    BirdID = paste(
      str_sub(individual_local_identifier, 1, 4),
      str_sub(individual_local_identifier, 6, 9),
      sep = "_"
    )
  ) %>%
  filter(month(date) >= 3 & month(date) <= 5)  # Filter March–May

#' Get all unique IDs
ids <- unique(mean_nsd_per_day_m$individual_local_identifier)

for (id in ids) {
  plot_data <- mean_nsd_per_day_m %>%
    filter(individual_local_identifier == id)
  
  p <- ggplot(plot_data, aes(x = days_numeric, y = mean_nsd_m)) +
    geom_line(color = "forestgreen", size = 1) +
    geom_point(color = "darkgreen") +
    labs(
      title = "Mean Net-Squared Displacement Over Time (Meters²)",
      subtitle = paste("Individual:", id),
      x = "Calendar Day",
      y = "Mean NSD (m²)"
    ) +
    scale_x_continuous(breaks = seq(min(plot_data$days_numeric, na.rm = TRUE),
                                    max(plot_data$days_numeric, na.rm = TRUE),
                                    by = 15)) +
    theme_light()
  
  print(p)
  Sys.sleep(1)
}


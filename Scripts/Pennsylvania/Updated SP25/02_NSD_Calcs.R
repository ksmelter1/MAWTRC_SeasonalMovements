#'---
#' title: Seasonal Movements of Wild Turkeys in the Mid-Atlantic Region
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: df.Winter.Rdata
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script downloads movement data associated with nesting hens for NSD calculation
#' **Last Updated**: 5/21/25


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


load("Data Management/RData/Pennsylvania/GPS Data/NestingHensGPS.RData")

#' Create an sf object from lon/lat
#' Convert to utm coordinate system
df_sf <-df.all%>%
  mutate(long = unlist(map(df.all$geometry,1)),
         lat = unlist(map(df.all$geometry,2))) %>%
  dplyr::select(BirdID, NestID, timestamp,long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(32618)

#' Extract UTM coordinates
df_coords <- df_sf %>%
  mutate(
    utm_x = st_coordinates(.)[, 1],
    utm_y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# Calculate daily displacement in meters
df_displacement <- df_coords %>%
  arrange(NestID, timestamp) %>%
  group_by(NestID) %>%
  mutate(
    displacement = sqrt((utm_x - lag(utm_x))^2 + (utm_y - lag(utm_y))^2),
    date = as_date(timestamp),
    timestamp = first(timestamp)
  ) %>%
  ungroup()

#' Recalculate NSD in meters²
df_nsd_meters <- df_displacement %>%
  arrange(NestID, timestamp) %>%
  group_by(NestID) %>%
  mutate(
    x0 = first(utm_x),
    y0 = first(utm_y),
    nsd_m = (utm_x - x0)^2 + (utm_y - y0)^2,
    date = as_date(timestamp)
  ) %>%
  ungroup()

#' Calculate mean net squared displacement for each individual
#' Create a days_numeric column
#' Constrain data to March-May
mean_nsd_per_day_m <- df_displacement %>%
  group_by(NestID, date) %>%
  summarise(
    mean_nsd_m = mean((utm_x - first(utm_x))^2 + (utm_y - first(utm_y))^2, na.rm = TRUE),
    mean_disp_m = mean(displacement, na.rm = TRUE),
    timestamp = first(timestamp)
  ) %>%
  ungroup() %>%
  mutate(
    days_numeric = yday(date),
    BirdID = paste(
      str_sub(NestID, 1, 4),
      str_sub(NestID, 6, 9),
      sep = "_")
  ) %>%
  filter(month(date) >= 3 & month(date) <= 5)


#' Get all unique IDs
ids <- unique(mean_nsd_per_day_m$NestID)

for (id in ids) {
  plot_data <- mean_nsd_per_day_m %>%
    filter(NestID == id)
  
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

#' Subset columns 
mean_nsd_per_day_m <- mean_nsd_per_day_m %>%
  dplyr::select(all_of(c("NestID", "BirdID", "timestamp", "mean_nsd_m", "mean_disp_m", "days_numeric")))


#' Rename columns 
dat <- mean_nsd_per_day_m %>%
  dplyr::rename("t_" = timestamp) %>%
  dplyr::rename("nsd_daily_mean" = mean_nsd_m) %>%
  dplyr::rename("displacement" = mean_disp_m)


#' Write a csv for each unique NestID
#' Filter data by unique NestID
#' Store in directory titled Nest Data
#' Create a directory to store CSV files if it doesn't exist
output_dir <- "Data Management/Csvs/Hen_NSD_Data/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

#' Loop through each unique NestID and write a CSV
unique_nests <- unique(dat$NestID)

for (nest in unique_nests) {
  nest_data <- dat %>% filter(NestID == nest)
  filename <- paste0(output_dir, "/", nest, ".csv")
  write.csv(nest_data, filename, row.names = FALSE)
  print(paste("CSV for NestID", nest, "is written"))
}

################################################################################
###############################################################################X

#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#' **Purpose**: This script creates NSD plots in km as well as visual GPS checks
#' **Last Updated**: 19 February 2026


################################################################################
## Load Packages

# Package names
packages<-c("tidyverse", 
            "here", 
            "mcp", 
            "lubridate", 
            "knitr", 
            "glue",
            "ezknitr", 
            "loo", 
            "geosphere",
            "flextable",
            "readr",
            "ggplot2",
            "mapview",
            "scales",
            "geosphere",
            "sf")

# Install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


################################################################################
## Data Prep

# Read in full dataset
df <- read_csv(here("Data Management/Csvs/Hen_NSD_Data/Full/all_nsd2025.csv"))
df
length(unique(df$BirdID))

# Create yr and month columns using lubridate
df <- df %>%
  dplyr::mutate(
    yr = lubridate::year(timestamp),
    month = lubridate::month(timestamp)
  )

# Create julian day column
df$julian <- yday(df$timestamp)

# Create julian date column
df$jdate <- as.Date(paste(as.character(df$yr), as.character(df$julian), sep = "-"), "%Y-%j")

# Calculate average daily nsd value
df <- df %>%
  dplyr::group_by(BirdID, jdate) %>%
  dplyr::mutate(daily_nsd = mean(nsd))

# Create sf object
# Filter data between February and May
# Create a month column
df_sub_sf <- df %>%
  dplyr::filter(month(jdate) >= 2 & month(jdate) < 5) %>%
  st_as_sf(., coords = c("lat", "long"), crs = 4326) %>%
  dplyr::mutate(month = lubridate::month(timestamp)) 

# Function to clean GPS outliers per BirdID
remove_gps_outliers <- function(df_group) {
  coords <- st_coordinates(df_group)
  med_lat <- median(coords[, 2], na.rm = TRUE)
  med_long <- median(coords[, 1], na.rm = TRUE)
  
  # Calculate distance to median point
  df_group$dist_m <- distHaversine(coords, c(med_long, med_lat))
  
  # Remove top 1% distance outliers
  df_group <- df_group %>%
    filter(dist_m <= quantile(dist_m, 0.90, na.rm = TRUE))
  
  return(df_group)
}

# Apply per BirdID
df_sub_sf_clean <- df_sub_sf %>%
  group_by(BirdID) %>%
  group_split() %>%
  lapply(remove_gps_outliers) %>%
  bind_rows() 

# Drop geometry
df_sub_clean <- df_sub_sf %>%
  st_drop_geometry()

# Reduce down to a single point per day
nsd_sub <- df_sub_clean %>%
  distinct(BirdID, jdate, daily_nsd)

# Create a days_numeric column 
# Create a year column
# Convert to km following Wolfson et al. (2024)
# Remove all values for a BirdID if the data object contains less than 30 observations
# Remove all BirdIDs with less than 30 observations
nsd_sub <- nsd_sub %>%
  dplyr::mutate(
    days_numeric = lubridate::yday(jdate),
    year = lubridate::year(jdate),
    nsd_plot_data = sqrt(daily_nsd) / 1000
  ) %>%
  dplyr::group_by(BirdID) %>%
  dplyr::filter(dplyr::n() >= 30) %>%
  dplyr::ungroup() 

# 466 is our sample
# 41 birds were removed
length(unique(nsd_sub$BirdID))

################################################################################
## Create NSD Plots

# Create NSD plots and write filtered data to CSV
ids <- unique(nsd_sub$BirdID)

# Loop through each Bird ID
for (i in seq_along(ids)) {
  
  # Subset data for this BirdID
  bird_data <- nsd_sub[nsd_sub$BirdID == ids[[i]], ]
  
  # Create plot and assign to variable `p`
  p <- ggplot(bird_data, aes(jdate, nsd_plot_data)) +
    geom_line() +
    labs(
      y = "Displacement in km",
      x = "Date",
      title = paste(ids[[i]])
    ) +
    theme(plot.title = element_text(size = 22))
  
  # Save plot (overwrites by default)
  ggsave(
    filename = here::here(glue("Data Management/Csvs/Hen_NSD_Data/NSD Plots/NSD Plots/{ids[[i]]}.pdf")),
    plot = p,
    device = "pdf"
  )
  
  # Save filtered data to CSV (also overwrites by default)
  write_csv(
    bird_data,
    file = here::here(glue("Data Management/Csvs/Hen_NSD_Data/Individual/Individual Csvs/{ids[[i]]}_data.csv"))
  )
}

################################################################################
## Visualize GPS Data during Spring Movements

# Define base colors for each month
month_base_colors <- c("2" = "red", "3" = "blue", "4" = "green")

# PDF output
pdf("Data Management/Csvs/Hen_NSD_Data/NSD Plots/GPS Visuals/GPS_Locations_By_Month_Per_BirdID_Spring.pdf", width = 8, height = 6)

unique_ids <- unique(df_sub_sf_clean$BirdID)

for (id in unique_ids) {
  
  df_bird <- df_sub_sf_clean %>%
    filter(BirdID == id) %>%
    mutate(
      month = factor(month(timestamp)),  
      day_in_month = day(timestamp),
      normalized_day = day_in_month / days_in_month(timestamp),
      color_shade = mapply(function(m, n) alpha(month_base_colors[as.character(m)], n), 
                           month, normalized_day)
    )
  
  p <- ggplot(df_bird) +
    geom_sf(aes(color = month, alpha = normalized_day), size = 2) +  
    scale_color_manual(values = month_base_colors, name = "Month") +
    scale_alpha(range = c(0.3, 1), guide = "none") +  
    theme_light() +
    labs(
      title = paste("GPS Locations by Month for BirdID:", id),
      x = "Longitude",
      y = "Latitude"
    )
  
  print(p)
}

dev.off()


################################################################################
## Separate Lattitude and Longitude plots for each hen-year dataset

# df used to create lattitude and longitude plots
df_sub <- df %>%
  dplyr::filter(month(jdate) >= 2 & month(jdate) < 5) %>%
  dplyr::mutate(month = lubridate::month(timestamp))


################
## Lattitude ##
################

pdf("Latitude_Over_Time_By_BirdID.pdf", width = 8, height = 6)


for (id in unique_ids) {
  df_bird <- df_sub %>%
    filter(BirdID == id) %>%
    mutate(
      month = factor(month(timestamp)),
      day_in_month = day(timestamp),
      normalized_day = day_in_month / days_in_month(timestamp),
      color_shade = mapply(function(m, n) alpha(month_base_colors[as.character(m)], n), 
                           month, normalized_day)
    )
  
  p_lat <- ggplot(df_bird, aes(x = jdate, y = lat)) +
    geom_point(aes(color = month, alpha = normalized_day), size = 2) +
    scale_color_manual(values = month_base_colors, name = "Month") +
    scale_alpha(range = c(0.3, 1), guide = "none") +
    theme_light() +
    labs(
      title = paste("Latitude Over Time for BirdID:", id),
      x = "Date",
      y = "Latitude"
    )
  print(p_lat)
}
dev.off()  


################
## Longitude ##
################

# Create Longitude plots and save to separate PDF
pdf("Data Management/Csvs/GPS Visuals/Longitude_Over_Time_By_BirdID.pdf", width = 8, height = 6)

for (id in unique_ids) {
  df_bird <- df_sub %>%
    filter(BirdID == id) %>%
    mutate(
      month = factor(month(timestamp)),
      day_in_month = day(timestamp),
      normalized_day = day_in_month / days_in_month(timestamp),
      color_shade = mapply(function(m, n) alpha(month_base_colors[as.character(m)], n), 
                           month, normalized_day)
    )
  
  p_long <- ggplot(df_bird, aes(x = jdate, y = long)) +
    geom_point(aes(color = month, alpha = normalized_day), size = 2) +
    scale_color_manual(values = month_base_colors, name = "Month") +
    scale_alpha(range = c(0.3, 1), guide = "none") +
    theme_light() +
    labs(
      title = paste("Longitude Over Time for BirdID:", id),
      x = "Date",
      y = "Longitude"
    )
  print(p_long)
}
dev.off()  

################################################################################
###############################################################################X
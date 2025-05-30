#'---
#' title: Seasonal Movements of Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#' **Purpose**: This script creates NSD plots in km 
#' **Last Updated**: 5/30/25


################################################################################
## Load Packages


#' Package names
packages<-c("tidyverse", 
            "here", 
            "mcp", 
            "lubridate", 
            "knitr", 
            "ezknitr", 
            "loo", 
            "flextable",
            "readr",
            "ggplot2",
            "mapview")

#' Install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

#' Load packages
invisible(lapply(packages, library, character.only = TRUE))


################################################################################
## Data Prep

#' Read in full dataset
df <- read_csv(here("Data Management/Csvs/Hen_NSD_Data/Full/full_w_nsd.csv"))
df

#' Create yr and month columns using lubridata
df <- df %>%
  dplyr::mutate(
    yr = lubridate::year(timestamp),
    month = lubridate::month(timestamp)
  )

#' Create julian day column
df$julian <- yday(df$timestamp)

#' Create julian date column
df$jdate <- as.Date(paste(as.character(df$yr), as.character(df$julian), sep = "-"), "%Y-%j")

#' Calculate average daily nsd value
df <- df %>%
  dplyr::group_by(BirdID, jdate) %>%
  dplyr::mutate(daily_nsd = mean(nsd))

#' Reduce down to a single point per day
nsd_sub <- df %>%
  distinct(BirdID, jdate, daily_nsd)

#' Filter data to only include months between February and May
#' Create a days_numeric column 
#' Create a year column
nsd_sub <- nsd_sub %>%
  dplyr::filter(month(jdate) >= 2 & month(jdate) < 6) %>%
  dplyr::mutate(days_numeric = yday(jdate),
                year = lubridate::year(jdate),
                nsd_plot_data = sqrt(daily_nsd) / 1000)


################################################################################
## Create NSD Plots

#' Create NSD plots and write filtered data to CSV
ids <- unique(nsd_sub$BirdID)

for (i in seq_along(ids)) {
  
  #' Subset data for this BirdID
  bird_data <- nsd_sub[nsd_sub$BirdID == ids[[i]], ]
  
  #' Plot
  ggplot(bird_data, aes(jdate, nsd_plot_data)) +
    geom_line() +
    labs(y = "Displacement in km", x = "Date", title = paste(ids[[i]])) +
    theme(plot.title = element_text(size = 22))
  
  #' Save plot
  ggsave(here::here(glue::glue("Data Management/Csvs/Hen_NSD_Data_Plots/{ids[[i]]}.pdf")))
  
  #' Save data to CSV
  write_csv(bird_data, here::here(glue::glue("Data Management/Csvs/Hen_NSD_Data/Individual/{ids[[i]]}_data.csv")))
}

################################################################################
## Visualize GPS Data during Spring Movements

#' Create sf object
#' Filter data between February and May
#' Create a month
df_sub_sf <- df %>%
  dplyr::filter(month(jdate) >= 2 & month(jdate) < 6) %>%
  st_as_sf(., coords = c("lat", "long"), crs = 4326) %>%
  dplyr::mutate(month = lubridate::month(timestamp))
mapview(df_sub_sf)

#' Create ggplot
ggplot(data = df_sub_sf) +
  geom_sf(aes(color = factor(month)), size = 2) +
  scale_color_brewer(palette = "Set1", name = "Month") +
  theme_light() +
  labs(
    title = "GPS Locations by Month for BirdID 8805_2023",
    x = "Longitude",
    y = "Latitude"
  )

#' Create a PDF file to save plots
pdf("GPS_Locations_By_Month_Per_BirdID.pdf", width = 8, height = 6)

#' Loop through each unique BirdID
unique_ids <- unique(df_sub_sf$BirdID)

for (id in unique_ids) {
  
  df_bird <- df_sub_sf %>%
    filter(BirdID == id)
  
  p <- ggplot(data = df_bird) +
    geom_sf(aes(color = factor(month)), size = 2) +
    scale_color_brewer(palette = "Set1", name = "Month") +
    theme_light() +
    labs(
      title = paste("GPS Locations by Month for BirdID:", id),
      x = "Longitude",
      y = "Latitude"
    )
  
  print(p)  #' Sends the plot to the PDF
}

dev.off()  #' Closes the PDF file

################################################################################
###############################################################################X
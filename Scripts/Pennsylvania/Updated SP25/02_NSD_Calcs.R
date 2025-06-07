#'---
#' title: Seasonal Movements of Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script downloads movement data associated with nesting hens for NSD calculation
#' **Last Updated**: 5/30/25


################################################################################
## Load Packages

#' Vector of package names
packages <- c("purrr",
              "lubridate",
              "ggplot2",
              "move2",
              "tidyverse",
              "sf",
              "stringr",
              "amt",
              "here")

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

#' Data with all nesting hens
load("Data Management/RData/Pennsylvania/GPS Data/NestingHensGPS.RData")

#' Data with all non-nesting hens
load("Data Management/RData/Pennsylvania/GPS Data/NonNestingHensGPS.RData")

#' Bind non-nesting and nesting hen data together
df.all <- bind_rows(df.nesting, df.nonnesting)


################################################################################
## NSD Calculations using Net Squared Displacement

#' Create the sf object from lon/lat and convert to UTM (zone 18N, EPSG:32618)
df_sf <- df.all %>%
  mutate(
    long = unlist(map(geometry, 1)),
    lat = unlist(map(geometry, 2))
  ) %>%
  dplyr::select(BirdID, NestID, timestamp, long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(32618)

#' Extract UTM coordinates and include original lat/lon
#' Drop geometry at the end because it is unnecessary
#' Create BirdID column that exists for each hen for each year
df_coords <- df_sf %>%
  dplyr::mutate(
    utm_E = st_coordinates(.)[, 1],
    utm_N = st_coordinates(.)[, 2],
    long = st_coordinates(st_transform(., 4326))[, 1],
    lat = st_coordinates(st_transform(., 4326))[, 2]
  ) %>%
  st_drop_geometry() 


#' Create a track using amt
trk <- mk_track(df_coords, .x = utm_E, .y = utm_N, .t = timestamp, id = BirdID, all_cols = T)

#' Calculate nsd using amt package
trk2 <- trk %>% nest(data = -BirdID) %>%
  dplyr::mutate(nsd = map(.x = data, .f = amt::nsd)) %>%
  unnest(cols = c(data, nsd))

#' Output df
df_nsd <- data.frame(BirdID = trk2[[1]], 
                     x = trk2[[2]],
                     y = trk2[[3]],
                     timestamp = trk2[[4]],
                     lat = trk2[[6]],
                     long = trk2[[7]],
                     nsd = trk2[[8]]
                     )

#' Output dataset
write_csv(df_nsd, here("Data Management/Csvs/Hen_NSD_Data/full_nsd.csv"))

################################################################################
###############################################################################X

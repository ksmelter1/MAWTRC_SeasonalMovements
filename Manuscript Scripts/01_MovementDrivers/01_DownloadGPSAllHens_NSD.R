#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'  
#' **Purpose**: This script downloads movement data associated with nesting hens for NSD calculation
#' **Last Updated**: 19 February 2026


################################################################################
## Load Packages 

# Vector of package names
packages <- c("purrr",
              "lubridate",
              "dplyr",
              "move2",
              "tidyverse",
              "amt",
              "stringr")

# Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

# Apply the function to each package name
lapply(packages, load_packages)


################################################################################
## Login to Movebank

login <- movebank_store_credentials(username = "Kyle.Smelter",
                                    password="Rayshawks5!",
                                    key="Kyle",
                                    force= T)


################################################################################
## WMU 4D - GPS Data


dat.4d <- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 4D", 
                                      login = login,
                                      removeDuplicatedTimestamps=T)


################################################################################
## WMU 3D - GPS Data

dat.3d <- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 3D", 
                                      login = login,
                                      removeDuplicatedTimestamps=T)


################################################################################
##  WMU 2D - GPS Data


dat.2d <- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 2D", 
                                      login = login,
                                      removeDuplicatedTimestamps=T)

################################################################################
##  WMU 5C - GPS Data


dat.5c <- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 5C", 
                                      login = login,
                                      removeDuplicatedTimestamps=T)

################################################################################
## Organize Movement Data

# Function to process all of the GPS data
# Create BirdID column for each bird-year dataset
process_gps_data <- function(dat) {
  as.data.frame(dat) %>%
    dplyr::mutate(
      timestamp = ymd_hms(timestamp),
      BirdID = paste0(individual_local_identifier, "_", year(timestamp)),
      BirdID = stringr::str_extract(BirdID, "^\\d+_\\d{4}$")
    ) %>%
    dplyr::filter(
      lubridate::month(timestamp) %in% 1:4
    )
}

# Apply to all regions
full_all_3d <- process_gps_data(dat.3d)
full_all_4d <- process_gps_data(dat.4d)
full_all_2d <- process_gps_data(dat.2d)
full_all_5c <- process_gps_data(dat.5c)

# Bind rows 
df <- bind_rows(full_all_2d,
                full_all_3d,
                full_all_4d,
                full_all_5c)

# Save the filtered and processed data
save(
  df,
  full_all_2d,
  full_all_3d, 
  full_all_4d,
  full_all_5c, 
  file = "Data Management/RData/Pennsylvania/GPS Data/HensGPS2025.RData"
)

################################################################################
###############################################################################X
#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'  
#' **Purpose**: This script downloads movement data associated with nesting hens for NSD calculation
#' **Last Updated**: 18 February 2026
#' **Key Changes**: This script corrects the error with parsing mover and resident GPS data
# Need to add in the mean spring movement initiation date for each year and then process the 2025 GPS data

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

# Sample of Movers and Residents from Piecewise Regression Model
all <- read_csv("Sample/Complete Sample/PA_Sample.csv")


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
      BirdID = paste0(individual_local_identifier, "_", year(timestamp))
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
                full_all_5c) %>%
  dplyr::rename("BandID" = individual_local_identifier)


# Read in files needed for subsetting data
pa.sample <- read_csv("Sample/Complete Sample/PA_Sample.csv") 

# List of movers
dispersed.sample <- pa.sample %>%
  dplyr::filter(Status == "1")

# Read in captures csv
caps <- read_csv("Data Management/Csvs/Raw/Captures/captures_pa.csv") %>%
  dplyr::rename(BandID = bandid)

# Ensure BirdID is character and trimmed
pa.sample$BirdID <- trimws(as.character(pa.sample$BirdID))
dispersed.sample$BirdID <- trimws(as.character(dispersed.sample$BirdID))

# Merge datasets
pa.sample.all <- left_join(pa.sample, dispersed.sample, by = "BirdID") %>%
  dplyr::rename(BandID = BandID.x,
                Age = Age.x,
                Year = Year.x,
                LPDV = LPDV.x,
                ChangePoint_1 = ChangePoint_1.x,
                ChangePoint_2 = ChangePoint_2.x,
                WMU = WMU.x) %>%
  dplyr::select(-BandID.y,
                -Age.y,
                -Year.y,
                -LPDV.y,
                -ChangePoint_1.y,
                -ChangePoint_2.y,
                -WMU.y)

# Final join with captures
pa.sample.ready <- left_join(pa.sample.all, caps, by = "BandID") 

# Consolidate and rename columns 
pa.sample.ready <- pa.sample.ready %>%
  dplyr::rename(Status = Status.x,
                DayOfYear = DayOfYear.x) %>%
  dplyr::select(-Status.y, -DayOfYear.y)

################################################################################
## Loop to consolidate GPS data by BirdID and Movement Status

subset_list <- list()

mean_startI_dates <- list(
  "2022" = as.Date("2022-03-17"),
  "2023" = as.Date("2023-03-19"),
  "2024" = as.Date("2024-03-17"),
  "2025" = as.Date("2025-03-19")
)

for (bird in unique(pa.sample.ready$BirdID)) {
  
  meta <- pa.sample.ready %>%
    dplyr::filter(BirdID == bird) %>%
    slice(1)
  
  status <- meta$Status
  meta_year <- meta$Year
  change_point <- as.Date(meta$ChangePoint_1)
  
  bird_data <- df %>% 
    dplyr::filter(BirdID == bird)
  
  if (status == 1) {
    
    # Movers: 21 days prior to individual change-point
    bird_subset <- bird_data %>%
      dplyr::filter(timestamp >= change_point - days(21) &
               timestamp <= change_point)
    
  } else {
    
    # Residents: 21 days prior to mean initiation date
    mean_date <- mean_startI_dates[[as.character(meta_year)]]
    
    bird_subset <- bird_data %>%
      dplyr::filter(timestamp >= mean_date - days(21) &
               timestamp <= mean_date)
  }
  
  subset_list[[bird]] <- bird_subset
}

df.filtered <- bind_rows(subset_list) %>%
  dplyr::select(BandID, BirdID, timestamp, geometry)

# Visual check of counts
birdid_counts <- df.filtered %>%
  dplyr::group_by(BirdID) %>%
  summarise(Row_Count = n()) %>%
  arrange(desc(Row_Count))

# 4 bird-years were excluded from analyses here due to their first GPS fix being after the mean for movers
setdiff(pa.sample.ready$BirdID, birdid_counts$BirdID)

################################################################################
## Output Data

# Save the filtered and processed data
save(
  df.filtered, 
  file = "Data Management/RData/Pennsylvania/GPS Data/HensGPS4HomeRangeManuscript_2025Revision.RData"
)

################################################################################
###############################################################################X
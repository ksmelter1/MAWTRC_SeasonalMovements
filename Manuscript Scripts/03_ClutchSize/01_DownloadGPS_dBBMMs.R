#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'  
#' **Purpose**: This script downloads GPS data to be used to fit dynamic Brownian Bridge Movement Models
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

# Read in sample of hens to be downloaded
all <- read_csv("Sample/Complete Sample/PA_Sample.2025Revision.csv")


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

pa.sample.ready <- all %>%
  # Rename DayOfYear to DayOfYearStart
  dplyr::rename(DayofYearStart = DayOfYear) %>%
  # Create DayofYearEnd by extracting Julian day from ChangePoint_2
  dplyr:: mutate(DayofYearEnd = yday(as.Date(ChangePoint_2)),
         # Create Diff as the difference between ChangePoint_2 and ChangePoint_1 in calendar days
         Diff = as.Date(ChangePoint_2) - as.Date(ChangePoint_1))

# Calculate mean Diff only for rows where Status == 1
mean_diff_status1 <- pa.sample.ready %>%
  dplyr::filter(Status == 1) %>%
  summarise(mean_diff = mean(as.numeric(Diff), na.rm = TRUE))
print(mean_diff_status1)


################################################################################
## Stratify GPS Data for Movers and Residents

# Set 23 March as the mean julian date
 mean_julian <- 82
 
  pa.sample.ready <- pa.sample.ready %>%
   dplyr::mutate(
     # Extract the year from ChangePoint_1 for each row
     year_val = year(as.Date(ChangePoint_1)),
     
     # Update ChangePoint_1 for Status == 0 using the correct year (as Date)
     ChangePoint_1 = if_else(Status == 0,
                             as.Date(floor(mean_julian) - 1, origin = paste0(year_val, "-01-01")),
                             as.Date(ChangePoint_1)),
     
     # Update ChangePoint_2 for Status == 0 as 14 days after ChangePoint_1
     ChangePoint_2 = if_else(Status == 0,
                             ChangePoint_1 + 14,
                             as.Date(ChangePoint_2))
   ) %>%
   dplyr::select(-year_val)  


################################################################################
## Loop to consolidate GPS data by BirdID

subset_list <- list()

# Loop through each bird
for (bird in unique(pa.sample.ready$BirdID)) {
  
  meta <- pa.sample.ready %>% dplyr::filter(BirdID == bird)
  change_point_1 <- meta$ChangePoint_1
  change_point_2 <- meta$ChangePoint_2
  
  # Skip if either change point is missing
  if (is.na(change_point_1) | is.na(change_point_2)) next
  
  # Convert to dates
  start_date <- as.Date(change_point_1)
  end_date <- as.Date(change_point_2)
  
  # Get GPS data for this bird
  bird_data <- df %>% dplyr::filter(BirdID == bird)
  
  # Filter between ChangePoint_1 and ChangePoint_2
  bird_subset <- bird_data %>%
    dplyr::filter(timestamp >= start_date & timestamp <= end_date)
  
  subset_list[[bird]] <- bird_subset
}

# Combine all into a single data frame
df.filtered <- bind_rows(subset_list) %>%
  dplyr::select(BandID, BirdID, timestamp, geometry)

# Create a summary data frame with row counts per BirdID
# Check to see if there are any birds that contain insufficient data
# 15 bird-years were removed because they had zero locations within the window
birdid_counts <- df.filtered %>%
  dplyr::group_by(BirdID) %>%
  summarise(Row_Count = n()) %>%
  arrange(desc(Row_Count)) 


################################################################################
## Output Data

# Save the filtered and processed data
save(
  df.filtered, 
  file = "Data Management/RData/Pennsylvania/GPS Data/HensGPS4DBBMMsDraft_2025Revision.RData"
)

################################################################################
###############################################################################X

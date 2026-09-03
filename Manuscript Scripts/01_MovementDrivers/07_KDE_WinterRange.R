#---
#' title: Seasonal Movements of Wild Turkeys in Pennsylvania
#' author: K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: 
#'   html_document: 
#'     toc: true
#---
#  
#' **Purpose**: This script creates kde home ranges during winter and nesting and extracts covariates
#' **Last Updated**: 18 February 2026
#' **Key Changes**: This script corrects road density calculations by placing accurate units on hr size

################################################################################
## Load Packages

library(sp)
library(adehabitatHR)
library(tidyverse)
library(mapview)
library(terra)
library(sf)
library(units)

################################################################################
## Winter Home Ranges

# Load winter GPS data
load("Data Management/RData/Pennsylvania/GPS Data/HensGPS4HomeRangeManuscript.RData")

# Format data for kde home range estimation
# 322 birds with GPS data
df.subset <- df.filtered %>%
  dplyr::mutate(long = unlist(map(geometry, 1)),
                lat = unlist(map(geometry, 2))) %>%
  dplyr::select(BirdID, timestamp, long, lat)

# Convert timestamp to POSIXct if not already
df.subset$timestamp <- as.POSIXct(df.subset$timestamp)

# Create an empty list to store utilization distributions (UDs)
out.winter <- list()

# Get unique bird IDs
ids <- unique(df.subset$BirdID)

################################################################################
## Loop to Generate Winter Home Ranges

out.winter <- list()
ids <- unique(df.subset$BirdID)

for (i in seq_along(ids)) {
  id <- ids[i]
  
  sub <- df.subset %>% filter(BirdID == id)
  
  # Convert to sf first
  sub_sf <- st_as_sf(sub, coords = c("long", "lat"), crs = 4326)
  
  # Project to UTM zone 18N for Pennsylvania (EPSG:32618)
  sub_proj <- st_transform(sub_sf, 32618)
  
  # Convert back to SpatialPointsDataFrame for kernelUD
  sub_sp <- as(sub_proj, "Spatial")
  
  tryCatch({
    kde <- kernelUD(sub_sp, h = "href", grid = 40, kern = "bivnorm", extent = 0.5)
    ver <- getverticeshr(kde, 95)
    
    # Keep only custom BirdID column
    ver@data <- ver@data %>% dplyr::select(-id)
    ver@data$ID <- id
    
    # Calculate polygon area in km²
    ver@data$Area_km2 <- sapply(slot(ver, "polygons"), function(x) slot(x, "area")) / 1e6
    
    out.winter[[id]] <- ver
  }, error = function(e) {
    message(paste("Skipping ID", id, "due to error:", e$message))
  })
}

# Combine all home ranges
all_ranges <- do.call(rbind, out.winter)

# Plot with mapview
mapview(all_ranges, zcol = "ID")

# Save output
#save(out.winter, file = "Data Management/RData/Pennsylvania/Home Range/Nesting/winterhrs.RData")


################################################################################
## Save hrs as polygons and extract landcover

# Updated Pennsylvania NLCD with Pasture and Crop classes
pa.nlcd <- terra::rast("Data Management/Rasters/NLCD/pa.nlcd.tif")

# Convert home ranges to sf objects and create 95% home ranges
# Remove 9065_2023_1 due to an unrealistic home range size
winter_sf <- lapply(out.winter, function(hr) {
  if (!is.null(hr)) ctmm::as.sf(hr, level = 0.95) else NULL
}) %>%
  Filter(Negate(is.null), .) %>%
  do.call(rbind, .)

# Create a vector of home ranges
winter_vect <- terra::vect(winter_sf)

# Extract the landcover counts within each home range using pa.nlcd
landcov_counts <- terra::extract(
  x = pa.nlcd,
  y = winter_vect,
  fun = table,
  ID = TRUE
)

# Initialize landcov_props df
landcov_props <- landcov_counts

# Save the ID column before conducting row-wise measurements
IDs <- winter_sf$ID

# Remove ID so we can do row-wise math
landcov_matrix <- landcov_props[, -1]

# Compute proportions
landcov_matrix <- sweep(landcov_matrix, 1, rowSums(landcov_matrix, na.rm = TRUE), "/")

# Add ID back
landcov_props <- cbind(ID = IDs, landcov_matrix)

# Add name column briefly before merge
landcov_props$ID <- winter_sf$ID

# Perform the join by ID (ensure both dataframes have the 'ID' column)
winter_landcov <- left_join(winter_sf, landcov_props, by = "ID") %>%
  dplyr::select(ID, everything()) %>%
  dplyr::rename("BirdID" = ID)

# Read in Birdlist csv which contains sample of birds
# Create HR_ID column
birdlist <- read_csv("Sample/Complete Sample/PA_Sample.csv")

# Create home ranges object for further analysis
# All NAs were located in columns where land cover props couldn't be estimated
dat.ready <- left_join(birdlist,winter_landcov) %>%
  drop_na(area)
summary(dat.ready)

################################################################################
## Calculate Road Density

# Load vector roads
roads.prim <- vect("Data Management/Shapefiles/Pennsylvania/roads/Pennsylvania/Primary Roads/PaStateRoads2023_10.shp")
roads.sec <- vect("Data Management/Shapefiles/Pennsylvania/roads/Pennsylvania/Secondary Roads/PaLocalRoads2023_10.shp")

# Reproject to winter.vect
roads.prim <- project(roads.prim, crs(winter_vect))
roads.sec <- project(roads.sec, crs(winter_vect))

# Intersect roads with each home range
roads.prim.hr <- terra::intersect(roads.prim, winter_vect)
roads.sec.hr <- terra::intersect(roads.sec, winter_vect)
names(roads.sec.hr)

# Convert intersected roads to sf objects
roads_prim_sf <- st_as_sf(roads.prim.hr)
roads_sec_sf  <- st_as_sf(roads.sec.hr)

# Reproject to UTM (or PA State Plane)
roads_prim_sf <- st_transform(roads_prim_sf, 26918)  
roads_sec_sf  <- st_transform(roads_sec_sf, 26918)

# Create BirdID columns in data
roads_prim_sf <- roads_prim_sf %>% 
  rename(BirdID = ID)
roads_sec_sf <- roads_sec_sf %>% 
  rename(BirdID = ID_2)

# Add length column (in meters)
roads_prim_sf$length_m <- st_length(roads_prim_sf)
roads_sec_sf$length_m  <- st_length(roads_sec_sf)

# Group and sum lengths for primary and secondary roads
prim_len_df <- roads_prim_sf %>%
  group_by(BirdID) %>%
  summarise(primary_road_m = sum(length_m, na.rm = TRUE)) %>%
  st_drop_geometry()
sec_len_df <- roads_sec_sf %>%
  group_by(BirdID) %>%
  summarise(secondary_road_m = sum(length_m, na.rm = TRUE)) %>%
  st_drop_geometry()

# Join road data
road_lengths <- full_join(prim_len_df, sec_len_df, by = "BirdID") %>%
  replace_na(list(
    primary_road_m   = set_units(0, "m"),
    secondary_road_m = set_units(0, "m")
  ))

# Join road data with main data
dat.ready <- left_join(dat.ready, road_lengths, by = c("BirdID"))

# Calculation is road length (km) / home range size (km2)
# 19 birds had no primary or secondary roads overlap their winter range
dat.ready <- dat.ready %>%
  dplyr::mutate(
    primary_road_m   = replace_na(primary_road_m, set_units(0, "m")),
    secondary_road_m = replace_na(secondary_road_m, set_units(0, "m"))
  ) %>%
  dplyr::mutate(
    primary_road_km   = set_units(primary_road_m, "km") %>% drop_units(),
    secondary_road_km = set_units(secondary_road_m, "km") %>% drop_units(),
    prim_density_km2  = primary_road_km / Area_km2,
    sec_density_km2   = secondary_road_km / Area_km2
  )

################################################################################
## Age Class

# Read in capture csv
caps <- read_csv("Data Management/Csvs/Pennsylvania/Processed/Captures/20250629_PAHenCaptures_2022_2023_2024.csv")

# Join spatial data and captures by BandID
dat.ready <- left_join(dat.ready, caps, by = "BandID") %>%
  dplyr::rename(Age = Age.x) %>%
  dplyr::select(-Age.y) %>%
  dplyr::rename(Primary = prim_density_km2,
                Secondary = sec_density_km2)

################################################################################
## Add in Weather

# Load in rasters
# All rasters are 1km x 1km despite the filenames
precip.2024 <- terra::rast("Data Management/Rasters/Weather/daymet_v4_daily_na_prcp_202400.nc")
tmin.2024   <- terra::rast("Data Management/Rasters/Weather/daymet_v4_daily_na_tmin_202400.nc")

precip.2023 <- terra::rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/prcp_2023_PA_50km.tif")
tmin.2023   <- terra::rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/tmin_2023_PA_50km.tif")

precip.2022 <- terra::rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/prcp_2022_PA_50km.tif")
tmin.2022   <- terra::rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/tmin_2022_PA_50km.tif")

# Get min/max timestamp per BirdID
bird_dates <- df.subset %>%
  group_by(BirdID) %>%
  summarise(start_date = as.Date(min(timestamp)),
            end_date   = as.Date(max(timestamp))) %>%
  ungroup()

# Function to extract Julian day
get_julian <- function(date) {
  yday(date)
}

# Initialize output lists
prcp_sum_list <- list()
tmin_mean_list <- list()

# Reproject winter_sf to match raster CRS
winter_sf_proj <- st_transform(winter_sf, crs(precip.2022))

# Match BirdIDs to winter_sf
bird_ids <- winter_sf_proj$ID

################################################################################
## Loop to extract weather data from each home range 

for (i in seq_along(bird_ids)) {
  
  bird_id <- bird_ids[i]
  cat("Processing:", bird_id, "\n")
  
  # Get date range for this bird
  row <- bird_dates %>% filter(BirdID == bird_id)
  if (nrow(row) == 0) next
  
  start_date <- row$start_date
  end_date   <- row$end_date
  years <- unique(year(start_date):year(end_date))
  
  bird_poly <- winter_sf_proj[i, ]
  
  # Initialize accumulators
  prcp_vals <- c()
  tmin_vals <- c()
  swe_vals  <- c()
  
  for (yr in years) {
    
    # Adjust start/end if bird doesn't span full year
    start_julian <- ifelse(year(start_date) == yr, yday(start_date), 1)
    end_julian   <- ifelse(year(end_date) == yr, yday(end_date), 
                           ifelse(leap_year(yr), 366, 365))
    
    # Select raster stacks for this year
    prcp_stack <- switch(as.character(yr),
                         "2022" = precip.2022,
                         "2023" = precip.2023,
                         "2024" = precip.2024)
    
    tmin_stack <- switch(as.character(yr),
                         "2022" = tmin.2022,
                         "2023" = tmin.2023,
                         "2024" = tmin.2024)
    
    # Subset by Julian day
    prcp_subset <- prcp_stack[[start_julian:end_julian]]
    tmin_subset <- tmin_stack[[start_julian:end_julian]]
    
    # Extract and aggregate over bird polygon
    prcp_extracted <- terra::extract(prcp_subset, vect(bird_poly), fun = mean, na.rm = TRUE)[1, -1]
    tmin_extracted <- terra::extract(tmin_subset, vect(bird_poly), fun = mean, na.rm = TRUE)[1, -1]
    
    # Accumulate values
    prcp_vals <- c(prcp_vals, unlist(prcp_extracted))
    tmin_vals <- c(tmin_vals, unlist(tmin_extracted))
  }
  
  # Save results
  prcp_sum_list[[bird_id]]  <- sum(prcp_vals, na.rm = TRUE)
  tmin_mean_list[[bird_id]] <- mean(tmin_vals, na.rm = TRUE)
}

# Convert to dataframe
weather_df <- tibble(
  BirdID = names(prcp_sum_list),
  prcp_mean_mm = unlist(prcp_sum_list),
  tmin_avg_degC = unlist(tmin_mean_list)
)

# Convert to dataframe
weather_df <- tibble(
  BirdID = names(prcp_sum_list),
  prcp_mean_mm = unlist(prcp_sum_list),
  tmin_avg_degC = unlist(tmin_mean_list)
)

# Join with final dataset
dat.ready <- left_join(dat.ready, weather_df, by = "BirdID")

################################################################################
## Add in a covariate to account for if a bird nested the previous year

# Nest table
nests <- read_csv("Data Management/Csvs/Raw/Nests/20250629_CleanedNests_2022_2023_2024.csv") %>%
  dplyr::filter(SurveyYr != "2025")

# Add a 4 to the end of BirdIDs missing that
dat.ready <- dat.ready %>%
  dplyr::mutate(
    BirdID = if_else(str_ends(BirdID, "202"), str_c(BirdID, "4"), BirdID)
  )

# Create year and birdid columns in nests_processed
nests_processed <- nests %>%
  dplyr::mutate(
    Year = str_extract(NestID, "(?<=_)\\d{4}(?=_)"),
    BirdID = str_extract(NestID, "^.*(?=_[^_]+$)")
  ) 

# Create BirdID + Year from nests
nests_processed <- nests_processed %>%
  distinct(BirdID) %>%
  dplyr::mutate(
    ID = str_extract(BirdID, "^\\d+"),                     
    Year = as.integer(str_extract(BirdID, "\\d{4}$")),     
    BirdID_next = paste0(ID, "_", Year + 1)                
  )

# Create a lookup table: birds who nested in the previous year
nested_lookup <- nests_processed %>%
  transmute(BirdID = BirdID_next, nested_last_year = 1)

# Merge with dat.ready
dat.ready <- dat.ready %>%
  left_join(nested_lookup, by = "BirdID") %>%
  dplyr::mutate(nested_last_year = replace_na(nested_last_year, 0)) %>%
  dplyr::rename(WMU = WMU.x) %>%
  dplyr::select(-WMU.y)

################################################################################
## Output Data

# Save the filtered and processed data
save(
  dat.ready,
  file = "Data Management/RData/Pennsylvania/01_MovementFactors_ManuscriptUpdated.RData"
)

################################################################################
## Format Data for Journal

# Mean spring movement start dates per year
mean_startI_dates <- list(
  "2022" = as.Date("2022-03-17"),
  "2023" = as.Date("2023-03-19"),
  "2024" = as.Date("2024-03-17")
)
 
# Create dataframe
mean_dates_df <- tibble::tibble(
  Year = as.numeric(names(mean_startI_dates)),
  mean_date = as.Date(unlist(mean_startI_dates))
)

# Sample size for Manuscript
birdlist <- dplyr::semi_join(birdlist, df.subset, by = "BirdID")

# Add a 4 to the end of BirdIDs missing that
birdlist <- birdlist %>%
  dplyr::mutate(
    BirdID = if_else(str_ends(BirdID, "202"), str_c(BirdID, "4"), BirdID)
  )

# Consolidate columns 
birdlist <- birdlist %>%
  dplyr::select(-DayOfYear,
                -BandID)

# Correct change points for residents
birdlist <- birdlist %>%
  dplyr::left_join(mean_dates_df, by = "Year") %>%
  dplyr::mutate(
    ChangePoint_1 = dplyr::if_else(Status == 0, mean_date, ChangePoint_1),
    ChangePoint_2 = dplyr::if_else(Status == 0, ChangePoint_1 + 17, ChangePoint_2)
  ) %>%
  dplyr::select(-mean_date)

# Assign random numbers to BirdID column to anonymise data
set.seed(123)
unique_ids <- unique(birdlist$BirdID)
random_ids <- sample(100000:999999, length(unique_ids), replace = FALSE)
id_map <- setNames(random_ids, unique_ids)
birdlist$BirdID <- id_map[birdlist$BirdID]

# Export csv
 write_csv(birdlist, "Manuscript/Ecography/Data/01_BirdData.csv")

# Probability of Movement Dataframe
dat.ready.journal <- dat.ready %>%
  dplyr::rename("Precip" = prcp_mean_mm,     # Rename columns
                "Tmin" = tmin_avg_degC,
                "Area" = Area_km2,
                "Nested_LastYr" = nested_last_year) %>%
                                  st_drop_geometry() %>%   # Remove geometry
                                                    dplyr::select(-area,    # Remove unnecessary columns
                                                    -secondary_road_m,
                                                    -secondary_road_km,
                                                    -primary_road_m,
                                                    -primary_road_km,
                                                    -geometry,
                                                    -DayOfYear,
                                                    -Lat,
                                                    -Long,
                                                    -BandID,
                                                    -DayOfYear,
                                                    -CaptureDate,
                                                    -Other,
                                                    -ChangePoint_2) %>%
                                                                dplyr::select(BirdID,  # Finalize data arrangement in dataframe
                                                                              Age,
                                                                              Year,
                                                                              WMU,
                                                                              LPDV,
                                                                              Status,
                                                                              Area,
                                                                              Developed,
                                                                              Hardwood,
                                                                              Conifer,
                                                                              Pasture,
                                                                              Crop,
                                                                              Primary,
                                                                              Secondary,
                                                                              Precip,
                                                                              Tmin,
                                                                              everything())

# Replace Change_Point1 for Bird_Years where status = 0 to the mean for movers that year
dat.ready.journal <- dat.ready.journal %>%
  dplyr::left_join(mean_dates_df, by = "Year") %>%
  dplyr::mutate(
    ChangePoint_1 = dplyr::if_else(Status == 0, mean_date, ChangePoint_1)
  ) %>%
  dplyr::select(-mean_date)

# Assign random numbers to BirdID column to anonymise data
set.seed(123)
unique_ids <- unique(dat.ready.journal$BirdID)
random_ids <- sample(100000:999999, length(unique_ids), replace = FALSE)
id_map <- setNames(random_ids, unique_ids)
dat.ready.journal$BirdID <- id_map[dat.ready.journal$BirdID]

# Export csv
write_csv(dat.ready.journal, "Manuscript/Ecography/Data/03_ProbabilityofMover.csv")

# Home Range Estimation GPS Data
df.subset.journal <- df.subset %>%
  dplyr::mutate(
    BirdID = if_else(str_ends(BirdID, "202"), str_c(BirdID, "4"), BirdID)
  ) %>%
  dplyr::select(-lat, -long)

# Assign random numbers to BirdID column to anonymise data
set.seed(123)
unique_ids <- unique(df.subset.journal$BirdID)
random_ids <- sample(100000:999999, length(unique_ids), replace = FALSE)
id_map <- setNames(random_ids, unique_ids)
df.subset.journal$BirdID <- id_map[df.subset.journal$BirdID]

# Export csv
write_csv(df.subset.journal, "Manuscript/Ecography/Data/02_GPS_WinterRange.csv")

################################################################################
###############################################################################X
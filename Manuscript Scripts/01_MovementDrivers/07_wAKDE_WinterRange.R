#---
#' title: Seasonal Movements of Wild Turkeys in Pennsylvania
#' author: K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   html_document:
#'     toc: true
#---

# Purpose: Estimate weighted AKDE (wAKDE) winter home ranges for GPS-tagged
# wild turkey hens and extract land cover, road density covariates, weather, and individual variables.
#
# Last Updated: 27 June 2026

################################################################################
## Load Packages

library(tigris)
library(ctmm)
library(move)
library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(units)

################################################################################
## Load GPS Data

# Read in the cleaned GPS dataset 
load("Data Management/RData/Pennsylvania/GPS Data/HensGPS4HomeRangeManuscript_2025Revision.RData")

################################################################################
## Prepare GPS Data

# Extract longitude and latitude from the sf geometry column
# The geometry column stores point coordinates rather than separate x/y columns
df.subset <- df.filtered %>%
  dplyr::mutate(
    long = unlist(map(geometry, 1)),
    lat  = unlist(map(geometry, 2))
  ) %>%
  dplyr::select(BirdID, timestamp, long, lat)

# Set timezone as America/New York
df.subset$timestamp <- as.POSIXct(
  df.subset$timestamp,
  tz = "America/New_York"
)

################################################################################
## Fit weighted AKDE Home Ranges

# 1. Loop through GPS data for each bird-year
# 2. Fit movement models for each bird-year
# 3. Fit a wAKDE to the best supported model
# 4. Store outputs and collect calculations such as area needed for road density

# Lists to store outputs
out.akde <- list()
out.poly <- list()

# Generate a vector of bird-years to fit wAKDEs too
ids <- unique(df.subset$BirdID) 

# Loop through each unique bird-year
for(i in seq_along(ids)){
  
  id <- ids[i]
  
  # Display current BirdID and progress
  message(
    "\n========================================\n",
    "Processing BirdID: ", id,
    " (", i, " of ", length(ids), ")\n",
    "========================================"
  )
  
  # Subset the GPS data to include only locations from the current bird
  sub <- df.subset %>%
    dplyr::filter(BirdID == id) %>%
    arrange(timestamp) %>%
    distinct(timestamp, .keep_all = TRUE)
  
  # Require minimum sample size
  # Remove bird-years with less than 30 locations
  if(nrow(sub) < 30){
    message("Skipping ", id, ": fewer than 30 locations.")
    next
  }
  
  tryCatch({

# Convert output to a telemetry data which is needed for fitting a wAKDE
telem <- as.telemetry(
      data.frame(
        timestamp = sub$timestamp,
        longitude = sub$long,
        latitude  = sub$lat
      )
    )

# Fit a movement model to each individual    
GUESS <- ctmm.guess(
      telem,
      interactive = FALSE
    )
    
# Use model selection to pick the best model    
MODEL <- ctmm.select(
      telem,
      GUESS
    )

# Fit the wAKDE to the best model    
WAKDE <- akde(
      telem,
      MODEL,
      weights = TRUE
    )

# Convert the wAKDE to an sf polygon   
HR95 <- as.sf(
      WAKDE,
      level = 0.95
    )

# Create a vector for each BirdID    
HR95$BirdID <- id

# Reproject for area calculation    
HR95 <- st_transform(
      HR95,
      32618
    )
    
# Calculate the area of each UD    
HR95$Area_km2 <- as.numeric(st_area(HR95)) / 1e6
  
# Save the outputs    
out.akde[[id]] <- WAKDE
out.poly[[id]] <- HR95
    },
  error = function(e){
    
    message("Skipping ", id, ": ", e$message)
    
  })
  
}

# Combine Home Range Polygons
winter_sf <- do.call(rbind, out.poly)


################################################################################
## Output a PDF for Each BirdID UD

# Get unique BirdIDs
birds <- unique(winter_sf$BirdID)

# Open PDF device
pdf("Winter_WAKDE_Home_Ranges_OnePagePerBird.pdf", width = 10, height = 8)

for (b in birds) {
  
  # Subset data for one bird
  dat <- winter_sf %>%
    filter(BirdID == b)
  
  # Create plot
  p <- ggplot(dat) +
    geom_sf(fill = "steelblue", alpha = 0.4, color = "black", linewidth = 0.3) +
    theme_minimal() +
    labs(
      title = paste("Winter wAKDE Home Range - Bird", b),
      subtitle = "95% utilization distribution",
      x = NULL,
      y = NULL
    )
  
  print(p)
}

dev.off()

# Collapse winter_sf
winter_sf_clean <- winter_sf %>%
  dplyr::group_by(BirdID) %>%
  dplyr::summarise(
    Area_km2 = sum(Area_km2),
    do_union = TRUE
  )

################################################################################
## Output Winter Range Data

# Save outputs 
save(
  out.akde,
  winter_sf_clean,
  file = "Data Management/RData/Pennsylvania/Home Range/Winter/wAKDE_home_ranges.RData"
)

################################################################################
## Extract the Proportion Land Cover from each wAKDE

load ("Data Management/RData/Pennsylvania/Home Range/Winter/wAKDE_home_ranges.RData")

# Read in pa.nlcd
pa.nlcd <- terra::rast(
  "Data Management/Rasters/NLCD/pa.nlcd.tif"
)

# Create a vector of winter ranges
winter_vect <- terra::vect(winter_sf_clean)

# Reproject polygons to the raster CRS
winter_vect <- terra::project(
  winter_vect,
  crs(pa.nlcd)
)

# Extract land cover counts from each UD
landcov_counts <- terra::extract(
  x = pa.nlcd,
  y = winter_vect,
  fun = table,
  ID = TRUE
)

# Add BirdID using the ID column
landcov_counts$BirdID <- winter_sf_clean$BirdID[landcov_counts$ID]

# Move BirdID to the front
landcov_counts <- landcov_counts %>%
  dplyr::relocate(BirdID, .before = ID)

# Calculate proportion of each land cover type within a winter range
landcov_props <- landcov_counts %>%
  dplyr::group_by(BirdID, ID) %>%
  dplyr::mutate(
    Total = sum(count),
    Proportion = count / Total
  ) %>%
  ungroup()

# Convert to wide format
landcov_wide <- landcov_counts %>%
  dplyr::group_by(BirdID, Class) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  ) %>%
  dplyr::group_by(BirdID) %>%
  dplyr::mutate(
    Proportion = count / sum(count)
  ) %>%
  ungroup() %>%
  dplyr::select(BirdID, Class, Proportion) %>%
  pivot_wider(
    names_from = Class,
    values_from = Proportion,
    values_fill = list(Proportion = 0)
  )

# Create winter_landcov object
winter_landcov <- winter_sf_clean %>%
  dplyr::left_join(landcov_wide, by = "BirdID")

# Read in the birdlist csv
birdlist <- read_csv(
  "Sample/Complete Sample/Manuscript/PA_Sample.2025Revision.csv"
)

# Create dataset with the bird-years and winter ranges
# The pasture NA filter removes 6 bird-years in which home ranges weren't estimated that existed in birdlist 
# 4 of those bird-years were from the download script
dat.ready <- left_join(
  birdlist,
  winter_landcov,
  by = "BirdID"
) %>% dplyr::filter(Pasture != "NA")

# Output data
save(dat.ready,
     df.subset,
     file = "Data Management/RData/Pennsylvania/Home Range/Winter/Landcover_home_ranges.RData")

################################################################################
## Extract Road Density Information from each UD

load("Data Management/RData/Pennsylvania/Home Range/Winter/Landcover_home_ranges.RData")

# Read primary and secondary road data from PASDA
roads.prim <- st_read(
  "Data Management/Shapefiles/Pennsylvania/roads/Pennsylvania/Primary Roads/PaStateRoads2023_10.shp"
)

roads.sec <- st_read(
  "Data Management/Shapefiles/Pennsylvania/roads/Pennsylvania/Secondary Roads/PaLocalRoads2023_10.shp"
)

# Match CRS to winter home ranges
roads.prim <- st_transform(roads.prim, st_crs(winter_sf_clean))
roads.sec  <- st_transform(roads.sec,  st_crs(winter_sf_clean))

# Intersect roads within winter ranges
roads_prim_hr <- st_intersection(roads.prim, winter_sf_clean)
roads_sec_hr  <- st_intersection(roads.sec,  winter_sf_clean)

# Compute road lengths in meters
roads_prim_hr <- roads_prim_hr %>%
  dplyr::mutate(length_m = as.numeric(st_length(geometry)))

roads_sec_hr <- roads_sec_hr %>%
  dplyr::mutate(length_m = as.numeric(st_length(geometry)))

# Group by each BirdID and convert road lengths to km
prim_len_df <- roads_prim_hr %>%
  st_drop_geometry() %>%
  dplyr::group_by(BirdID) %>%
  summarise(primary_road_km = sum(length_m) / 1000)

sec_len_df <- roads_sec_hr %>%
  st_drop_geometry() %>%
  dplyr::group_by(BirdID) %>%
  summarise(secondary_road_km = sum(length_m) / 1000)

# Create a road lengths object that contains primary and secondary roads
road_lengths <- full_join(prim_len_df, sec_len_df, by = "BirdID") %>%
  dplyr::mutate(
    primary_road_km   = replace_na(primary_road_km, 0),
    secondary_road_km = replace_na(secondary_road_km, 0)
  )

# Join with main dataset
# 26 bird-years contained winter ranges that intersected zero primary and secondary roads
# That is where the NAs come from for lengths
dat.ready <- dat.ready %>%
  left_join(road_lengths, by = "BirdID")

# Calculate road density and fill NAs with zero
# Remove road length columns
dat.ready <- dat.ready %>%
  dplyr::mutate(
    primary_road_km   = replace_na(primary_road_km, 0),
    secondary_road_km = replace_na(secondary_road_km, 0)
  ) %>%
  dplyr::mutate(
    Primary   = primary_road_km / Area_km2,
    Secondary = secondary_road_km / Area_km2
  ) %>%
  dplyr::select(
    -primary_road_km,
    -secondary_road_km
  )

################################################################################
## Add Weather Covariates

# 1. Ensure that all rasters are set to the same CRS
# 2. Ensure that all rasters have the same extent

# Reference CRS
crs_string <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +
+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Read in temperature and precipitation rasters for each year
precip.2022 <- rast("Data Management/Rasters/Weather/PA_daymet_2022_2025/prcp_2022_PA_50km.tif")
precip.2023 <- rast("Data Management/Rasters/Weather/PA_daymet_2022_2025/prcp_2023_PA_50km.tif")
precip.2024 <- rast("Data Management/Rasters/Weather/PA_daymet_2022_2025/prcp_2024_PA_50km.tif")
precip.2025 <- rast("Data Management/Rasters/Weather/PA_daymet_2022_2025/prcp_2025_PA_50km.tif")
tmin.2022 <- rast("Data Management/Rasters/Weather/PA_daymet_2022_2025/tmin_2022_PA_50km.tif")
tmin.2023 <- rast("Data Management/Rasters/Weather/PA_daymet_2022_2025/tmin_2023_PA_50km.tif")
tmin.2024 <- rast("Data Management/Rasters/Weather/PA_daymet_2022_2025/tmin_2024_PA_50km.tif")
tmin.2025 <- rast("Data Management/Rasters/Weather/PA_daymet_2022_2025/tmin_2025_PA_50km.tif")

# Create a list of rasters
rast_list <- list(
  precip.2022, precip.2023, precip.2024, precip.2025,
  tmin.2022, tmin.2023, tmin.2024, tmin.2025
)

# Function to ensure CRS's match for each raster
rast_list <- lapply(rast_list, function(r){
  crs(r) <- crs_string
  r
})

# Apply function
precip.2022 <- rast_list[[1]]
precip.2023 <- rast_list[[2]]
precip.2024 <- rast_list[[3]]
precip.2025 <- rast_list[[4]]
tmin.2022   <- rast_list[[5]]
tmin.2023   <- rast_list[[6]]
tmin.2024   <- rast_list[[7]]
tmin.2025   <- rast_list[[8]]


# Obtain a Pennsylvania outline and buffer that outline 50km
st <- tigris::states(cb = TRUE)
st <- st_transform(st, crs_string)
pa <- subset(st, NAME == "Pennsylvania")
pa_50km <- st_buffer(pa, 50000)
pa_vect <- terra::vect(pa_50km)

# Set precip.2022 as the reference raster
ref_raster <- precip.2022

# This function projects each raster to the reference crs
# Resamples the rasters
# Crops the rasters to the reference extent
# Masks the rasters to the proper extent
align_to_ref <- function(r, ref, mask_poly){
  r <- terra::project(r, terra::crs(ref))
  r <- terra::resample(r, ref, method = "bilinear")
  r <- terra::crop(r, ref)
  r <- terra::mask(r, mask_poly)
  return(r)
}

precip.2023 <- align_to_ref(precip.2023, ref_raster, pa_vect)
precip.2024 <- align_to_ref(precip.2024, ref_raster, pa_vect)
precip.2025 <- align_to_ref(precip.2025, ref_raster, pa_vect)

tmin.2023 <- align_to_ref(tmin.2023, ref_raster, pa_vect)
tmin.2024 <- align_to_ref(tmin.2024, ref_raster, pa_vect)
tmin.2025 <- align_to_ref(tmin.2025, ref_raster, pa_vect)

# Verify
compareGeom(precip.2022, precip.2023, stopOnError = FALSE)
compareGeom(precip.2022, precip.2024, stopOnError = FALSE)
compareGeom(precip.2022, precip.2025, stopOnError = FALSE)
compareGeom(precip.2022, tmin.2023, stopOnError = FALSE)
compareGeom(precip.2022, tmin.2024, stopOnError = FALSE)
compareGeom(precip.2022, tmin.2025, stopOnError = FALSE)

# A check to see time intervals for each GPS marked bird
bird_dates <- df.subset %>%
  group_by(BirdID) %>%
  summarise(
    start_date = as.Date(min(timestamp)),
    end_date   = as.Date(max(timestamp)),
    .groups = "drop"
  )

# Create a vector of bird-years
bird_ids <- winter_sf_clean$BirdID
setdiff(bird_dates$BirdID, bird_ids)

################################################################################
## Extract weather variables

# 1. Iterates through each BirdID
# 2. Extracts the temporal range of GPS tracking for that individual
# 3. Subsets daily climate rasters (precipitation + minimum temperature)
# 4. Extracts mean climate conditions within each AKDE home range
# 5. Aggregates values into one seasonal summary per bird

# Initialize lists to store data
prcp_sum_list  <- list()
tmin_mean_list <- list()

for(i in seq_along(bird_ids)){
  
  bird_id <- bird_ids[i]
  
  # Print progress message to monitor run time and identify failures
  message(
    "Weather extraction: ",
    bird_id,
    " (",
    i,
    " of ",
    length(bird_ids),
    ")"
  )
  
  # Subset table containing first and last GPS fix for each BirdID
  row <- bird_dates %>%
    filter(BirdID == bird_id)
  
  # Skip if no temporal information is available
  if(nrow(row)==0) next
  
  # Extract start and end dates of tracking period
  start_date <- row$start_date
  end_date   <- row$end_date
  
  # Creates a vector of all years spanned by the bird's movement data
  years <- unique(year(start_date):year(end_date))
  
  # Subset corresponding home-range polygon
  # Overlay winter_sf ploygons with weather rasters
  bird_poly <- winter_sf_clean[i,]
  
  # Store tmin and precip values for each bird-year
  prcp_vals <- c()
  tmin_vals <- c()
  
  # Loop through years
  for(yr in years){
    
    # Start day = first day bird is tracked in that year OR Jan 1
    start_julian <-
      ifelse(year(start_date)==yr,
             yday(start_date),
             1)
    
    # End day = last tracked day OR end of year (365/366)
    end_julian <-
      ifelse(year(end_date)==yr,
             yday(end_date),
             ifelse(leap_year(yr),366,365))
    
    # Select precip and tmin raster stacks for each year
    prcp_stack <- switch(
      as.character(yr),
      "2022" = precip.2022,
      "2023" = precip.2023,
      "2024" = precip.2024,
      "2025" = precip.2025
    )
    
    tmin_stack <- switch(
      as.character(yr),
      "2022" = tmin.2022,
      "2023" = tmin.2023,
      "2024" = tmin.2024,
      "2025" = tmin.2025
    )
    
    # Reduce raster to only days where birds were tracked
    prcp_subset <- prcp_stack[[start_julian:end_julian]]
    tmin_subset <- tmin_stack[[start_julian:end_julian]]
    
    # Extract mean precip and tmin across all raster cells inside wAKDE polygon
    prcp_extract <-
      terra::extract(
        prcp_subset,
        terra::vect(bird_poly),
        fun = mean,
        na.rm = TRUE
      )[1,-1]
    
    tmin_extract <-
      terra::extract(
        tmin_subset,
        terra::vect(bird_poly),
        fun = mean,
        na.rm = TRUE
      )[1,-1]
    
    prcp_vals <- c(prcp_vals, unlist(prcp_extract))
    tmin_vals <- c(tmin_vals, unlist(tmin_extract))
    
  }
  
  # Append values across all years and time steps
  prcp_sum_list[[bird_id]]  <- sum(prcp_vals, na.rm = TRUE)
  tmin_mean_list[[bird_id]] <- mean(tmin_vals, na.rm = TRUE)
  
}

################################################################################
## Join Weather

# Create summary table
weather_df <- tibble(
  BirdID = names(prcp_sum_list),
  prcp_mean_mm = unlist(prcp_sum_list),
  tmin_avg_degC = unlist(tmin_mean_list)
)

# Join with dat.ready
dat.ready <- left_join(
  dat.ready,
  weather_df,
  by = "BirdID"
)

################################################################################
## Nested the Prior Year Covariate

# Read in nesting data
nests <- read_csv(
  "Data Management/Csvs/Pennsylvania/Processed/Nests/Nests/Pennsylvania/20260325_CleanedNests_PA_2022_2023_2024_2025.csv"
)

# Build bird-year nesting history (DO NOT collapse to single BirdID)
nests_clean <- nests %>%
  dplyr::mutate(
    Year = as.integer(str_extract(NestID, "(?<=_)\\d{4}(?=_)")),
    IndividualID = str_extract(NestID, "^[^_]+")
  ) %>%
  dplyr::distinct(IndividualID, Year) %>%
  dplyr::mutate(nested_this_year = 1)

# Create full bird-year grid (important for 0s)
bird_years <- expand.grid(
  IndividualID = unique(nests_clean$IndividualID),
  Year = sort(unique(nests_clean$Year))
)

# Join nesting info
nest_history <- bird_years %>%
  dplyr::left_join(nests_clean, by = c("IndividualID", "Year")) %>%
  dplyr::mutate(nested_this_year = tidyr::replace_na(nested_this_year, 0))

# Create lag (nested last year)
nest_history <- nest_history %>%
  dplyr::arrange(IndividualID, Year) %>%
  dplyr::group_by(IndividualID) %>%
  dplyr::mutate(
    nested_last_year = dplyr::lag(nested_this_year, default = 0)
  ) %>%
  dplyr::ungroup()

# Join to main dataset
dat.ready <- dat.ready %>%
  dplyr::mutate(
    IndividualID = stringr::str_extract(BirdID, "^[^_]+"),
    Year = as.integer(stringr::str_extract(BirdID, "\\d{4}$"))
  ) %>%
  dplyr::left_join(
    nest_history %>% dplyr::select(IndividualID, Year, nested_last_year),
    by = c("IndividualID", "Year")
  ) %>%
  dplyr::mutate(
    nested_last_year = tidyr::replace_na(nested_last_year, 0)
  )

################################################################################
## Disease 

# Read in LPDV dataset
disease <- read_csv("Data Management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/DiseaseStatus.csv")

# Bring in LPDV and remove potential duplicates
dat.ready <- left_join(dat.ready,disease, by = "BandID") %>%
  dplyr::select(-LPDV.x) %>%
  dplyr::rename(LPDV = LPDV.y) %>%
  distinct(BirdID, .keep_all = T) 
  

################################################################################
## Save Final Dataset

# Output data 
save(
  dat.ready,
  file = "Data Management/RData/Pennsylvania/01_MovementFactors_wAKDE.RData"
)

################################################################################
###############################################################################X
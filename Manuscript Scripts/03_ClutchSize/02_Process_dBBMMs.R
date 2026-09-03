#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'  
#' **Purpose**: This script fits dynamic Brownian Bridge Movement Models
#' **Last Updated**: 19 February 2026

################################################################################
## Load Packages and Data

library(move)
library(sf)
library(raster)
library(lubridate)
library(dplyr)
library(ggplot2)
library(terra)
library(units)
library(tidyr)
library(tidyverse)

# GPS Data for dBBMM estimation 
load("Data Management/RData/Pennsylvania/GPS Data/HensGPS4DBBMMsDraft_2025Revision.RData") 


################################################################################
## Create dynamic Brownian Bridge Movement Models for each Bird-Year Dataset

# Convert to sf object
df.filtered <- df.filtered %>%
  st_as_sf()

# dBBMM wouldn't converge for these birds due too few locations
# Need to be removed here otherwise loop won't work
df.filtered <- df.filtered %>%
  dplyr::filter(BirdID != "8464_2023") %>%
  dplyr::filter(BirdID !="9081_2024")

# Initialize list to store UDs
dbbmm.list <- list()

# Loop through each unique BirdID
bird_ids <- unique(df.filtered$BirdID)

# Ensure the sf object is in a projected CRS (UTM zone 18N here as example)
if (st_is_longlat(df.filtered)) {
  message("Reprojecting coordinates to UTM Zone 18N (EPSG:32618)...")
  df.filtered <- st_transform(df.filtered, crs = 32618)
}

# Initialize list to store dBBMM polygons and failed bird IDs
dbbmm.list <- list()
failed_birds <- c()

# Loop through each unique BirdID
bird_ids <- unique(df.filtered$BirdID)

for (i in seq_along(bird_ids)) {
  
  bird_id <- bird_ids[i]
  
  message(paste("Processing bird:", bird_id, "-", i, "of", length(bird_ids)))
  
  # Subset GPS data for this bird and ensure it's ordered
  bird_data <- df.filtered %>%
    filter(BirdID == bird_id) %>%
    arrange(timestamp)
  
  # Skip birds with too few points
  if (nrow(bird_data) < 20) {
    warning(paste("Skipping", bird_id, "- fewer than 20 GPS fixes"))
    failed_birds <- c(failed_birds, bird_id)
    next
  }
  
  # Convert to move object
  tryCatch({
    t_turkeygps <- move(
      x = st_coordinates(bird_data)[,1],
      y = st_coordinates(bird_data)[,2],
      time = bird_data$timestamp,
      data = as.data.frame(bird_data),
      proj = st_crs(bird_data)$proj4string,
      animal = bird_id
    )
    
    # Compute dynamic Brownian Bridge
    turk_dBBMM <- brownian.bridge.dyn(
      object = t_turkeygps,
      raster = 30,           # resolution in meters
      location.error = 29,   # GPS error in meters
      margin = 5,
      window.size = 15,
      ext = 1.5
    )
    
    # Extract 95% utilization distribution contour as polygon
    turkey_UD <- raster2contour(turk_dBBMM, level = 0.99)
    dBBMM_line <- st_as_sf(turkey_UD)
    dBBMM_poly <- st_cast(dBBMM_line, "POLYGON")
    dBBMM_poly$BirdID <- bird_id
    
    # Save to output list
    dbbmm.list[[bird_id]] <- dBBMM_poly
    
  }, error = function(e) {
    warning(paste("Failed to process bird", bird_id, "-", e$message))
    failed_birds <- c(failed_birds, bird_id)
  })
}

# Combine all into a single sf object
if (length(dbbmm.list) > 0) {
  dbbmm.sf <- do.call(rbind, dbbmm.list)
  message("✅ dBBMM processing complete.")
} else {
  dbbmm.sf <- NULL
  message("⚠️ No successful dBBMMs created.")
}

# View or export failed birds
if (length(failed_birds) > 0) {
  message("Birds that failed or had too few points:")
  print(failed_birds)
}

# Combine all into one sf object
dbbmm.sf <- do.call(rbind, dbbmm.list)


dbbmm.sf <- st_make_valid(dbbmm.sf)

dbbmm.sf$Area_km2 <- as.numeric(st_area(dbbmm.sf)) / 1e6

################################################################################
## Create a PDF for each unique dBBMM UD

# File path for multi-page PDF
pdf_file <- "All_Birds_dBBMM_UDs.pdf"

# Create output directory if needed
if (!dir.exists("output")) dir.create("output")

# Open PDF device
pdf(file = pdf_file, width = 6, height = 6)  

# Loop through each BirdID and plot
for (bird_id in unique(dbbmm.sf$BirdID)) {
  
  # Subset dBBMM polygon
  bird_ud <- dbbmm.sf %>% filter(BirdID == bird_id)
  
  # Generate ggplot
  p <- ggplot() +
    geom_sf(data = bird_ud, fill = "lightblue", color = "black", alpha = 0.5) +
    labs(
      title = paste("99% Utilization Distribution - Bird", bird_id),
      caption = paste("Generated on", Sys.Date())
    ) +
    theme_minimal()
  
  print(p)
  
  message("Added BirdID", bird_id, "to PDF")
}

dev.off()

message("✅ Multi-page PDF saved to: ", pdf_file)


################################################################################
## Extract Proportion Land Cover Type and Density of Primary and Secondary Roads

# Read in pa.nlcd
pa.nlcd <- terra::rast(
  "Data Management/Rasters/NLCD/pa.nlcd.tif"
)

# Create a vector of winter ranges
winter_vect <- terra::vect(dbbmm.sf)

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
landcov_counts$BirdID <- dbbmm.sf$BirdID[landcov_counts$ID]

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
dbbmm_landcov <- dbbmm.sf %>%
  dplyr::left_join(landcov_wide, by = "BirdID")

# Read in the birdlist csv
birdlist <- read_csv(
  "Sample/Complete Sample/Manuscript/PA_Sample.2025Revision.csv"
)

# Create dataset with the bird-years and winter ranges
# The pasture NA filter removes 
dat.ready <- left_join(
  birdlist,
  dbbmm_landcov,
  by = "BirdID"
) %>%
  dplyr::filter(Pasture != "NA") %>%
  dplyr::select(-LPDV)

# Output data
save(dat.ready,
     file = "Data Management/RData/Pennsylvania/Home Range/SeasonalMovements/Landcover_MovementPaths.RData")


################################################################################
## Calculate Road Density within each UD

# Read primary and secondary road data from PASDA
roads.prim <- st_read(
  "Data Management/Shapefiles/Pennsylvania/roads/Pennsylvania/Primary Roads/PaStateRoads2023_10.shp"
)

roads.sec <- st_read(
  "Data Management/Shapefiles/Pennsylvania/roads/Pennsylvania/Secondary Roads/PaLocalRoads2023_10.shp"
)

# Match CRS to winter home ranges
roads.prim <- st_transform(roads.prim, st_crs(dbbmm.sf))
roads.sec  <- st_transform(roads.sec,  st_crs(dbbmm.sf))

# Intersect roads within winter ranges
roads_prim_hr <- st_intersection(roads.prim, dbbmm.sf)
roads_sec_hr  <- st_intersection(roads.sec,  dbbmm.sf)

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
## Add in LPDV

# Read in disease data
disease <- read_csv("Data Management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/DiseaseStatus.csv")

# Join disease data to dat.ready by BandID
dat.ready <- dat.ready %>%
  left_join(disease, by = "BandID") %>%
  distinct(BirdID, .keep_all = T)


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

# Get min/max timestamp per BirdID
bird_dates <- df.filtered %>%
  group_by(BirdID) %>%
  summarise(start_date = as.Date(min(timestamp)),
            end_date   = as.Date(max(timestamp))) %>%
  ungroup()

# Function to extract Julian day
get_julian <- function(date) {
  yday(date)
}

################################################################################
## Extract weather variables for dBBMM ranges

# Initialize lists
prcp_sum_list  <- list()
tmin_mean_list <- list()

# Ensure correct BirdID order
bird_ids <- dbbmm.sf$BirdID

for(i in seq_along(bird_ids)){
  
  bird_id <- bird_ids[i]
  
  message(
    "dBBMM Weather extraction: ",
    bird_id,
    " (",
    i,
    " of ",
    length(bird_ids),
    ")"
  )
  
  # Extract tracking dates
  row <- bird_dates %>%
    filter(BirdID == bird_id)
  
  if(nrow(row) == 0) next
  
  start_date <- row$start_date
  end_date   <- row$end_date
  
  # Years included in tracking period
  years <- unique(year(start_date):year(end_date))
  
  # dBBMM home range polygon
  bird_poly <- dbbmm.sf[i,]
  
  # Storage vectors
  prcp_vals <- c()
  tmin_vals <- c()
  
  
  for(yr in years){
    
    # Julian day limits
    start_julian <-
      ifelse(year(start_date) == yr,
             yday(start_date),
             1)
    
    end_julian <-
      ifelse(year(end_date) == yr,
             yday(end_date),
             ifelse(leap_year(yr), 366, 365))
    
    
    # Select yearly raster stacks
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
    
    
    # Subset to tracking period
    prcp_subset <- prcp_stack[[start_julian:end_julian]]
    tmin_subset <- tmin_stack[[start_julian:end_julian]]
    
    
    # Extract mean daily climate across dBBMM polygon
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
    
    
    # Append daily values
    prcp_vals <- c(prcp_vals, unlist(prcp_extract))
    tmin_vals <- c(tmin_vals, unlist(tmin_extract))
    
  }
  
  
  # Summarize across tracking period
  prcp_sum_list[[bird_id]] <-
    mean(prcp_vals, na.rm = TRUE)
  
  tmin_mean_list[[bird_id]] <-
    mean(tmin_vals, na.rm = TRUE)
  
}

# Create weather dataframe
weather_dbbmm_df <- tibble(
  BirdID = names(prcp_sum_list),
  prcp_mean_mm = unlist(prcp_sum_list),
  tmin_avg_degC = unlist(tmin_mean_list)
)

# Join with movement dataset
dat.ready <- dat.ready %>%
  left_join(
    weather_dbbmm_df,
    by = "BirdID"
  )

################################################################################
## Add in Energy Expenditure

# Read in ODBA csv
odba <- read_csv("Data Management/Csvs/ODBA_2025ModelData/ODBA_Data_2025Revision.csv")

# Get status column to merge
dat.ready.status <- dat.ready %>% dplyr::select(BirdID, Status)

# Merge
odba <- left_join(odba, dat.ready.status, by = "BirdID")

# Only keep ODBA values for residents between 22 March and 6 April
# This was done to attempt to maintain consistent GPS sample sizes between movers and residents
mean_julian <- 82

dat_filtered <- odba %>%
  dplyr::filter(
    !is.na(Status),
    Status != 0 |
      (Status == 0 &
         Julian >= mean_julian &
         Julian <= mean_julian + 14)
  ) %>%
  distinct(BirdID, local_time, .keep_all = TRUE)

# Summarize Mean ODBA by BirdID across a bird's spring movement path
# The other mean_odba was averaged 30 minutes up to and 30 minutes after each hour
mean_odba_per_bird <- dat_filtered %>%
  dplyr::group_by(BirdID) %>%
  summarise(
    mean_odba_dbbmms = mean(mean_odba, na.rm = TRUE),
    n_records = n()
  ) %>%
  arrange(desc(mean_odba_dbbmms))

# Merge with data
dat.ready<- dat.ready %>%
    left_join(mean_odba_per_bird, by = "BirdID")


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
## Output Data

# Save the filtered and processed data
save(
  dat.ready, 
  file = "Data Management/RData/Pennsylvania/Clutch Size/01_ClutchSizePredictors_2025Revision.RData"
)


################################################################################
## Format Data for Journal

# # dBBMM Estimation GPS Data
# df.subset.journal <- df.filtered %>%
#   dplyr::mutate(
#     BirdID = if_else(str_ends(BirdID, "202"), str_c(BirdID, "4"), BirdID)
#   ) %>%
#   st_drop_geometry() %>%
#   dplyr::select(-geometry, -BandID)
# 
# # Assign random numbers to BirdID column to anonymise data
# set.seed(123)
# unique_ids <- unique(df.subset.journal$BirdID)
# random_ids <- sample(100000:999999, length(unique_ids), replace = FALSE)
# id_map <- setNames(random_ids, unique_ids)
# df.subset.journal$BirdID <- id_map[df.subset.journal$BirdID]
# 
# # Export csv
# write_csv(df.subset.journal, "Manuscript/Ecography/Data/05_GPS_DBBMMs.csv")

################################################################################
###############################################################################X
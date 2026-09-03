#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'  
#' **Purpose**: This script calculates ODBA for each bird-year in the analysis
#' **Last Updated**: 1 July 2026

################################################################################
## Load Packages

library(data.table)
library(dplyr)
library(lubridate)
library(terra)
library(sf)
library(tidyr)
library(purrr)
library(tigris)


################################################################################
## Calculate Burst ODBA

# Calculate ODBA for a single row (Per burst)
# ODBA is computed as the mean absolute deviation from the mean acceleration across x, y, and z axes
# Serves as a proxy for movement-related energy expenditure

calc_odba <- function(row){
  xyz <- as.numeric(row[9:length(row)])
  xyz <- matrix(
    xyz,
    ncol = 3,
    byrow = TRUE
  )
  means <- colMeans(xyz)
  deviations <- abs(
    xyz -
      matrix(
        means,
        nrow = nrow(xyz),
        ncol = 3,
        byrow = TRUE
      )
  )
  sum(deviations) / nrow(xyz)
}

################################################################################
## Process Accelerometer Data

# Constrain ACC data to the daylight hours
# Removes night behaviors that are not of interest

process_odba_daytime <- function(df,
                                 timezone = "America/New_York") {
  
  df$ODBA <- apply(df, 1, calc_odba)
  df$timeofday <- ymd_hms(
    paste(df$date, df$time),
    tz = "UTC"
  )
  df$timeofday <- with_tz(
    df$timeofday,
    tzone = timezone
  )
  df <- df %>%
    filter(
      hour(timeofday) >= 6,
      hour(timeofday) <= 19
    )
  return(df)
}

################################################################################
## Process GPS Data

# Constrain GPS data to the daylight hours
# This removes nocturnal behaviors that are not of interest
# Matches temporal alignment with ACC Data

filter_gps_daytime_only <- function(df,
                                    timestamp_col = "timestamp",
                                    timezone = "America/New_York"){
  
  df[[timestamp_col]] <- as.POSIXct(
    df[[timestamp_col]],
    tz = "UTC"
  )
  local_time <- with_tz(
    df[[timestamp_col]],
    timezone
  )
  df %>%
    mutate(local_time = local_time) %>%
    filter(
      hour(local_time) >= 6,
      hour(local_time) <= 19
    )
}

################################################################################
## Mean ODBA Around Each GPS Location

# Calculates mean ODBA within ±30 minutes of every GPS fix
# This is a change from the previous script where it was an hourly calculation

calculate_centered_odba <- function(acc_df,
                                    gps_df,
                                    window_minutes = 30){
  
  acc_df <- acc_df %>%
    arrange(timeofday)
  
  acc_times <- acc_df$timeofday
  acc_odba  <- acc_df$ODBA
  
  gps_df$mean_odba <- NA_real_
  gps_df$n_acc_obs <- 0L
  
  for(i in seq_len(nrow(gps_df))){
    
    gps_time <- gps_df$local_time[i]
    
    idx <- acc_times >= gps_time - minutes(window_minutes) &
      acc_times <= gps_time + minutes(window_minutes)
    
    gps_df$mean_odba[i] <- if(any(idx))
      mean(acc_odba[idx], na.rm = TRUE)
    else
      NA_real_
    
    gps_df$n_acc_obs[i] <- sum(idx)
    
  }
  
  gps_df
  
}

################################################################################
## Weather Raster Functions

# This just matches the alignment of the weather rasters between years
# This is important because the 2024 and 2025 data were aligned differently than the 2022 and 2023 data

align_to_reference <- function(raster,
                               reference,
                               mask_polygon){
  
  raster %>%
    project(crs(reference)) %>%
    resample(reference, method = "bilinear") %>%
    crop(reference) %>%
    mask(mask_polygon)
  
}

################################################################################
## Weather Extraction Function

# Create a raster stack of all precip and tmin rasters
# Extract the point locations from each stack filtered by year

extract_weather <- function(points,
                            precip_stack,
                            tmin_stack){
  
  precip <- numeric(nrow(points))
  tmin   <- numeric(nrow(points))
  
  for(i in seq_len(nrow(points))){
    
    layer <- points$Julian[i]
    
    precip[i] <- terra::extract(
      precip_stack[[layer]],
      points[i,],
      ID = FALSE
    )[1,1]
    
    tmin[i] <- terra::extract(
      tmin_stack[[layer]],
      points[i,],
      ID = FALSE
    )[1,1]
    
  }
  
  points$precip <- precip
  points$tmin   <- tmin
  
  points
  
}

################################################################################
## Load GPS Data

# This GPS data was created for the downstream dBBMM analysis
# For movers it is GPS data restricted to the changepoint windows
# For residents it is restricted from calendar day 82 (March 22) + 14 days

load(
  "Data Management/RData/Pennsylvania/GPS Data/HensGPS4DBBMMsDraft_2025Revision.RData"
)

################################################################################
## Load Land Cover Raster

# Land cover raster derived from the NLCD
# Contains Hardwood, Conifer, Pasture, Crop, Developed, and Other

pa.nlcd <- rast(
  "Data Management/Rasters/NLCD/pa.nlcd.tif"
)

################################################################################
## Load Weather Rasters

# CRS for weather rasters
crs_string <- paste(
  "+proj=lcc",
  "+lat_0=42.5",
  "+lon_0=-100",
  "+lat_1=25",
  "+lat_2=60",
  "+x_0=0",
  "+y_0=0",
  "+datum=WGS84",
  "+units=m",
  "+no_defs"
)

# Lists of rasters to be loaded
# Precip = Daily precipitation
# Tmin = Daily minimum temperature
precip <- list(
  `2022` = rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/prcp_2022_PA_50km.tif"),
  `2023` = rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/prcp_2023_PA_50km.tif"),
  `2024` = rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/prcp_2024_PA_50km.tif"),
  `2025` = rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/prcp_2025_PA_50km.tif")
)

tmin <- list(
  `2022` = rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/tmin_2022_PA_50km.tif"),
  `2023` = rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/tmin_2023_PA_50km.tif"),
  `2024` = rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/tmin_2024_PA_50km.tif"),
  `2025` = rast("Data Management/Rasters/Weather/PA_daymet_2019_2023/tmin_2025_PA_50km.tif")
)

################################################################################
## Standardize CRS

# Apply the CRS function to each raster

precip <- lapply(precip, function(x){
  
  crs(x) <- crs_string
  x
  
})

tmin <- lapply(tmin, function(x){
  
  crs(x) <- crs_string
  x
  
})

################################################################################
## Pennsylvania Boundary

# Obtain a PA boundary from the tigris package
# Crop the weather rasters to the PA boundary (50,000 km)

states <- tigris::states(cb = TRUE)

states <- st_transform(states, crs_string)

pa <- states %>%
  filter(NAME == "Pennsylvania")

pa_buffer <- st_buffer(
  pa,
  dist = 50000
)

pa_vect <- vect(pa_buffer)

################################################################################
## Align Weather Rasters

# Align the weather rasters with the reference raster

reference_raster <- precip[[1]]

for(i in 2:length(precip)){
  
  precip[[i]] <- align_to_reference(
    precip[[i]],
    reference_raster,
    pa_vect
  )
  
  tmin[[i]] <- align_to_reference(
    tmin[[i]],
    reference_raster,
    pa_vect
  )
  
}

################################################################################
## Processing Directories

# Set up the processing directories in PA OneDrive
# Extract files from KS hard drive

input_dir <- "E:/Ch2/RawACC/2025Revisions/"

output_dir <- "Data Management/Csvs/ODBA_Calcs_2025Revision/Test"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

files <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)


################################################################################
# Create a Failure Log

failure_log <- data.frame(
  file = character(),                    # Name of ACC file
  BandID = character(),                  # Band identifier being processed
  BirdID = character(),                  # Bird identifier (if available)
  stage = character(),                   # Processing step where failure occurred
  reason = character(),                  # Description of failure
  n_acc_records = integer(),             # Number of ACC records before filtering
  n_acc_daytime = integer(),             # Number of daytime ACC records
  n_gps_records = integer(),             # Number of GPS records before filtering
  n_gps_daytime = integer(),             # Number of daytime GPS records
  n_gps_after_odba_filter = integer(),   # GPS records remaining after ODBA matching
  stringsAsFactors = FALSE
)

failure_log <- data.frame(
  file = character(),
  BandID = character(),
  BirdID = character(),
  stage = character(),
  reason = character(),
  n_acc_records = integer(),
  n_acc_daytime = integer(),
  n_gps_records = integer(),
  n_gps_daytime = integer(),
  n_gps_after_odba_filter = integer(),
  stringsAsFactors = FALSE
)

################################################################################
## Create a table to record successful processing

success_log <- data.frame(
  file = character(),       # ACC filename
  BandID = character(),     # Band ID processed
  BirdID = character(),     # Bird ID(s)
  n_records = integer(),    # Number of records exported
  stringsAsFactors = FALSE
)

################################################################################
## Helper function for adding one row to the failure log

log_failure <- function(file,
                        BandID = NA,
                        BirdID = NA,
                        stage,
                        reason,
                        n_acc_records = NA,
                        n_acc_daytime = NA,
                        n_gps_records = NA,
                        n_gps_daytime = NA,
                        n_gps_after_odba_filter = NA){
  
  ## Append one new failure record to the global failure_log
  failure_log <<- rbind(
    failure_log,
    data.frame(
      file = basename(file),                 # Filename only (remove path)
      BandID = BandID,
      BirdID = BirdID,
      stage = stage,
      reason = reason,
      n_acc_records = n_acc_records,
      n_acc_daytime = n_acc_daytime,
      n_gps_records = n_gps_records,
      n_gps_daytime = n_gps_daytime,
      n_gps_after_odba_filter = n_gps_after_odba_filter,
      stringsAsFactors = FALSE
    )
  )
}

################################################################################
## Main processing loop
## Iterate through every ACC file.

for (file in files) {
  
  ## Print separator in the console.
  message("======================================================")
  
  ## Show which file is currently being processed.
  message("Processing file: ", basename(file))
  
  ##############################################################################
  ## Read ACC data
  
  ## Try reading the file. If reading fails, log the error and continue.
  acc <- tryCatch(
    
    fread(file),
    
    error = function(e){
      
      log_failure(
        file,
        stage = "ACC loading",
        reason = paste("Could not read ACC file:", e$message)
      )
      
      return(NULL)
      
    }
    
  )
  
  ## Free memory after reading
  gc()
  
  ## Skip this file if it could not be loaded
  if(is.null(acc))
    next
  
  ## Columns required for downstream processing
  required_cols <- c("date", "time", "bandid")
  
  ## Verify required columns exist
  if(!all(required_cols %in% names(acc))){
    
    log_failure(
      file,
      stage = "ACC loading",
      reason = "Missing required ACC columns",
      n_acc_records = nrow(acc)
    )
    
    next
    
  }
  
  ##############################################################################
  ## Process each BandID separately
  
  for (band_id in unique(acc$bandid)) {
    
    ## Display BandID being processed
    message("  BandID: ", band_id)
    
    ###########################################################################
    ## Keep only ACC records for this BandID
    
    acc_band <- acc %>%
      filter(bandid == band_id)
    
    ## If filtering returned nothing, record failure
    if(nrow(acc_band) == 0){
      
      log_failure(
        file,
        BandID = band_id,
        stage = "ACC filtering",
        reason = "No ACC records after BandID filtering"
      )
      
      next
      
    }
    
    ## Store total ACC observations
    n_acc_records <- nrow(acc_band)
    
    ###########################################################################
    ## Calculate daytime ODBA
    
    acc_band <- tryCatch(
      
      process_odba_daytime(acc_band),
      
      error = function(e){
        
        log_failure(
          file,
          BandID = band_id,
          stage = "ODBA calculation",
          reason = e$message,
          n_acc_records = n_acc_records
        )
        
        return(NULL)
        
      }
      
    )
    
    ## Skip BandID if ODBA calculation failed
    if(is.null(acc_band))
      next
    
    ## Number of daytime ACC observations
    n_acc_daytime <- nrow(acc_band)
    
    ## Skip if no daytime ACC observations remain
    if(n_acc_daytime == 0){
      
      log_failure(
        file,
        BandID = band_id,
        stage = "ODBA calculation",
        reason = "No daytime ACC records",
        n_acc_records = n_acc_records
      )
      
      next
      
    }
    
    ###########################################################################
    ## Match GPS data using BandID
    
    gps_band <- df.filtered %>%
      filter(BandID == band_id)
    
    ## Stop if no GPS exists.
    if(nrow(gps_band) == 0){
      
      log_failure(
        file,
        BandID = band_id,
        stage = "GPS matching",
        reason = "No GPS records for BandID",
        n_acc_records = n_acc_records,
        n_acc_daytime = n_acc_daytime
      )
      
      next
      
    }
    
    ## Store GPS counts
    n_gps_records <- nrow(gps_band)
    
    ## Save BirdIDs represented by this BandID
    bird_ids <- unique(gps_band$BirdID)
    
    ###########################################################################
    ## Keep only daytime GPS fixes
    
    gps_band <- filter_gps_daytime_only(gps_band)
    
    ## Count daytime GPS fixes
    n_gps_daytime <- nrow(gps_band)
    
    ## Skip if no daytime GPS fixes
    if(n_gps_daytime == 0){
      
      log_failure(
        file,
        BandID = band_id,
        BirdID = paste(bird_ids, collapse = ";"),
        stage = "GPS filtering",
        reason = "No daytime GPS fixes",
        n_acc_records = n_acc_records,
        n_acc_daytime = n_acc_daytime,
        n_gps_records = n_gps_records
      )
      
      next
      
    }
    
    ###########################################################################
    ## Prepare GPS spatial data
    
    gps_band <- tryCatch({
      
      gps_band %>%
        
        ## Extract longitude and latitude from geometry
        mutate(
          Longitude = st_coordinates(geometry)[,1],
          Latitude = st_coordinates(geometry)[,2],
          
          ## Create date variables
          Date = as.Date(local_time),
          Year = year(local_time),
          Julian = yday(local_time)
        ) %>%
        
        ## Convert back into an sf object
        st_as_sf(
          coords = c("Longitude","Latitude"),
          crs = 4326
        ) %>%
        
        ## Project coordinates
        st_transform(5070)
      
    }, error=function(e){
      
      log_failure(
        file,
        BandID = band_id,
        BirdID = paste(bird_ids, collapse=";"),
        stage="GPS processing",
        reason=e$message,
        n_acc_records=n_acc_records,
        n_acc_daytime=n_acc_daytime,
        n_gps_records=n_gps_records,
        n_gps_daytime=n_gps_daytime
      )
      
      return(NULL)
      
    })
    
    ## Skip if GPS processing failed
    if(is.null(gps_band))
      next
    
    ###########################################################################
    ## Add weather variables
    
    gps_band$precip <- NA_real_
    gps_band$tmin <- NA_real_
    
    ## Loop through each year separately
    for(yr in names(precip)){
      
      idx <- gps_band$Year == as.numeric(yr)
      
      if(any(idx)){
        
        gps_band[idx,] <- extract_weather(
          gps_band[idx,],
          precip[[yr]],
          tmin[[yr]]
        )
        
      }
      
    }
    
    ###########################################################################
    ## Extract land cover values
    
    gps_band <- cbind(
      gps_band,
      terra::extract(
        pa.nlcd,
        gps_band,
        ID = FALSE
      )
    )
    
    ###########################################################################
    ## Match GPS fixes to nearby ACC observations
    
    gps_band <- calculate_centered_odba(
      acc_df = acc_band,
      gps_df = gps_band,
      window_minutes = 30
    )
    
    ## Require at least 10 ACC observations
    gps_band <- gps_band %>%
      filter(n_acc_obs >= 10)
    
    ## Count remaining GPS fixes.
    n_gps_after_odba_filter <- nrow(gps_band)
    
    ## Skip if no matches remain.
    if(n_gps_after_odba_filter == 0){
      
      log_failure(
        file,
        BandID = band_id,
        BirdID = paste(bird_ids, collapse=";"),
        stage="ODBA-GPS matching",
        reason="No GPS fixes with >=10 ACC observations",
        n_acc_records=n_acc_records,
        n_acc_daytime=n_acc_daytime,
        n_gps_records=n_gps_records,
        n_gps_daytime=n_gps_daytime
      )
      
      next
      
    }
    
    ###########################################################################
    ## Export merged data
    
    output <- gps_band %>%
      st_drop_geometry() %>%
      as.data.frame()
    
    ## Construct output filename
    output_file <- file.path(
      output_dir,
      paste0(
        "merged_",
        band_id,
        "_",
        tools::file_path_sans_ext(
          basename(file)
        ),
        ".csv"
      )
    )
    
    ## Save merged dataset
    write.csv(
      output,
      output_file,
      row.names = FALSE
    )
    
    ###########################################################################
    ## Record successful processing
    
    success_log <- rbind(
      success_log,
      data.frame(
        file = basename(file),
        BandID = band_id,
        BirdID = paste(unique(output$BirdID), collapse=";"),
        n_records = nrow(output),
        stringsAsFactors = FALSE
      )
    )
    
    ## Print success message
    message(
      "    Saved: ",
      basename(output_file)
    )
    
    ## Release memory before processing the next BandID
    gc()
    
  }
  
}

################################################################################
## Save Logs

write.csv(
  failure_log,
  file.path(output_dir,"ODBA_failure_log.csv"),
  row.names = FALSE
)

write.csv(
  success_log,
  file.path(output_dir,"ODBA_success_log.csv"),
  row.names = FALSE
)


################################################################################
## Combine All BandID CSVs for Modeling

# Input and Output directories
input_dir <- "Data Management/Csvs/ODBA_Calcs_2025Revision/Test/"
output_dir <- "Data Management/Csvs/ODBA_2025ModelData/Test"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

files2combine <- list.files(
  input_dir,
  pattern = "^merged_.*\\.csv$",
  full.names = TRUE
)

safe_read <- function(file) {
  df <- read.csv(file, check.names = TRUE)
  
  # Drop first column if it’s likely just row numbers
  if (all(grepl("^\\d+$", as.character(df[[1]]))) &&
      !("BandID.x" %in% names(df))) {
    df <- df[, -1]
  }
  
  # Force Bird_Year and time_bin to character if they exist
  if ("Bird_Year" %in% names(df)) {
    df$Bird_Year <- as.character(df$Bird_Year)
  }
  if ("time_bin" %in% names(df)) {
    df$time_bin <- as.character(df$time_bin)
  }
  
  if ("BirdID" %in% names(df)) {
    df$BirdID <- as.character(df$BirdID)
  }
  
  if ("timestamp" %in% names(df)) {
    df$timestamp <- as.character(df$timestamp)
  }
  
  if ("local_time" %in% names(df)) {
    df$local_time <- as.character(df$local_time)
  }
  
  if ("Class" %in% names(df)) {
    df$Class <- as.character(df$Class)
  }
  
  if ("Date" %in% names(df)) {
    df$Date <- as.character(df$Date)
  }
  return(df)
}

# Read and combine
combined_df <- files2combine %>%
  lapply(safe_read) %>%
  bind_rows()

# Save output
output_file <- file.path(output_dir, "ODBA_Data_2025RevisionTest.csv")
write.csv(combined_df, output_file, row.names = FALSE)

message("✅ Combined CSV saved to: ", output_file)

################################################################################
###############################################################################X
#'---
#' title: Seasonal Movements of Wild Turkeys in the Mid-Atlantic Region
#' author: "K. Smelter, F. Buderman"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: 
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script creates ctmm home ranges during winter and nesting and extracts covariates
#' **Last Updated**: 5/20/25

library(sp)
library(adehabitatHR)
library(tidyverse)
library(mapview)

################################################################################
## Winter Home Ranges

# Load winter GPS data (60 days pre/post incubation)
load("Data Management/RData/Pennsylvania/GPS Data/Winter/df.Winter.RData")

# Format data for ctmm or adehabitatHR (used below)
df.all.Winter <- df.all.Winter %>%
  mutate(long = unlist(map(geometry, 1)),
         lat = unlist(map(geometry, 2))) %>%
  dplyr::select(BirdID, timestamp, long, lat)

# Convert timestamp to POSIXct if not already
df.all.Winter$timestamp <- as.POSIXct(df.all.Winter$timestamp)

# Create an empty list to store utilization distributions (UDs)
out.winter <- list()

# Loop through each individual
ids <- unique(df.all.Winter$BirdID)

# Create an empty list to store utilization distributions
out.winter <- list()

# Get unique bird IDs
ids <- unique(df.all.Winter$BirdID)

out.winter <- list()
ids <- unique(df.all.Winter$BirdID)

for (i in seq_along(ids)) {
  id <- ids[i]
  
  sub <- df.all.Winter %>% filter(BirdID == id)
  
  coordinates(sub) <- ~long + lat
  proj4string(sub) <- CRS("+proj=longlat +datum=WGS84")
  
  tryCatch({
    kde <- kernelUD(sub, h = "href", grid = 40, same4all = FALSE,
                    hlim = c(0.1, 1.5), kern = "bivnorm", extent = 0.5)
    
    ver <- getverticeshr(kde, 95)
    
    # Keep only your custom BirdID column
    ver@data <- ver@data %>% dplyr::select(-id)  # remove default id column if present
    ver@data$ID <- id  # assign BirdID as ID
    
    out.winter[[id]] <- ver
  }, error = function(e) {
    message(paste("Skipping ID", id, "due to error:", e$message))
  })
}

# Combine all home ranges into one object
all_ranges <- do.call(rbind, out.winter)

# Plot
mapview(all_ranges, zcol = "ID")

# Save output
save(out.winter, file = "Data Management/RData/Pennsylvania/Home Range/Nesting/winterhrs.RData")


################################################################################
## Nesting Home Range 

# Load nesting data (as you already did above)
load("Data Management/RData/Pennsylvania/GPS Data/Nesting/df.inc.RData")

# Format data for ctmm/adehabitatHR
df.all.inc <- df.all.inc %>%
  mutate(long = unlist(map(df.all.inc$geometry, 1)),
         lat = unlist(map(df.all.inc$geometry, 2))) %>%
  dplyr::select(BirdID, timestamp, long, lat)

# Create an empty list to store utilization distributions (UDs)
out.nest <- list()

# Loop through each individual bird (you might want to reduce this from 127 if necessary)
ids <- unique(df.all.inc$BirdID)

for (i in seq_along(ids)) {
  id <- ids[i]
  
  # Subset data for individual bird
  sub <- df.all.inc %>% filter(BirdID == id)
  
  # Convert to spatial object (SpatialPointsDataFrame)
  coordinates(sub) <- ~long + lat
  proj4string(sub) <- CRS("+proj=longlat +datum=WGS84")
  
  tryCatch({
    # Fit KDE model (using href as the bandwidth selector)
    kde <- kernelUD(sub, h = "href", grid = 40, same4all = FALSE,
                    hlim = c(0.1, 1.5), kern = "bivnorm", extent = 0.5)
    
    # Estimate the 95% home range contour (using getverticeshr)
    ver <- getverticeshr(kde, 99)
    
    # Keep only your custom BirdID column
    ver@data <- ver@data %>% dplyr::select(-id)  # remove default id column if present
    ver@data$ID <- id
    
    # Store in the list
    out.nest[[id]] <- ver
  }, error = function(e) {
    message(paste("Skipping ID", id, "due to error:", e$message))
  })
}

# Combine all home ranges into one object
all_ranges <- do.call(rbind, out.nest)

# Plot
mapview(all_ranges, zcol = "ID")

# Save the results (you can remove unused variables if necessary)
save(out.nest, file = "Data Management/RData/Pennsylvania/Home Range/Nesting/nesthrs.RData")


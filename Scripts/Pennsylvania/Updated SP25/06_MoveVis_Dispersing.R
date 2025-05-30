#'---
#' title: Seasonal Movements of Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script creates GPS visualizations for dispersing turkeys using the MoveVis package
#' **Last Updated**: 5/30/25


################################################################################
## Load Packages 

require(dplyr)
require(lubridate)
require(move2)
require(purrr)
require(mapview) 
devtools::install_github("16EAGLE/moveVis") #Developer version, had issues installing package
require(moveVis)


################################################################################
## Data Prep

#' Filter to a specific bird
#' Filter timestamps to a particular period
dat.move.disp <- df_sub_sf %>%
  dplyr::filter(BirdID == "8805_2023") %>%
  ungroup() %>%
  dplyr::filter(timestamp >= as.POSIXct("2023-03-14") & 
           timestamp <= as.POSIXct("2023-04-01"))

#' Extract coordinates
coords <- st_coordinates(dat.move.disp)

#' Double-check that lengths match
stopifnot(nrow(dat.move.disp) == nrow(coords))

#' Bind lon/lat
dat.move.disp <- dat.move.disp %>%
  mutate(
    long = coords[, "X"],
    lat = coords[, "Y"]
  )

#' Convert to a move object
dat <- df2move(df = dat.move.disp,
          proj = 4326, 
          x = c("long"), 
          y = c("lat"), 
          time = "timestamp",  
          track_id = "BirdID")  

#' Align move_data to a uniform time scale
m<- align_move(dat, res = 3, unit = "hours") 

#' Create frames
frames <- frames_spatial(m,
                         path_colours = c("orange"),
                         map_service = "osm",
                         map_type = "streets",
                         alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress() 


frames[[95]] # preview one of the frames, e.g. the 100th frame

#' Animate frames
animate_frames(frames, out_file = "Disp.gif", overwrite = T)

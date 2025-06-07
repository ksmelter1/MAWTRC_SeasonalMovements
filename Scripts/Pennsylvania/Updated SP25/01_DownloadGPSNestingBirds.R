#'---
#' title: Seasonal Movements of Wild Turkeys in the Mid-Atlantic Region
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'  
#' **Purpose**: This script downloads movement data associated with nesting hens for NSD calculation
#' **Last Updated**: 5/23/25


################################################################################
## Load Packages 

#' Vector of package names
packages <- c("purrr",
              "lubridate",
              "dplyr",
              "move2",
              "tidyverse",
              "amt",
              "stringr")

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
## Data Management

#' Read in Pennsylvania nests csv
pa.nests <- read_csv("Data Management/Csvs/Pennsylvania/Processed/Nests/Nests/Pennsylvania/2025_CleanedNests_2022_2023_PA.csv") 
pa.nests

#' Read in incubation csv
nests.inc <- read_csv("Data Management/Csvs/Pennsylvania/Processed/Incubation Dates/Pennsylvania/20250131_NestAttempts_allbirds_PA.csv")
nests.inc

#' Read in captures csv
captures <- read_csv("Data Management/Csvs/Pennsylvania/Processed/Captures/captures.csv")
captures

#' Filter captures to only include hens
#' Create a capture column
captures <- captures %>% rename(BandID = bandid) %>%
  dplyr::filter(sex == "F")  %>%
  mutate(CaptureDate = as.Date(paste(captyr, captmo, captday, sep = "-")))

#' Merge captures and PA nesting datasets
pa.nests <- right_join(captures, pa.nests)

#' Subset nesting data for 4D 
pa.nests.4D <- dplyr::filter(pa.nests, WMU =="4D")%>%
  dplyr::select(BandID, CheckDate, NestID, WMU, Lat, Long, NestNumber, CaptureDate) %>%
  dplyr::filter(NestID != "8262_2022_1") %>%
  dplyr::filter(NestNumber == "1")

#' Stratify dataset by incubation data
pa.nests.4D1 <- inner_join(pa.nests.4D, nests.inc, by = "NestID") %>%
  select(-CheckDate.y) %>%
  rename(CheckDate = CheckDate.x) %>%
  mutate(
    CapYear = year(CaptureDate),
    CheckYear = year(CheckDate),
    CapMonth = month(CaptureDate),
    Start = case_when(
      CapMonth == 12 ~ CaptureDate + days(5),
      CapYear != CheckYear ~ CaptureDate + days(365),
      TRUE ~ CaptureDate
    ),
    End = endI
  )

glimpse(pa.nests.4D1)


#' Subset nesting data for 3D in year 2022
pa.nests.3D <- dplyr::filter(pa.nests, WMU =="3D")%>%
  dplyr::select(BandID, CheckDate, NestID, WMU, Lat, Long, NestNumber, CaptureDate) %>%
  dplyr::filter(NestNumber == "1")

#' Merge pa.nests.3D and nests.inc, only keep nests that exist in both pa.nests.3D and nests.inc
pa.nests.3D1 <- dplyr::inner_join(pa.nests.3D, nests.inc, by = "NestID") %>%
  select(-CheckDate.y) %>%
  rename(CheckDate = CheckDate.x) %>%
  mutate(
    CapYear = year(CaptureDate),
    CheckYear = year(CheckDate),
    CapMonth = month(CaptureDate),
    Start = case_when(
      CapMonth == 12 ~ CaptureDate + days(5),
      CapYear != CheckYear ~ CaptureDate + days(365),
      TRUE ~ CaptureDate
    ),
    End = endI
  )

glimpse(pa.nests.3D1)

#' Subset nesting data for 2D 
pa.nests.2D <- dplyr::filter(pa.nests, WMU =="2D")%>%
  dplyr::select(BandID, CheckDate, NestID, WMU, Lat, Long, NestNumber, CaptureDate) %>%
  dplyr::filter(NestNumber == "1")

#' Stratify dataset by incubation data
pa.nests.2D1 <- inner_join(pa.nests.2D, nests.inc, by = "NestID") %>%
  select(-CheckDate.y) %>%
  rename(CheckDate = CheckDate.x) %>%
  mutate(
    CapYear = year(CaptureDate),
    CheckYear = year(CheckDate),
    CapMonth = month(CaptureDate),
    Start = case_when(
      CapMonth == 12 ~ CaptureDate + days(5),
      CapYear != CheckYear ~ CaptureDate + days(365),
      TRUE ~ CaptureDate
    ),
    End = endI
  )


#' Subset nesting data for 5C 
pa.nests.5C <- dplyr::filter(pa.nests, WMU =="5C")%>%
  dplyr::select(BandID, CheckDate, NestID, WMU, Lat, Long, NestNumber, CaptureDate) %>%
  dplyr::filter(NestNumber == "1")

#' Merge pa.nests.5C and nests.inc, only keep nests that exist in both pa.nests.5C and nests.inc
pa.nests.5C1 <- dplyr::inner_join(pa.nests.5C, nests.inc, by = "NestID") %>%
  select(-CheckDate.y) %>%
  rename(CheckDate = CheckDate.x) %>%
  mutate(
    CapYear = year(CaptureDate),
    CheckYear = year(CheckDate),
    CapMonth = month(CaptureDate),
    Start = case_when(
      CapMonth == 12 ~ CaptureDate + days(5),
      CapYear != CheckYear ~ CaptureDate + days(365),
      TRUE ~ CaptureDate
    ),
    End = endI
  )%>%
  dplyr::filter(NestID != "9072_2023_1") %>%
  dplyr::filter(NestID != "9093_2023_1")
glimpse(pa.nests.5C1)


################################################################################
## Login to Movebank


login <- movebank_store_credentials(username = "Kyle.Smelter",
                                    password="Rayshawks5!",
                                    key="Kyle",
                                    force= T)

# Downloads movebank data separately for hens that nest versus hens that didn't nest
# Will combine datasets in a later script
# For hens that nested download from the capture date through the end of incubation for nest attempt 1
# For hens that didn't nest, download from capture date through the end of May
# I later constrict GPS data between February and the end of May in both datasets


#############################################################################
## WMU 4D - GPS Data


unique.ID.4d<-unique(pa.nests.4D1$NestID)

for (j in 1:length(unique.ID.4d)){
  tmp.subset.4d<-pa.nests.4D1[which(pa.nests.4D1$NestID==unique.ID.4d[j]),]
  tmp.subset.4d$TrackID<-paste(unique.ID.4d[j],seq(1,nrow(tmp.subset.4d),1),sep="_")
  
  for(i in 1:nrow(tmp.subset.4d)){
    BirdID<- as.character(tmp.subset.4d[i,1])
    EndDate <- gsub("\\D","", tmp.subset.4d$End[i]) 
    
    StartDate <- gsub("\\D","", tmp.subset.4d$Start[i]) 
    
    Year <- lubridate::year(tmp.subset.4d$Start[i])
    
    
    dat.4d<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 4D", 
                                     login = login,
                                     individual_local_identifier= BirdID,
                                     timestamp_start= StartDate,
                                     timestamp_end= EndDate,
                                     Year = Year,
                                     removeDuplicatedTimestamps=T)
    mt_track_id(dat.4d)<-rep(tmp.subset.4d$TrackID[i],nrow(dat.4d))
    
    if(exists("full_all_4d")){ 
      full_all_4d <- rbind(full_all_4d, dat.4d)
      
      
    }else{
      full_all_4d <- dat.4d
    }
  }
}

saveRDS(full_all_4d, "Data Management/RData/Pennsylvania/GPS Data/4D/GPS.4D.RDS")



dat.4d.non <- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 4D", 
                                      login = login,
                                      removeDuplicatedTimestamps=T)

saveRDS(dat.4d.non, "Data Management/RData/Pennsylvania/GPS Data/4D/GPS.4D.NonNest.RDS")


################################################################################
## WMU 3D - GPS Data


unique.ID.3d<-unique(pa.nests.3D1$NestID)

for (j in 1:length(unique.ID.3d)){
  tmp.subset.3d<-pa.nests.3D1[which(pa.nests.3D1$NestID==unique.ID.3d[j]),]
  tmp.subset.3d$TrackID<-paste(unique.ID.3d[j],seq(1,nrow(tmp.subset.3d),1),sep="_")
  
  for(i in 1:nrow(tmp.subset.3d)){
    BirdID<- as.character(tmp.subset.3d[i,1])
    EndDate <- gsub("\\D","", tmp.subset.3d$End[i]) 
    
    StartDate <- gsub("\\D","", tmp.subset.3d$Start[i]) 
    
    Year <- lubridate::year(tmp.subset.3d$Start[i])
    
    
    dat.3d<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 3D", 
                                     login = login,
                                     individual_local_identifier= BirdID,
                                     timestamp_start= StartDate,
                                     timestamp_end= EndDate,
                                     removeDuplicatedTimestamps=T)
    mt_track_id(dat.3d)<-rep(tmp.subset.3d$TrackID[i],nrow(dat.3d))
    
    if(exists("full_all_3d")){ 
      full_all_3d <- rbind(full_all_3d, dat.3d)
      
      
    }else{
      full_all_3d <- dat.3d
    }
  }
}

saveRDS(full_all_3d, "Data Management/RData/Pennsylvania/GPS Data/3D/GPS.3D.RDS")


dat.3d.non <- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 3D", 
                                      login = login,
                                      removeDuplicatedTimestamps=T)

saveRDS(dat.3d.non, "Data Management/RData/Pennsylvania/GPS Data/3D/GPS.3D.NonNest.RDS")


################################################################################
##  WMU 2D - GPS Data


unique.ID.2d<-unique(pa.nests.2D1$NestID)

for (j in 1:length(unique.ID.2d)){
  tmp.subset.2d<-pa.nests.2D1[which(pa.nests.2D1$NestID==unique.ID.2d[j]),]
  tmp.subset.2d$TrackID<-paste(unique.ID.2d[j],seq(1,nrow(tmp.subset.2d),1),sep="_")
  
  for(i in 1:nrow(tmp.subset.2d)){
    BirdID<- as.character(tmp.subset.2d[i,1])
    EndDate <- gsub("\\D","", tmp.subset.2d$End[i]) 
    
    StartDate <- gsub("\\D","", tmp.subset.2d$Start[i]) 
    
    Year <- lubridate::year(tmp.subset.2d$Start[i])
    
    dat.2d<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 2D", 
                                     login = login,
                                     individual_local_identifier= BirdID,
                                     timestamp_start= StartDate,
                                     timestamp_end= EndDate,
                                     removeDuplicatedTimestamps=T)
    mt_track_id(dat.2d)<-rep(tmp.subset.2d$TrackID[i],nrow(dat.2d))
    
    if(exists("full_all_2d")){ 
      full_all_2d <- rbind(full_all_2d, dat.2d)
      
      
    }else{
      full_all_2d <- dat.2d
    }
  }
}

saveRDS(full_all_2d, "Data Management/RData/Pennsylvania/GPS Data/2D/GPS.2D.RDS")


dat.2d.non <- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 2D", 
                                      login = login,
                                      removeDuplicatedTimestamps=T)

saveRDS(dat.2d.non, "Data Management/RData/Pennsylvania/GPS Data/2D/GPS.2D.NonNest.RDS")

################################################################################
##  WMU 5C - GPS Data


unique.ID.5c<-unique(pa.nests.5C1$NestID)

for (j in 1:length(unique.ID.5c)){
  tmp.subset.5c<-pa.nests.5C1[which(pa.nests.5C1$NestID==unique.ID.5c[j]),]
  tmp.subset.5c$TrackID<-paste(unique.ID.5c[j],seq(1,nrow(tmp.subset.5c),1),sep="_")
  
  for(i in 1:nrow(tmp.subset.5c)){
    BirdID<- as.character(tmp.subset.5c[i,1])
    EndDate <- gsub("\\D","", tmp.subset.5c$End[i]) 
    
    StartDate <- gsub("\\D","", tmp.subset.5c$Start[i]) 
    
    Year <-lubridate::year(tmp.subset.5c$Start[i])
    
    
    dat.5c<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 5C", 
                                     login = login,
                                     individual_local_identifier= BirdID,
                                     timestamp_start= StartDate,
                                     timestamp_end= EndDate,
                                     removeDuplicatedTimestamps=T)
    mt_track_id(dat.5c)<-rep(tmp.subset.5c$TrackID[i],nrow(dat.5c))
    
    if(exists("full_all_5c")){ 
      full_all_5c <- rbind(full_all_5c, dat.5c)
      
      
    }else{
      full_all_5c <- dat.5c
    }
  }
}

saveRDS(full_all_5c, "Data Management/RData/Pennsylvania/GPS Data/5c/GPS.5c.RDS")


dat.5c.non <- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 5C", 
                                      login = login,
                                      removeDuplicatedTimestamps=T)

saveRDS(dat.5c.non, "Data Management/RData/Pennsylvania/GPS Data/5C/GPS.5C.NonNest.RDS")


################################################################################
## Organize Movement Data

#' Convert hens that nested to move objects to dataframes
#' Create BirdID column, keep characters 1-9
full_all_3d <- as.data.frame(full_all_3d) %>%
  dplyr::mutate(BirdID = paste0(individual_local_identifier, "_", year(ymd_hms(timestamp)))) %>%
  dplyr::mutate(BirdID = str_sub(BirdID, 1, 9))  

full_all_4d <- as.data.frame(full_all_4d) %>%
  dplyr::mutate(BirdID = paste0(individual_local_identifier, "_", year(ymd_hms(timestamp)))) %>%
  dplyr::mutate(BirdID = str_sub(BirdID, 1, 9))  

full_all_2d <- as.data.frame(full_all_2d) %>%
  dplyr::mutate(BirdID = paste0(individual_local_identifier, "_", year(ymd_hms(timestamp)))) %>%
  dplyr::mutate(BirdID = str_sub(BirdID, 1, 9))  

full_all_5c <- as.data.frame(full_all_5c) %>%
  dplyr::mutate(BirdID = paste0(individual_local_identifier, "_", year(ymd_hms(timestamp)))) %>%
  dplyr::mutate(BirdID = str_sub(BirdID, 1, 9))  

#' Convert hens that didn't nest to move objects
#' Create BirdID column 
#' Filter data to only include birds that didn't nest between 2022 and 2023 and that the month is less than June but greater than or equal to February
full_all_3d_non <- as.data.frame(dat.3d.non) %>%
  dplyr::mutate(BirdID = paste0(individual_local_identifier, "_", year(ymd_hms(timestamp))))  %>%
  dplyr::filter(!(BirdID %in% full_all_3d$BirdID)  & lubridate::year(timestamp) != 2024 & lubridate::year(timestamp) != 2025 & lubridate::month(timestamp) <6 & lubridate::month(timestamp) >= 2)
full_all_2d_non <- as.data.frame(dat.2d.non) %>%
  dplyr::mutate(BirdID = paste0(individual_local_identifier, "_", year(ymd_hms(timestamp)))) %>%
  dplyr::filter(!(BirdID %in% full_all_2d$BirdID)  & lubridate::year(timestamp) != 2024 & lubridate::year(timestamp) != 2025 & lubridate::month(timestamp) <6 & lubridate::month(timestamp) >= 2)
full_all_4d_non <- as.data.frame(dat.4d.non) %>%
  dplyr::mutate(BirdID = paste0(individual_local_identifier, "_", year(ymd_hms(timestamp)))) %>%
  dplyr::filter(!(BirdID %in% full_all_4d$BirdID)  & lubridate::year(timestamp) != 2024 & lubridate::year(timestamp) != 2025 & lubridate::month(timestamp) <6 & lubridate::month(timestamp) >= 2)
full_all_5c_non <- as.data.frame(dat.5c.non) %>%
  dplyr::mutate(BirdID = paste0(individual_local_identifier, "_", year(ymd_hms(timestamp)))) %>%
  dplyr::filter(!(BirdID %in% full_all_5c$BirdID) & lubridate::year(timestamp) != 2024 & lubridate::year(timestamp) != 2025 & lubridate::month(timestamp) <6 & lubridate::month(timestamp) >= 2)


#' Create dataframe of hens that nested to move object
#' Nesting Index "Yes" means the hen nested and was identified via chapter 1 start and end dates
df.nesting <- rbind(full_all_5c, 
                full_all_3d, 
                full_all_2d, 
                full_all_4d) %>%
  dplyr::mutate(individual_local_identifier = str_remove(individual_local_identifier, "_1$")) %>%
  dplyr::rename(NestID = individual_local_identifier) %>%
  dplyr::mutate("Nesting Index" = "Yes")


#' Create dataframe of hens that nested to move object
#' Nesting Index "No" means that hen may have nested but not identified by start or end dates or it didn't nest
df.nonnesting <- rbind(full_all_5c_non, 
                    full_all_3d_non, 
                    full_all_2d_non, 
                    full_all_4d_non) %>%
  dplyr::rename("BandID"= individual_local_identifier) %>%
  dplyr::mutate("Nesting Index" = "No")


#' Outout gps data for birds that nested
save(df.nesting, 
     full_all_2d,
     full_all_3d, 
     full_all_4d,
     full_all_5c, 
     file = "Data Management/RData/Pennsylvania/GPS Data/NestingHensGPS.RData", overwrite = T)

#' Output gps data for birds that didn't nest
save(df.nonnesting,
     full_all_2d_non,
     full_all_3d_non, 
     full_all_4d_non,
     full_all_5c_non, 
     file = "Data Management/RData/Pennsylvania/GPS Data/NonNestingHensGPS.RData", overwrite = T)

################################################################################
###############################################################################X
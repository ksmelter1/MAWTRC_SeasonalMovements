#---
# title: Seasonal Movements of Wild Turkeys in Pennsylvania
# author: K. Smelter
# date: "`r format(Sys.time(), '%d %B, %Y')`"
# output: 
#   html_document: 
#     toc: true
#---
#  
# **Purpose**: This provides summary statistics to be presented in the manuscript
# **Last Updated**: 12/28/25


################################################################################
## Load Packages

library(dplyr)
library(stringr)
library(tidyverse)

dat.ready <- read_csv("Sample/Complete Sample/PA_Sample.2025Revision.csv")

################################################################################
## Clean and standardize identifiers and key variables

dat_fixed <- dat.ready %>%
  mutate(
    # Ensure Year is numeric (important for Year + 1 comparisons)
    Year = as.numeric(as.character(Year)),
    
    # Ensure Status is numeric (e.g., 0 = mover, 1 = resident)
    Status = as.numeric(Status),
    
    # Extract the true individual bird ID
    # If BirdID contains an underscore (e.g., "123_2022"),
    # keep only the portion before the underscore
    # Otherwise, keep BirdID as-is
    TrueBirdID = ifelse(
      str_detect(BirdID, "_"),
      str_extract(BirdID, "^[^_]+"),
      as.character(BirdID)
    )
  )

################################################################################
## Identify individuals that transition from mover to resident
## between consecutive years

mover_to_resident <- dat_fixed %>%
  
  # Sort data by individual and year to ensure correct temporal order
  arrange(TrueBirdID, Year) %>%
  
  # Perform operations within each individual bird
  group_by(TrueBirdID) %>%
  
  mutate(
    # Status in the following year
    next_status = lead(Status),
    
    # Year corresponding to the following observation
    next_year = lead(Year)
  ) %>%
  
  # Retain cases where:
  # - bird is a mover in the current year (Status == 0)
  # - bird is a resident in the following year (next_status == 1)
  # - years are consecutive (next_year == Year + 1)
  filter(Status == 1 & next_status == 0 & next_year == Year + 1) %>%
  
  # Remove grouping structure
  ungroup() %>%
  
  # Keep one row per bird that exhibits this transition
  distinct(TrueBirdID)

################################################################################
## Inspect results

# View BirdIDs that transitioned from mover to resident
mover_to_resident

# Count number of birds that transitioned from mover to resident
n_mover_to_resident <- nrow(mover_to_resident)
n_mover_to_resident

################################################################################
## Identify individuals with more than two years of data

two_year_birds <- dat.ready %>%
  group_by(BandID) %>%
  summarise(
    n_years = n_distinct(Year),
    years = paste(sort(unique(Year)), collapse = ", ")
  ) %>%
  filter(n_years >= 2)

two_year_birds
  

################################################################################
## Proportions of Movers and Residents Infected with LPDV

## Movers
num <- dat.ready %>%
       dplyr::filter(LPDV == 1, Status == 1) %>%
       dplyr::distinct(BandID) %>%
       nrow()
 
   den <- dat.ready %>%
       dplyr::filter(Status == 1) %>%
       dplyr::distinct(BandID) %>%
       nrow()
 
prop <- num / den
prop


## Residents
num <- dat.ready %>%
       dplyr::filter(LPDV == 1, Status == 0) %>%
       dplyr::distinct(BandID) %>%
       nrow()
 
   den <- dat.ready %>%
       dplyr::filter(Status == 0) %>%
       dplyr::distinct(BandID) %>%
      nrow()

  prop <- num / den
  prop

################################################################################
###############################################################################X
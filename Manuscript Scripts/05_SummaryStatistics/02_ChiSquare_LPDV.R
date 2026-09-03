#'---
#' title: Spring Movements of Wild Turkeys in Pennsylvania
#' author: K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: 
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script creates models to estimate the probability of an individual being a mover
#' **Last Updated**: 20 February 2026

################################################################################
## Load Packages

library(tidyverse)
library(partR2)

################################################################################
## LPDV by Movement Status Significance Test

# Contains the 5 individuals where winter ranges didn't converge
all_sample <- read.csv("Sample/Complete Sample/PA_Sample.2025Revision.csv") %>%
  dplyr::select(-LPDV)

disease <- read.csv("Data Management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/DiseaseStatus.csv") 

  all_sample <- left_join(
    all_sample,
    disease,
    by = "BandID"
  ) %>%
    distinct(BirdID, .keep_all = T)
  

# Chi-squared test to see if LPDV presence varies significantly by movement status
tab <- table(all_sample$Status, all_sample$LPDV)
chisq.test(tab)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab
# X-squared = 11.893, df = 1, p-value = 0.0005

################################################################################
## Proportion Infected by Movement Status

all_sample %>%
  group_by(BandID) %>%
  summarise(
    Status = first(Status),
    LPDV = max(LPDV, na.rm = TRUE)
  ) %>%
  group_by(Status) %>%
  summarise(
    n_BandID = n(),
    n_LPDV_positive = sum(LPDV == 1, na.rm = TRUE),
    percent_LPDV = 100 * n_LPDV_positive / n_BandID
  )

################################################################################
###############################################################################X
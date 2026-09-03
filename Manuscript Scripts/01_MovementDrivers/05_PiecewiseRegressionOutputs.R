#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script organizes piecewise regression outputs and creates mover and resident sample sizes.
#' **Last Updated**: 5 June 2026
#' # Need to write csvs for residents and movers separately for analyses and need to calculate the mean spring movement initiation date for each year

################################################################################
## Load Packages

library(tidyverse)
library(lubridate)

################################################################################
## Read in Data

# Read piecewise regression model parameter estimates
params <- read_csv(
  "Data management/Csvs/Piecewise Regression/Model Parameters/best_mod_params_2025Revision.csv"
)

# Read LPDV disease status data
virus <- read_csv(
  "Data management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/DiseaseStatus.csv"
) 

# Read capture data, create a capture date column
caps <- read_csv(
  "Data management/Csvs/Raw/Captures/captures_pa.csv"
) %>%
  dplyr::mutate(
    CaptureDate = make_date(captyr, captmo, captday)
  ) %>%
  dplyr::rename(
    BandID = bandid,
    Age = age,
    WMU = studyarea
  )

# Read mortality data and create mortality date column
morts <- read_csv("Data Management/Csvs/Raw/Mortalities/mortalities_pa.csv") %>%
  dplyr::mutate(
    MortDate = make_date(estmortyr, estmortmo, estmortday)
  ) %>%
  dplyr::rename(BandID = bandid)

################################################################################
## Clean Model Output

# Remove incomplete model parameter records
df_clean <- params %>%
  dplyr::filter(!is.na(id), !is.na(name))

# Extract numeric BandID from the model ID field
sample_all <- df_clean %>%
  dplyr::mutate(
    BandID = as.numeric(str_remove(str_sub(id, 1, 5), "_"))
  )

################################################################################
## Merge Sample, Viral, and Capture Data

# Merge disease information with modeled individuals.
sample_virus <- right_join(virus, sample_all, by = "BandID") %>%
  dplyr::mutate(
    Yr = str_split_fixed(id, "_", 2)[, 2],
    
    # Correct a formatting issue where 2024 was truncated to 202 (Should be addressed)
    Yr = ifelse(Yr == 202, 2024, Yr)
  )

# Merge capture information with disease/model data
sample_caps <- right_join(caps, sample_virus, by = "BandID") %>%
  dplyr::select(
    BandID,
    id,
    Age,
    WMU,
    captyr,
    Yr,
    name,
    mean,
    upper,
    lower,
    LPDV
  ) %>%
  dplyr::mutate(
    captyr = as.numeric(captyr),
    Yr = as.numeric(Yr),
    
    # Years elapsed between capture and modeled year
    yrsincecap = Yr - captyr,
    
    # Convert age classes to binary
    Age = case_when(
      Age == "J" ~ 1,   
      Age == "A" ~ 0,   
      TRUE ~ NA_real_
    ),
    
    # Juveniles become adults after their first year
    Age = ifelse(Age == 1 & yrsincecap >= 1, 0, Age)
  )

################################################################################
# Identify Movers

# Birds are classified as movers if:
# 1. Both stationary (int_1) and movement (int_3) intercepts exist
# 2. Difference between movement and stationary intercepts exceeds 2.5

mover_ids <- df_clean %>%
  dplyr::group_by(id) %>%
  dplyr::filter(
    any(name == "int_1"),
    any(name == "int_3")
  ) %>%
  summarize(
    movement = mean(mean[name == "int_3"], na.rm = TRUE) -
      mean(mean[name == "int_1"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(movement > 2.5) %>%
  pull(id)

################################################################################
# Create a Mover Only Dataset

# Retain only movers and their estimated change points
mover_only <- sample_caps %>%
  dplyr::filter(
    id %in% mover_ids,
    name %in% c("cp_1", "cp_2")
  ) %>%
  dplyr::rename(BirdID = id) %>%
  
  # Convert change points from long to wide format
  pivot_wider(
    id_cols = c(BandID, BirdID, Age, captyr, Yr, LPDV, WMU),
    names_from = name,
    values_from = c(mean, upper, lower)
  ) %>%
  
  # Convert modeled day-of-year estimates to calendar dates
  dplyr::mutate(
    ChangePoint_1 = as.Date(
      purrr::map_dbl(mean_cp_1, 1) - 1,
      origin = paste0(Yr, "-01-01")
    ),
    ChangePoint_2 = as.Date(
      purrr::map_dbl(mean_cp_2, 1) - 1,
      origin = paste0(Yr, "-01-01")
    )
  ) %>%
  
  # Create final movement dataset
  transmute(
    BandID,
    BirdID,
    Age,
    Year = Yr,
    LPDV,
    WMU,
    ChangePoint_1,
    ChangePoint_2,
    DayOfYear = yday(ChangePoint_1),
    Status = "1"
  )

################################################################################
# Remove Birds Initiating Movement Immediately After Capture

# Extract capture dates
capture_dates <- caps %>%
  dplyr::select(BandID, CaptureDate)

# Calculate days between capture and movement initiation
mover_only <- mover_only %>%
  left_join(capture_dates, by = "BandID") %>%
  dplyr::mutate(
    DaysPostCapture = as.numeric(ChangePoint_1 - CaptureDate)
  )

# Examine distribution of movement initiation timing
summary(mover_only$DaysPostCapture)

# Remove birds beginning movement within 14 days of capture,
# reducing potential capture-related behavioral effects
mover_only_filtered <- mover_only %>%
  dplyr::filter(DaysPostCapture > 14)

# Number removed
n_removed <- nrow(mover_only) - nrow(mover_only_filtered)
n_removed

################################################################################
# Remove Birds that Died During Their Movement Period

# Add mortality information
mover_only_filtered <- mover_only_filtered %>%
  left_join(
    morts %>%
      dplyr::select(BandID, MortDate),
    by = "BandID"
  )

# Identify birds dying between movement initiation and termination
dead_during_movement <- mover_only_filtered %>%
  filter(
    !is.na(MortDate),
    MortDate >= ChangePoint_1,
    MortDate <= ChangePoint_2
  ) %>%
  pull(BirdID) %>%
  unique()

length(dead_during_movement)

# Remove those birds
mover_only_filtered <- mover_only_filtered %>%
  dplyr::filter(!BirdID %in% dead_during_movement)

################################################################################
# Remove Unrealistically Long Movement Periods

# # Calculate movement duration
# mover_only_filtered <- mover_only_filtered %>%
#   mutate(
#     MovementDuration = as.numeric(ChangePoint_2 - ChangePoint_1)
#   )
# 
# summary(mover_only_filtered$MovementDuration)
# 
# # Exclude birds whose movement period exceeded 30 days
# mover_only_filtered <- mover_only_filtered %>%
#   filter(MovementDuration <= 30)
# 
# # Number removed 
# n_removed_long <- sum(
#   mover_only_filtered$MovementDuration > 30,
#   na.rm = TRUE
# )

################################################################################
# Create a Complete Sample Containing Movers and Residents

# Create one record per bird and classify movement status
birdlist <- sample_caps %>%
  dplyr::select(id, BandID, Age, Yr, LPDV) %>%
  distinct() %>%
  dplyr::mutate(
    Status = ifelse(id %in% mover_ids, 1, 0)
  ) %>%
  dplyr::rename(BirdID = id)

# Capture dates.
capture_dates <- caps %>%
  dplyr::select(BandID, CaptureDate)

# Create a resident dataset
# Residents receive a fixed spring interval for comparison
resident_only <- birdlist %>%
  dplyr::filter(Status == 0) %>%
  left_join(capture_dates, by = "BandID") %>%
  transmute(
    BandID,
    BirdID,
    Age,
    Year = Yr,
    LPDV,
    CaptureDate,
    
    # Fixed spring window
    ChangePoint_1 = make_date(Yr, 2, 7),
    ChangePoint_2 = make_date(Yr, 4, 28),
    
    DayOfYear = yday(make_date(Yr, 2, 7)),
    Status = "0"
  )

################################################################################
# Plot Spring Movement Initiation by WMU

# Define custom colors for WMUs
custom_palette <- c(
  "#fbb4ae",
  "#b3cde3",
  "#ccebc5",
  "#decbe4"
)

# Recalculate day of year after filtering
mover_only_filtered <- mover_only_filtered %>%
  dplyr::mutate(
    DayOfYear = yday(ChangePoint_1)
  )

# Boxplot showing variation in movement initiation dates among WMUs
ggplot(
  mover_only_filtered,
  aes(
    x = WMU,
    y = DayOfYear,
    fill = WMU
  )
) +
  geom_boxplot(
    alpha = 0.7,
    color = "black",
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.2,
    size = 2,
    alpha = 0.6,
    color = "black"
  ) +
  scale_fill_manual(values = custom_palette) +
  labs(
    x = "Wildlife Management Unit",
    y = "Spring Movement Initiation (Day of Year)"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(
      size = 12,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 12,
      face = "bold",
      margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      size = 10,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 10,
      face = "bold"
    )
  )

################################################################################
## Combine Datasets

# Combine movers and residents into a single analysis dataset
combined_data <- bind_rows(
  mover_only_filtered,
  resident_only
)

# Ensure WMU is populated for all birds
combined_data_wmu <- combined_data %>%
  left_join(
    caps %>% dplyr::select(BandID, WMU),
    by = "BandID"
  ) %>%
  dplyr::mutate(
    WMU = coalesce(WMU.x, WMU.y)
  ) %>%
  dplyr::select(-WMU.x, -WMU.y, -DaysPostCapture, -MortDate) %>%
  distinct() %>%
  arrange(BirdID) 

################################################################################
## Export Data

# Export final dataset containing both movers and residents
write_csv(
  combined_data_wmu,
  "Sample/Complete Sample/PA_Sample.2025Revision.csv"
)

################################################################################
###############################################################################X
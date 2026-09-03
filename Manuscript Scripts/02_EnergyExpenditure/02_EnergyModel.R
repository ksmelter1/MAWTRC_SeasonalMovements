#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: 
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script creates models for movement-related energy expenditure
#' **Last Updated**: 19 February 2026

################################################################################
## Load Packages and Data

library(glmmTMB)
library(tidyverse)
library(broom.mixed)
library(ggpubr)
library(patchwork)
library(ggeffects)

# Combined csv with all ACC data
 dat <- read_csv("Data Management/Csvs/ODBA_2025ModelData/ODBA_Data_2025Revision.csv") %>%
   dplyr::mutate(Bird_Year = BirdID) 

################################################################################
## Add in mover column to ODBA data 

# Read in sample csv
pa.sample <- read.csv("Sample/Complete Sample/Manuscript/PA_Sample.2025Revision.csv") 

# Select Bird_Year and status columns 
pa.sample <- pa.sample %>%
  dplyr::rename(Bird_Year = BirdID) %>%
  dplyr::select(Bird_Year, Status, Age) 

# Join sample and ACC data  
dat <- right_join(pa.sample, dat, by = "Bird_Year")

################################################################################
## Add in a covariate to account for if a bird nested the previous year

# Nest table
nests <- read_csv("Data Management/Csvs/Pennsylvania/Processed/Nests/Nests/Pennsylvania/20260325_CleanedNests_PA_2022_2023_2024_2025.csv")

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

# Create a lookup table: birds who nested in the **previous year**
nested_lookup <- nests_processed %>%
  transmute(BirdID = BirdID_next, nested_last_year = 1)

# Merge with dat.ready
dat.ready <- dat %>%
  left_join(nested_lookup, by = "BirdID") %>%
 dplyr::mutate(nested_last_year = replace_na(nested_last_year, 0)) 

# Read in LPDV data
disease <- read_csv("Data Management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/DiseaseStatus.csv") %>%
  distinct(BandID, .keep_all = T)

# Convert to character
disease$BandID <- as.character(disease$BandID)

# Create a BandID column in dat.ready
dat.ready$BandID <- sub("_.*", "", dat.ready$Bird_Year)

# Merge LPDV data with movement data object
dat.ready <- left_join(dat.ready, disease, by = "BandID") 

# Use the mean precipitation and the mean temperature per day
dat.ready <- dat.ready %>%
  dplyr::group_by(Bird_Year, Date) %>%
  dplyr::mutate(
    precip = mean(precip, na.rm = TRUE),
    tmin   = mean(tmin, na.rm = TRUE)
  ) %>%
  ungroup()

# Rename Class to Landuse
dat <- dat.ready %>%
  dplyr::rename(Landuse = Class)  %>%
  dplyr::mutate(BandID = str_extract(Bird_Year, "^[^_]+"))

# Make Landuse a factor
dat$Landuse <- factor(dat$Landuse)

# Set Hardwood as the reference level
dat$Landuse <- relevel(dat$Landuse, ref = "Hardwood")

# Remove observations with the other category
dat <- dat %>% filter(Landuse != "Other") 

# Check distribution of mean ODBA
hist(dat$mean_odba)

# Scale weather variables
dat$precip <- as.numeric(scale(dat$precip))
dat$tmin <- as.numeric(scale(dat$tmin))


# Constrain resident ACC data between Julian day 82 and 96
dat_filtered <- dat %>%
  dplyr::mutate(
    Date = as.Date(Date),
    Julian = as.numeric(format(Date, "%j"))
  ) %>%
  dplyr::filter(!is.na(Status)) %>%
  dplyr::filter(
    Status != 0 |
      (Status == 0 & Julian >= 82 & Julian <= 96)
  )
 
 # Ensure data is sorted correctly
 dat_filtered.a <- dat_filtered %>%
   arrange(Bird_Year, Date) %>%
   dplyr::group_by(Bird_Year) %>%
   dplyr::mutate(
     # Create a sequential factor for time within each individual
     time_bin = factor(row_number())
   ) %>%
   ungroup() %>%
   dplyr::filter(Age == "0")
 
 # Ensure data is sorted correctly
 dat_filtered.j <- dat_filtered %>%
   arrange(Bird_Year, Date) %>%
   dplyr::group_by(Bird_Year) %>%
   dplyr::mutate(
     # Create a sequential factor for time within each individual
     time_bin = factor(row_number())
   ) %>%
   ungroup() %>%
   dplyr::filter(Age == "1")

 ###############################################################################
 ## Process Data for Adult and Juvenile Models
 
 # Exclude individuals with less than 10 datapoints
 # This is needed for convergence of the autoregressive term
 dat_filtered.a<- dat_filtered.a %>%
   dplyr::group_by(Bird_Year) %>%
   dplyr::filter(n() >= 10) %>%
   ungroup()
 
 # Get sample size of movers and residents by age class
 dat_filtered.a %>%
   dplyr::group_by(Age, Status) %>%
   summarise(
     n_unique_Birds = n_distinct(Bird_Year)
   ) %>%
   ungroup()
 
 # Exclude individuals with less than 10 datapoints
 dat_filtered.j <- dat_filtered.j %>%
   dplyr::group_by(Bird_Year) %>%
   dplyr::filter(n() >= 10) %>%
   ungroup()
 
 # Get sample size of movers and residents by age class
 dat_filtered.j %>%
   dplyr::group_by(Age, Status) %>%
   summarise(
     n_unique_Birds = n_distinct(Bird_Year)
   ) %>%
   ungroup()
 
 
 ###############################################################################
 ## Adult Model
 
 # Generalized linear mixed effects model
 # Birds that were traveling through hardwood, not infected with LPDV and didn't nest the prior yr are the reference category
 # Random effects for WMU and BandID
 # Autoregressive error term for time to account for temporal autocorrelation

hab_ar1.a <- glmmTMB(
  mean_odba ~ Landuse + precip + tmin + Status + LPDV + nested_last_year +
    Status * Landuse + Status * precip + Status * tmin + Status*nested_last_year +
    LPDV * Status +
   (1 | BandID) + ar1(time_bin + 0 | Bird_Year),
  family = Gamma(link = "log"),
  data = dat_filtered.a)

summary(hab_ar1.a)
confint(hab_ar1.a, level = 0.95)
VarCorr(hab_ar1.a)

################################################################################
## Juvenile Model

# Generalized linear mixed effects model
# Birds that were traveling through hardwood, not infected with LPDV are the reference category
# Random effects for WMU and BandID
# Autoregressive error term for time to account for temporal autocorrelation

hab_ar1.j <- glmmTMB(
  mean_odba ~ Landuse + precip + tmin + Status + LPDV +
    Status * Landuse + Status * precip + Status * tmin +
    LPDV * Status +
    (1|BandID) + ar1(time_bin + 0 | Bird_Year),
  family = Gamma(link = "log"),
  data = dat_filtered.j
)

summary(hab_ar1.j)
confint(hab_ar1.j, level = 0.95)
VarCorr(hab_ar1.j)

################################################################################
## Visualize Data

# Obtain coefficients from adult model
hab_ar1.a_tidy <- tidy(hab_ar1.a, effects = "fixed", conf.int = TRUE, conf.level = 0.95) %>%
  filter(term != "(Intercept)") %>%
  mutate(Age = "Adult")

# Obtain coefficients from juvenile model
hab_ar1.j_tidy <- tidy(hab_ar1.j, effects = "fixed", conf.int = TRUE, conf.level = 0.95) %>%
  filter(term != "(Intercept)") %>%
  mutate(Age = "Juvenile")

# Create dataframe of coefficients and rename variables
combined_tidy <- bind_rows(hab_ar1.a_tidy, hab_ar1.j_tidy) %>%
  mutate(term_label = recode(term,
                             "LandusePasture" = "Pasture",
                             "LanduseCrop" = "Crop",
                             "LanduseConifer" = "Conifer",
                             "LanduseDeveloped" = "Developed",
                             "precip" = "Precip.",
                             "tmin" = "Min. Temp.",
                             "Status" = "Mover",
                             "nested_last_year" = "Nested Prior Yr.",
                             "LPDV" = "LPDV",
                             # Interactions
                             "LandusePasture:Status" = "Mover * Pasture",
                             "LanduseCrop:Status" = "Mover * Crop",
                             "LanduseConifer:Status" = "Mover * Conifer",
                             "LanduseDeveloped:Status" = "Mover * Developed",
                             "precip:Status" = "Mover * Precip.",
                             "tmin:Status" = "Mover * Min. Temp.",
                             "Status:nested_last_year" = "Mover * Nested Prior Yr.",
                             "Status:LPDV" = "Mover * LPDV",
                             .default = term))

# Assign variables to covariate classes
combined_tidy <- combined_tidy %>%
  mutate(Scale = case_when(
    term_label %in% c("Pasture", "Crop", "Conifer", "Developed",
                      "Mover * Pasture", "Mover * Crop", "Mover * Conifer", "Mover * Developed") ~ "Landscape",
    term_label %in% c("Precip.", "Min. Temp.",
                      "Mover * Precip.", "Mover * Min. Temp.") ~ "Weather",
    term_label %in% c("Nested Prior Yr.", "Mover * Nested Prior Yr.",
                      "LPDV", "Mover * LPDV",
                      "Mover") ~ "Individual",
    TRUE ~ NA_character_
  ))

# Organize variables to be plotted
combined_tidy <- combined_tidy %>%
  mutate(term_label = factor(term_label,
                             levels = c(
                               
                               # Individual scale (top section of plot)
                               "Mover * Precip.",
                               "Precip.",
                               
                               "Mover * Min. Temp.",
                               "Min. Temp.",
                               
                               # Landscape scale (bottom section of plot)
                               "Mover * Crop",
                               "Crop",
                               
                               "Mover * Pasture",
                               "Pasture",
                               
                               "Mover * Conifer",
                               "Conifer",
                               
                               "Mover * Developed",
                               "Developed",
                               
                               "Mover * Nested Prior Yr.",
                               "Nested Prior Yr.",
                               
                               "Mover * LPDV",
                               "LPDV",
                              
                               # Mover at very top
                               "Mover"
                             )))



################################################################################
## Create Beta Plot

faceted_plot <- ggplot(combined_tidy, 
                       aes(x = estimate, 
                           y = term_label, 
                           color = Scale, 
                           shape = Age)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7),
                 height = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  labs(
    x = "Estimate",
    y = NULL,
    color = "Predictor Scale",
    shape = "Age Class"
  ) +
  scale_color_manual(
    values = c("Weather" = "#66c2a5",
               "Landscape" = "#fc8d62",
               "Individual" = "#8da0cb")
  ) +
  scale_shape_manual(
    values = c("Adult" = 16, "Juvenile" = 17)
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",      
    legend.box.just = "center",
    axis.text.x = element_text(hjust = 1),
    axis.title.x = element_text(face = "bold"),
  )

print(faceted_plot)


################################################################################
## Prediction Plots

# Create predictions for adult
pred_odba_adult <- ggpredict(hab_ar1.a, terms = c("Landuse", "Status")) %>%
  as.data.frame() %>%
  dplyr::mutate(Age = "Adult") %>%
  dplyr::rename(Status = group)

# Create predictions for juvenile
pred_odba_juv <- ggpredict(hab_ar1.j, terms = c("Landuse", "Status")) %>%
  as.data.frame() %>%
  dplyr::mutate(Age = "Juvenile") %>%
  dplyr::rename(Status = group)

# Combine predictions
pred_odba_combined <- bind_rows(pred_odba_adult, pred_odba_juv)


################################################################################
## Create Plot (Figure 3)

ggplot(pred_odba_combined, aes(x = x, y = predicted, color = Status, shape = Age)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.7), width = 0.2) +
  labs(
    x = "Land use",
    y = "Predicted ODBA",
    color = "Status",
    shape = "Age Class"
  ) +
  scale_color_manual(
    values = c("0" = "#1b9e77", "1" = "#d95f02"),
    labels = c("Resident", "Mover")
  ) +
  scale_shape_manual(values = c("Adult" = 16, "Juvenile" = 17)) +
  guides(
    shape = guide_legend(order = 1),  
    color = guide_legend(order = 2)   
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "center",
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

################################################################################
###############################################################################X
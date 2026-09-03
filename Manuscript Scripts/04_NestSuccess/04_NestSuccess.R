#'---
#' title: Seasonal Movements of Wild Turkeys in Pennsylvania
#' author: K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'  
#' **Purpose**: This script creates nest success models using the complementary log-log link
#' **Last Updated**: 1/1/26


################################################################################
## Load Packages and Data

library(glmmTMB)
library(tidyverse)
library(GGally)
library(broom.mixed)
library(lubridate)
library(sf)

# DBBMM data from prior script
load("Data Management/RData/Pennsylvania/Clutch Size/01_ClutchSizePredictors_2025Revision.RData")

# Clutch Size Data from chapter 1
pa.nests <- read_csv("Data Management/Csvs/Pennsylvania/Processed/Nests/Nests/Pennsylvania/20260325_CleanedNests_PA_2022_2023_2024_2025.csv") 
pa.nests

################################################################################
##  Data Prep

# Convert BandID to character
pa.nests.ready <- pa.nests %>%
  dplyr::rename(Clutch = `ClutchSize`,
                Yr = SurveyYr) %>%
  dplyr::select(BandID, NestID, Yr, Clutch, NestFate_Binary, WMU) %>%
  dplyr::mutate(BandID = as.character(BandID)) 

# Reduce before joining
pa.nests.ready %>%
  dplyr::select(BandID, Yr, NestID, Clutch) %>%
  arrange(BandID, Yr)

# Separate BandID and Yr columns to allow for data merge 
dat.ready <- dat.ready %>%
  st_drop_geometry() %>%
  dplyr::mutate(Bird_Year = BirdID) %>%
  separate(BirdID, into = c("BandID", "Yr"), sep = "_", convert = TRUE) %>%
  dplyr::mutate(BandID = as.character(BandID)) %>%
  dplyr:: mutate(
    Pasture = as.numeric(scale(Pasture)),
    Crop = as.numeric(scale(Crop)),
    Developed   = as.numeric(scale(Developed)),
    Hardwood    = as.numeric(scale(Hardwood)),
    Conifer     = as.numeric(scale(Conifer)),
    Primary     = as.numeric(scale(Primary)),
    Secondary   = as.numeric(scale(Secondary)),
    odba = as.numeric(scale(mean_odba_dbbmms)),
    TMin = as.numeric(scale(tmin_avg_degC)),
    Precip = as.numeric(scale(prcp_mean_mm))) %>%
  dplyr::mutate(
    Age_binary = ifelse(Age == "A", 0, 1)
  )

# Merge by BandID and Yr columns 
# Accounts for differences in individual behavior between years
dat.ready.nest <- left_join(dat.ready, pa.nests.ready, by = c("BandID", "Yr"))

# Rename WMU column and remove unneeded column 
dat.ready <- dat.ready.nest %>% 
  dplyr::rename(WMU = WMU.x) %>%
  dplyr::select(-WMU.y)

# Read in PA sample
pa.sample <- read.csv("Sample/Complete Sample/Manuscript/PA_Sample.2025Revision.csv")

# Create Bird_Year column
pa.sample <- pa.sample %>%
  dplyr::mutate(Bird_Year = BirdID) %>%
  dplyr::select(Bird_Year)

# Drop NA NestIDs
# Just means the bird didn't nest
dat.ready <- right_join(pa.sample,dat.ready, by = "Bird_Year") %>%
  drop_na(NestID)

# Filter to only include an individual's first nesting attempt
dat.ready <- dat.ready %>%
  dplyr::filter(grepl("_1$", NestID)) %>%
  distinct(Bird_Year, .keep_all = T)

################################################################################
## Apparent Nest Success by Movement Status

# Overall
nest_success_status <- dat.ready %>%
  dplyr::group_by(Status) %>%
  summarise(
    total_nests = n(),
    successful_nests = sum(NestFate_Binary == 1, na.rm = TRUE),
    apparent_nest_success = successful_nests / total_nests
  )

nest_success_status

# Juvenile movers
dat.ready %>%
  dplyr::filter(Age == 1,
         Status == 1,
         NestFate_Binary == 1) %>%
  summarise(successful_juvenile_mover_nests = n())

################################################################################
## Datasets for Modeling 

# Separate adult age class from juveniles
dat.ready.a <- dat.ready %>%
  dplyr::filter(Age == "0")
dat.ready.a %>%
  dplyr::group_by(Status) %>%
  summarise(unique_bird_years = n_distinct(Bird_Year))

# Separate juvenile dataset from adults
dat.ready.j <- dat.ready %>%
  dplyr::filter(Age == "1")
dat.ready.j %>%
  dplyr::group_by(Status) %>%
  summarise(unique_bird_years = n_distinct(Bird_Year))

################################################################################
## Nest Success

# Generalized linear mixed effects model
# Cloglog regression
# Random effect for WMU
# Mover covariate not included due to small sample size

################################################################################
## Adult Model

hab.a <- glmmTMB(
  NestFate_Binary ~ Pasture + Crop + Conifer + Developed + Primary +
    + Precip + TMin + nested_last_year + LPDV  + odba + Status,
  family = binomial(link = "cloglog"),
  data = dat.ready.a
)

summary(hab.a)
confint(hab.a, level = 0.95)

################################################################################
## Juvenile Model


hab.j <- glmmTMB(
  NestFate_Binary ~ Crop + Pasture + Conifer + Developed + Primary +
    + Precip + TMin + LPDV  + odba + Status,
  family = binomial(link = "cloglog"),
  data = dat.ready.j
)

summary(hab.j)
confint(hab.j, level = 0.95)


################################################################################
## Visualize Data

# Extract effects from adult model
hab_a_tidy <- broom.mixed::tidy(hab.a, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = str_replace_all(term, ":", " x "),
    Age = "Adult"
  )

# Extract effects from juvenile model
hab_j_tidy <- broom.mixed::tidy(hab.j, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = str_replace_all(term, ":", " x "),
    Age = "Juvenile"
  )

# Combine both into one dataframe
combined_tidy <- bind_rows(hab_a_tidy, hab_j_tidy)

# Create labels for data
label_map.age <- c(
  # Main effects
  "odba" = "ODBA",
  "Pasture" = "Prop. Pasture",
  "Crop" = "Prop. Crop",
  "Conifer" = "Prop. Conifer",
  "Developed" = "Prop. Developed",
  "Primary" = "Primary Road",              
  "TMin" = "Min.Temp.",
  "Precip" = "Precip.",
  "nested_last_year" = "Nested Prior Yr.",
  "Status" = "Mover",
  "LPDV" = "LPDV",
  "AgeJ" = "Juvenile",
  
  # Interactions
  "odba x Status" = "Mover * ODBA",
  "Pasture x Status" = "Mover * Prop. Pasture ",
  "Crop x Status" = "Mover * Prop. Crop ",
  "Conifer x Status" = "Mover * Prop. Conifer",
  "Developed x Status" = "Mover * Prop. Developed",
  "Primary x Status" = "Mover * Primary Road",   
  "TMin x Status" = "Mover * Min.Temp.",
  "Precip x Status" = "Mover * Precip.",
  "nested_last_year x Status" = "Mover * Nested Prior Yr.",
  "Status x LPDV" = "Mover * LPDV",
  "Status x Primary" = "Mover * Primary Road",   
  "AgeJ x Status" = "Mover * Juvenile"
)

# Create order for data to be plotted
env_order <- c(
  "Mover * Precip.",
  "Precip.",
  "Mover * Min.Temp.",
  "Min.Temp.",
  "Mover * Prop. Pasture",
  "Prop. Pasture",
  "Mover * Prop. Crop",
  "Prop. Crop",
  "Mover * Prop. Hardwood",
  "Prop. Hardwood",
  "Mover * Prop. Conifer",
  "Prop. Conifer",
  "Mover * Prop. Developed",
  "Prop. Developed",
  "Mover * Primary Road",
  "Primary Road",
  "Mover * ODBA",
  "ODBA",
  "Mover * Nested Prior Yr.",
  "Nested Prior Yr.",
  "Mover * LPDV",
  "LPDV",
  "Mover * Juvenile",
  "Juvenile",
  "Mover"
)


# Apply same label mapping from your code
combined_tidy <- combined_tidy %>%
  mutate(term_label = recode(term, !!!label_map.age, .default = term),
         term_label = factor(term_label, levels = env_order))

# Assign predictor to class
combined_tidy <- combined_tidy %>%
  mutate(
    Scale = case_when(
      term_label %in% c("Prop. Pasture", "Prop. Crop", "Prop. Conifer", "Prop. Developed", "Prop. Hardwood",
                        "Primary Road", "Mover * Primary Road", "Mover * Prop. Agriculture",
                        "Mover * Prop. Conifer", "Mover * Prop. Developed", "Mover * Prop. Hardwood") ~ "Landscape",
      term_label %in% c("Precip.", "Min.Temp.", "Mover * Precip.", "Mover * Min.Temp.") ~ "Weather",
      term_label %in% c("ODBA", "Nested Prior Yr.", "Mover * ODBA", "Mover * Nested Prior Yr.",
                        "Mover", "LPDV", "Mover * LPDV", "Juvenile", "Mover * Juvenile") ~ "Individual",
      TRUE ~ NA_character_
    )
  )


################################################################################
## Create Beta Plot

faceted_plot <- ggplot(combined_tidy, 
                       aes(x = estimate, 
                           y = term_label, 
                           color = Scale, 
                           shape = Age)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.8),
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
    shape = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",      
    legend.box.just = "center",
    axis.text.x = element_text(hjust = 1),
    axis.title.x = element_text(face = "bold")
  )

print(faceted_plot)

################################################################################
###############################################################################X

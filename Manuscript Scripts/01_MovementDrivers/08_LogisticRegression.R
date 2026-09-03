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
#' **Last Updated**: 17 February 2026

################################################################################
## Load Packages and Data

library(glmmTMB)
library(dplyr)
library(purrr)
library(ggplot2)
library(GGally)
library(ggpubr)
library(sf)
library(tidyverse)

# Read in Data from the Winter home range script
load("Data Management/RData/Pennsylvania/01_MovementFactors_wAKDE.RData")

################################################################################
## Prepare Data

# Read in PA sample
pa.sample <- read_csv("Sample/Complete Sample/Manuscript/PA_Sample.2025Revision.csv")

# Keep only birds that are in birdlist
dat.ready <- dat.ready %>%
  semi_join(pa.sample, by = "BirdID") %>%
  distinct(BirdID, .keep_all = TRUE)

# Birds in pa.sample that are NOT in dat.ready
anti_join(pa.sample, dat.ready, by = "BirdID")

# Scale and convert predictors to numeric
dat.ready <- dat.ready %>%
  dplyr::mutate(
    Pasture = as.numeric(scale(Pasture)),
    Crop = as.numeric(scale(Crop)),
    Developed   = as.numeric(scale(Developed)),
    Hardwood    = as.numeric(scale(Hardwood)),
    Conifer     = as.numeric(scale(Conifer)),
    Primary     = as.numeric(scale(Primary)),
    Secondary   = as.numeric(scale(Secondary)),
    tmin   = as.numeric(scale(tmin_avg_degC)),
    precip  = as.numeric(scale(prcp_mean_mm)),
    nested_last = as.numeric(nested_last_year))


# Select continous predictors for correlation plotting
dat.habitat.preds <- dat.ready %>%
  dplyr::select(Crop, Pasture, Developed, Hardwood, Conifer, Primary, Secondary, precip, tmin, nested_last, LPDV, Age)

# Plot pairwise correlations
ggpairs(dat.habitat.preds)


################################################################################
## Fit Models

# Generalized linear mixed effects models
# Logit link is used, odds ratios can be calculated

# Regress status on age and habitat predictors
# Interactions account for differences in the probability of moving based on juvenile age class
# Random effect for WMU

# Separate juvenile and adult datasets for analysis
dat.ready.a <- dat.ready %>%
  dplyr::filter(Age == 0)
dat.ready.j <- dat.ready %>%
  dplyr::filter(Age == 1)

################################################################################
## Fit Adult Model

pred.Age.a <- glmmTMB(
  Status ~ nested_last + Conifer + Pasture + Crop + Developed + precip + tmin + Primary + LPDV,
  family = binomial(),
  data = dat.ready.a
)
summary(pred.Age.a)
confint(pred.Age.a, level = 0.95)

################################################################################
## Fit Juvenile Model

pred.Age.j <- glmmTMB(
  Status ~ Conifer + Pasture + Crop + Developed + precip + tmin + Primary + LPDV,
  family = binomial(),
  data = dat.ready.j
)
summary(pred.Age.j)
confint(pred.Age.j, level = 0.95)

################################################################################
## Visualize Data

# Updated helper function for 95% CI
extract_effects <- function(model, predictor_name) {
  ci <- confint(model, parm = predictor_name, level = 0.95)
  est <- coef(summary(model))$cond[predictor_name, "Estimate"]
  
  data.frame(
    Predictor = predictor_name,
    Estimate = est,
    CI_low = ci[1],
    CI_high = ci[2]
  )
}

# List of predictors in adult model
predictors.age.a <- c(
  "Conifer",
  "Pasture",
  "Crop",
  "Developed",
  "Primary",
  "tmin",
  "precip",
  "nested_last",
  "LPDV"
)

# List of predictors in juvenile model
predictors.age.j <- c(
  "Conifer",
  "Pasture",
  "Crop",
  "Developed",
  "Primary",
  "tmin",
  "precip",
  "LPDV"
)

# Apply functions to extract effects
effects_df.age.a <- map_df(predictors.age.a, ~ extract_effects(pred.Age.a, .x))
effects_df.age.j<- map_df(predictors.age.j, ~ extract_effects(pred.Age.j, .x))

# Add Age labels
effects_df.age.a <- effects_df.age.a %>%
  mutate(AgeClass = "Adult")
effects_df.age.j <- effects_df.age.j %>%
  mutate(AgeClass = "Juvenile")

# Combine effects into a single df
effects_df.age <- bind_rows(effects_df.age.a, effects_df.age.j)

# Compute Odds Ratios and CIs
effects_df.age <- effects_df.age %>%
  mutate(
    OR = exp(Estimate),
    OR_low = exp(CI_low),
    OR_high = exp(CI_high)
  )

# Apply labels to effects
label_map.age <- c(
  "nested_last" = "Nested Prior Yr.",
  "Hardwood" = "Prop. Hardwood",
  "Crop" = "Prop. Crop",
  "Pasture" = "Prop. Pasture",
  "Conifer" = "Prop. Conifer",
  "Developed" = "Prop. Developed",
  "tmin" = "Min. Temp.",
  "precip" = "Precip.",
  "Primary" = "Density of Primary Roads",
  "LPDV" = "LPDV"
)

# Create covariate classes
effects_df.age <- effects_df.age %>%
  mutate(
    Label = label_map.age[as.character(Predictor)],
    Scale = case_when(
      Predictor %in% c("Hardwood", "Pasture", "Crop", "Developed", "Conifer", "Primary") ~ "Landscape",
      Predictor %in% c("precip", "tmin") ~ "Weather",
      Predictor %in% c("nested_last", "LPDV") ~ "Individual",
      TRUE ~ NA_character_
    )
  )


# Define the order of labels: main effects first, interactions later
ordered_labels.age <- c(
"precip", "tmin", "Primary", "Developed", "Conifer", "Pasture", "Crop", "Hardwood","nested_last","LPDV", "Age" 
)

# Age and landscape effects- apply labelling 
effects_df.age <- effects_df.age %>%
  mutate(
    Label = label_map.age[as.character(Predictor)],
    Label = factor(Label, levels = label_map.age[ordered_labels.age]),
  )


################################################################################
## Plot Out Effects

# Make plot
faceted_plot <- ggplot(effects_df.age, 
                       aes(x = Estimate, 
                           y = Label, 
                           color = Scale, 
                           shape = AgeClass)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
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
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
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
###############################################################################X
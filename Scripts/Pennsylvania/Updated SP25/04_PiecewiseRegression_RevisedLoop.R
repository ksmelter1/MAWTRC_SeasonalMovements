#'---
#' title: Seasonal Movements of Wild Turkeys in the Mid-Atlantic Region
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script uses piecewise regression to identify behavioral transitions
#' **Last Updated**: 6/7/25
#' **Key Changes**: This script follows Wolfson et al. (2024)'s workflow using piecewise regression

############
## Notes ##
############

# If a bird moved greater than 3 km and didn't have an average daily displacement value less than 2.5 km on day 106 (April 15th) it dispersed
# I flagged birds that moved less than 1.3 km and didn't include them in the analysis 
# The 3 km is from Hayden et al. 1990 where average dispersal distance from adults was 3.2 km (They calculated it differently as it was 1990)
# April 15th is arbitrary however, it is an apriori estimate of when the majority of seasonal movements have ceased and nesting/prenesting behavior has begun
# 7 birds exhibited dispersal behavior after April 15th 
# Piecewise regression models for 25 hen-year datasets didn't converge as they contained an Rhat value greater than 1.1. It just means that they didn't disperse as a change point wasn't detected
# 11 hens were removed because they were not tested for LPDV

################################################################################
## Load Packages


# package names
packages<-c("tidyverse", 
            "here", 
            "mcp", 
            "lubridate", 
            "knitr", 
            "ezknitr", 
            "loo", 
            "flextable",
            "readr",
            "ggplot2",
            "foreach")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))


################################################################################
## Data Prep

# List all CSV files in the directory
csv_files <- list.files(path = "Data Management/Csvs/Hen_NSD_Data/Individual/", pattern = ".csv", full.names = TRUE)

# Extract BirdIDs from each file
bird_ids <- sapply(csv_files, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  unique(df$BirdID)[1]
})

# Create model syntax
# Fit two intercept models for each hen-year dataset
two_int<-list(nsd_plot_data~1,
              ~1)

int_mods<-list(two_int)
int_mod_vec<-c("two_int")

# Create objects to store the results
# This helps with the automation process
res<-data.frame(id=NA, year=NA, name=NA, mean=NA, lower=NA, upper=NA, Rhat=NA, n.eff=NA)
write_csv(res, 
          here("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Model Parameters/best_mod_params.csv"))

# Compare models in csv
model_comparison<-data.frame(id=NA, year=NA, 
                             two_int_loo=NA)
write_csv(model_comparison, 
          here("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Model Comparison/model_comparisons.csv"))



# Create a folder to save plots if it doesn't exist
if (!dir.exists("Data Management/Csvs/Hen_NSD_Plots")) {
  dir.create("Data Management/Csvs/Hen_NSD_Data_Plots")
}

# fit mcp models
out_mods<-list()
# Track skipped BirdIDs
skipped_birds <- c()

for (file in csv_files) {
  
  df <- read_csv(file)
  file_id <- unique(df$BirdID)[1]
  cat("Working on BirdID", print(file_id))
  
  out_mods <- foreach(mm = 1:length(int_mods), .packages = "mcp", .errorhandling = 'pass') %dopar% {
    tryCatch(
      mcp(model = int_mods[[mm]],
          data = df[, c("days_numeric", "nsd_plot_data")],
          par_x = "days_numeric",
          adapt = 10000,
          iter = 15000),
      error = function(e) NULL
    )
  }
  
  loo_list <- vector("list", length(out_mods))
  model_failed <- FALSE
  
  for (k in seq_along(out_mods)) {
    if (!is.null(out_mods[[k]])) {
      mod_summary <- as.data.frame(summary(out_mods[[k]]))
      out_mods[[k]]$rhat_fail <- any(mod_summary$Rhat > 1.1)
      
      if (!out_mods[[k]]$rhat_fail) {
        out_mods[[k]]$loo <- loo(out_mods[[k]])
        loo_list[[k]] <- out_mods[[k]]$loo$estimates["elpd_loo", "Estimate"]
      } else {
        model_failed <- TRUE
        break  
      }
    } else {
      model_failed <- TRUE
      break
    }
  }
  
  if (model_failed) {
    skipped_birds <- c(skipped_birds, file_id)
    next  
  }
  
  loo_vec <- unlist(loo_list)
  if (length(loo_vec) < length(int_mod_vec)) {
    loo_vec <- rep(-9999, length(int_mod_vec))
  }
  names(loo_vec) <- paste0(int_mod_vec, "_loo")
  
  mods <- as.data.frame(as.list(c(name = file_id, loo_vec)))
  best_mod_index <- which.max(loo_vec)
  best_mod <- out_mods[[best_mod_index]]
  params <- as.data.frame(summary(best_mod))
  params <- cbind.data.frame(name = file_id, params)
  write_csv(params, here("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Model Parameters/best_mod_params.csv"), append = TRUE)
  
  p <- plot(best_mod, q_fit = TRUE) +
    labs(y = "displacement in km",
         x = "Date",
         title = glue::glue("BirdID: {file_id}"))
  
  max_disp <- max(df$nsd_plot_data, na.rm = TRUE)
  post_day100_disp <- df %>% filter(days_numeric >= 106) %>% pull(nsd_plot_data)
  low_disp_after_100 <- any(post_day100_disp < 2.5 & post_day100_disp >= 1.3, na.rm = TRUE)
  
  root_dir <- "C:/Users/kjs7255/OneDrive - The Pennsylvania State University/Masters/Research/MS_DispersalMovements/MS_Chapter2/MAWTRC_SeasonalMovements/Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Piecewise Regression Plots"
  
  if (max_disp < 1.3) {
    save_dir <- file.path(root_dir, "Flagged Plots")
  } else if (low_disp_after_100) {
    save_dir <- file.path(root_dir, "Didn't Disperse")
  } else if (max_disp > 3) {
    save_dir <- file.path(root_dir, "Dispersed")
  } else {
    save_dir <- file.path(root_dir, "Didn't Disperse")
  }
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  ggsave(plot = p,
         filename = file.path(save_dir, paste0(file_id, ".png")),
         width = 8, height = 5)
  
  cat("Finished hen", print(file_id))
}

# Write skipped BirdIDs to a CSV
write_csv(data.frame(Skipped_BirdID = skipped_birds),
          here("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Model Parameters/skipped_birdids.csv"))


################################################################################
## Create a list of birds based on dispersal status-- complete sample

# Read skipped BirdIDs
skipped_df <- read.csv(here("Data Management/Csvs/Piecewise Regression/Model Parameters/skipped_birdids.csv"),
                       stringsAsFactors = FALSE)

# Create dataframe of skipped birds with Status = "N"
skipped_df$Status <- "N"
colnames(skipped_df)[1] <- "BirdID"  # Ensure column name matches

# Set folder paths
dispersed_path <- "Data Management/Csvs/Piecewise Regression/Piecewise Regression Plots/Model Output Plots/Dispersed/"
no_path <- "Data Management/Csvs/Piecewise Regression/Piecewise Regression Plots/Model Output Plots/Didn't Disperse/"

# Get file names
dispersed_files <- list.files(dispersed_path)
no_files <- list.files(no_path)

# Remove .png extension explicitly
dispersed_ids <- sub("\\.png$", "", dispersed_files)
no_ids <- sub("\\.png$", "", no_files)

# Create data frames
dispersed_df <- data.frame(BirdID = dispersed_ids, Status = "Y", stringsAsFactors = FALSE)
no_df <- data.frame(BirdID = no_ids, Status = "N", stringsAsFactors = FALSE)

# Combine all: dispersed, not dispersed, and skipped (as not dispersed)
sample <- rbind(dispersed_df, no_df, skipped_df)

# Remove duplicates in case a BirdID is in multiple categories (keep first occurrence)
sample <- sample[!duplicated(sample$BirdID), ]

# Extract bandid and convert to numeric
sample <- sample %>%
  dplyr::mutate("BandID" = str_sub(BirdID, 1,4)) %>%
  dplyr::mutate(BandID = as.numeric(BandID))

# Read in virus csv
virus <- read_csv("Data Management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/virus_raw.csv") %>%
  dplyr::rename("BandID" = bandid)
virus

# Merge virus and sample
# If a bird dispersed assign that bird a value of 1 in the status
sample.virus <- right_join(virus, sample) %>%
  dplyr::select(-REV) %>%
  dplyr::mutate(Status = if_else(Status == "Y", 1, 0))


################################################################################
## Output a csv of the complete sample 

# 25 bird-year datasets do not have any model estimates as their models didn't converge
# These birds didn't disperse

# Read in model parameter csv
params <- read_csv("Data Management/Csvs/Piecewise Regression/Model Parameters/best_mod_params.csv")
params

# Right_join and save new csv with change point estimates
# 11 hens were not tested for LPDV so they were removed from the analysis
# Only include observations from the first change point
dat4analysis_hr <- right_join(params, sample.virus) %>%
  dplyr::filter(!is.na(LPDV)) %>%
  dplyr::mutate(Yr = str_sub(BirdID,6,9)) %>%
  dplyr::select(BirdID, BandID, Yr, LPDV, everything())


# Read in capture data
caps <- read_csv("Data Management/Csvs/Raw/Captures/captures.csv") %>%
  dplyr::rename("BandID" = bandid)
caps

# Convert to numeric 
caps$captyr <- as.numeric(caps$captyr)
dat4analysis_hr$Yr <- as.numeric(dat4analysis_hr$Yr)

# Merge caps and dat4analysis_hr together
sample.caps <- right_join(caps, dat4analysis_hr) %>%
  dplyr::select(BandID, 
                BirdID,
                age,
                studyarea,
                captyr,
                Yr,
                name,
                mean,
                upper,
                lower,
                LPDV,
                Status)

# Create a years since capture column
sample.caps <- sample.caps %>%
  dplyr::mutate(yrsincecap = Yr-captyr)

# Assign Adult as the reference level
sample.caps$age <- ifelse(sample.caps$age == "J", 1, 
                           ifelse(sample.caps$age == "A", 0, NA))

# Dealing with scaling age ad hoc
# If the bird is an adult and the years since capture is >1 assign it as an adult
# If not keep the age as juvenile 
sample.caps$age <- ifelse(sample.caps$age == 1 & sample.caps$yrsincecap >= 1, 0, sample.caps$age)

# Output csv with all change points
# This will be used to answer the first research question about the factors influencing dispersal
write_csv(sample.caps, "Data Management/Csvs/Piecewise Regression/Model Parameters/dat4hr.csv")

# Output sample csv as a reference for future analysis without model results
sample.final <- sample.caps %>%
  dplyr::select(BirdID, BandID, Yr, age, studyarea, LPDV, Status)
write_csv(sample.final, "Sample/Complete Sample/PA_Sample.csv")


################################################################################
## Output a csv containing only the birds that dispersed

# Filter for only birds that dispersed (Status == "Y")
dispersed_only <- sample.caps %>%
  dplyr::filter(Status == "1")

# Only contain change point estimate 
dispersed_only <- dispersed_only %>%
  dplyr::filter(name == "cp_1") %>%
  dplyr::select(-yrsincecap)

table(dispersed_only$age)
table(dispersed_only$LPDV)

# Convert from calendar day to ymd using as.Date
dispersed_only <- dispersed_only %>%
  dplyr::mutate(
    ChangePoint = as.Date(mean - 1, origin = paste0(Yr, "-01-01")),
    upper_date = as.Date(upper - 1, origin = paste0(Yr, "-01-01")),
    lower_date = as.Date(lower - 1, origin = paste0(Yr, "-01-01"))
  ) %>%
  dplyr::select(-upper_date,
                -lower_date,
                -name,
                -lower,
                -upper, 
                -mean, 
                -captyr, 
                -Status) %>%
  dplyr::rename("Age" = age) %>%
  dplyr::rename("Year" = Yr) %>%
  dplyr::rename("StudyArea" = studyarea)


# Write csv containing all dispersed birds 
# This will be used for the second research question about energy expenditure during dispersal
write.csv(dispersed_only, "Sample/Dispersed Birds/birdlist.csv")

################################################################################
###############################################################################X

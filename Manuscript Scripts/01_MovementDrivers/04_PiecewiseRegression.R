#'---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script creates a pdf showing piecewise regression plots based on NSD data
#' **Last Updated**: 19 February 2026

################################################################################
## Load packages

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
            "foreach",
            "gridExtra",
            "grid")

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
csv_files <- list.files(path = "Data management/Csvs/Hen_NSD_Data/Individual/Individual Csvs/", pattern = ".csv", full.names = TRUE)

# Extract BirdIDs from each file
bird_ids <- sapply(csv_files, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  unique(df$BirdID)[1]
})

# Create model syntax
three_int<-list(nsd_plot_data~1,
                ~1,
                ~1)

int_mods<-list(three_int)
int_mod_vec<-c("three_int")

# Create objects to store the results
# This helps with the automation process
res<-data.frame(id=NA, name=NA, mean=NA, lower=NA, upper=NA, Rhat=NA, n.eff=NA)
write_csv(res, 
          here("Data management/Csvs/Piecewise Regression/Model Parameters/best_mod_params_2025Revision.csv"))

# Compare models in csv
model_comparison<-data.frame(BirdID=NA,  
                             three_int=NA,
                             loo=NA)
write_csv(model_comparison, 
          here("Data management/Csvs/Piecewise Regression/Model Parameters/Three Intercepts/model_comparisons_2025Revision.csv"))


# Create a folder to save plots if it doesn't exist
if (!dir.exists("Data management/Csvs/Hen_NSD_Data/NSD Plots/Piecewise Regression/")) {
  dir.create("Data management/Csvs/Hen_NSD_Data/NSD Plots/Piecewise Regression/")
}

# fit mcp models
out_mods<-list()

# Create a list to store all final plots for all birds
all_final_plots <- list()
skipped_birds <- c()

for (file in csv_files) {
  
  df <- read_csv(file)
  file_id <- unique(df$BirdID)[1]
  cat("Working on BirdID", file_id, "\n")
  
  # Fit models
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
  
  # Evaluate models
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
  
  # Save model parameters
  params <- as.data.frame(summary(best_mod))
  params <- cbind.data.frame(name = file_id, params)
  write_csv(params, here("Data management/Csvs/Piecewise Regression/Model Parameters/best_mod_params_2025Revision.csv"), append = TRUE)
  
  # Save model comparison
  write_csv(as.data.frame(mods), here("Data management/Csvs/Piecewise Regression/Model Parameters/Three Intercepts/model_comparisons_2025Revision.csv"), append = TRUE)
  
  plot_list <- list()
  loo_labels <- list()
  
  for (k in seq_along(out_mods)) {
    if (!is.null(out_mods[[k]])) {
      p_k <- plot(out_mods[[k]], q_fit = TRUE) +
        labs(title = glue::glue("{int_mod_vec[k]} model"),
             subtitle = glue::glue("BirdID: {file_id}"),
             x = "Date", y = "Displacement (km)")
      
      loo_val <- round(loo_vec[k], 2)
      is_best <- (k == best_mod_index)
      label_color <- if (is_best) "darkgreen" else "red"
      
      loo_text <- textGrob(glue::glue("LOO: {loo_val}"),
                           gp = gpar(col = label_color, fontsize = 12))
      
      plot_list[[k]] <- p_k
      loo_labels[[k]] <- loo_text
    }
  }
  
  combined_list <- mapply(function(p, label) {
    arrangeGrob(p, label, ncol = 1, heights = c(0.9, 0.1))
  }, plot_list, loo_labels, SIMPLIFY = FALSE)
  
  title_grob <- textGrob(glue::glue("BirdID: {file_id}"),
                         gp = gpar(fontsize = 16, fontface = "bold"))
  
  final_plot <- arrangeGrob(
    title_grob,
    arrangeGrob(grobs = combined_list, ncol = 1),
    ncol = 1,
    heights = c(0.1, 0.9)
  )
  
  all_final_plots <- c(all_final_plots, list(final_plot))
  
  cat("Finished hen", file_id, "\n")
}

pdf_dir <- here("Data management/Csvs/Piecewise Regression/Piecewise Regression Plots/Model Output Plots/")
if (!dir.exists(pdf_dir)) {
  dir.create(pdf_dir, recursive = TRUE)
}

pdf(file = here("All_BirdIDs_Model_Comparison_Packet.manuscript.pdf"), width = 12, height = 12)

for (plot_page in all_final_plots) {
  grid.newpage()
  grid.draw(plot_page)
}

dev.off()

write_csv(data.frame(Skipped_BirdID = skipped_birds),
          here("Data management", "Csvs", "Piecewise Regression", "Model Parameters", "skipped_birdids.csv"))


################################################################################
## Create output for winter range analysis

# Read in model parameter csv
# params <- read_csv("Data management/Csvs/Piecewise Regression/Model Parameters/best_mod_params_2025Revision.csv")
# params
# 
# # Remove rows where id or name is NA
# df_clean <- subset(params, !is.na(id) & !is.na(name))
# 
# # Extract bandid and convert to numeric
# sample <- df_clean %>%
#   dplyr::mutate("BandID" = str_sub(id, 1,5)) %>%
#   dplyr::mutate(BandID = as.numeric(str_replace(BandID, "_", ""))) 
# 
# # Read in virus csv
# virus <- read_csv("Data management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/virus_raw.csv") %>%
#   dplyr::rename("BandID" = bandid)
# virus
# 
# # Merge virus and sample
# # If a bird mover assign that bird a value of 1 in the status
# sample.virus <- right_join(virus, sample) %>%
#   dplyr::select(-REV) %>%
#   na.omit()
# 
# # Read in capture data
# caps<- read_csv("Data management/Csvs/Raw/Captures/captures_pa.csv")
# caps
# 
# # Create a capture date column
# caps <- caps %>%
#   dplyr::mutate(
#     CaptureDate = make_date(captyr, captmo, captday)) %>%
#   dplyr::rename(BandID = bandid,
#                 Age = age
#                 )
# 
# # Convert to numeric 
# caps$captyr <- lubridate::year(caps$CaptureDate)
# sample.virus <- sample.virus %>%
#   dplyr::mutate(Yr = stringr::str_split_fixed(id, "_", 2)[, 2])
# 
# # Merge caps and dat4analysis_hr together
# sample.caps <- right_join(caps, sample.virus) %>%
#   dplyr::select(BandID, 
#                 id,
#                 Age,
#                 captyr,
#                 Yr,
#                 name,
#                 mean,
#                 upper,
#                 lower,
#                 LPDV) 
# 
# # Convert columns to numeric for calculation
# sample.caps$captyr <- as.numeric(sample.caps$captyr)
# sample.caps$Yr <- as.numeric(sample.caps$Yr)
# 
# # Create a years since capture column
# sample.caps <- sample.caps %>%
#   dplyr::mutate(yrsincecap = Yr-captyr) %>%
#   na.omit()
# 
# # Assign Adult as the reference level
# sample.caps$Age <- ifelse(sample.caps$Age == "J", 1, 
#                           ifelse(sample.caps$Age == "A", 0, NA))
# 
# # Dealing with scaling Age ad hoc
# # If the bird is an adult and the years since capture is >1 assign it as an adult
# # If not keep the Age as juvenile 
# sample.caps$Age <- ifelse(sample.caps$Age == 1 & sample.caps$yrsincecap >= 1, 0, sample.caps$Age)
# 
# 
# ################################################################################
# ## Create object for birds that only moved
# 
# # Find ids that have an "int_3" row
# ids_with_int3 <- unique(df_clean$id[df_clean$name == "int_3"])
# 
# # Filter the original dataframe to only include those ids
# filtered_df <- subset(df_clean, id %in% ids_with_int3)
# length(unique(filtered_df$id))
# 
# # The difference between the third and first intercepts must be at least 2.5 km
# filtered_df_3km <- filtered_df %>%
#   dplyr::group_by(id) %>%
#   dplyr::filter(
#     any(name == "int_3") & any(name == "int_1") &
#       (mean(mean[name == "int_3"], na.rm = TRUE) -
#          mean(mean[name == "int_1"], na.rm = TRUE) > 2.5)
#   ) %>%
#   dplyr::ungroup()
# 
# # Sample of mover birds
# length(unique(filtered_df_3km$id))
# 
# # Extract bandid and convert to numeric
# sample <- filtered_df_3km %>%
#   dplyr::mutate("BandID" = str_sub(id, 1,5)) %>%
#   dplyr::mutate(BandID = as.numeric(str_replace(BandID, "_", ""))) 
# 
# # Read in virus csv
# virus <- read_csv("Data management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/Disease_Status.csv") %>%
#   dplyr::rename("BandID" = bandid)
# virus
# 
# # Merge virus and sample
# # If a bird mover assign that bird a value of 1 in the status
# sample.virus <- right_join(virus, sample) 
# 
# # Create an ID column
# sample.virus <- sample.virus %>%
#   dplyr::mutate(Yr = stringr::str_split_fixed(id, "_", 2)[, 2])
# 
# # Merge caps and dat4analysis_hr together
# sample.caps <- right_join(caps, sample.virus) %>%
#   dplyr::select(BandID, 
#                 id,
#                 Age,
#                 captyr,
#                 Yr,
#                 name,
#                 mean,
#                 upper,
#                 lower,
#                 LPDV) %>%
#   dplyr::mutate(Yr = ifelse(Yr == 202, 2024, Yr))
# 
# # Convert columns to numeric for calculation
# sample.caps$captyr <- as.numeric(sample.caps$captyr)
# sample.caps$Yr <- as.numeric(sample.caps$Yr)
# 
# # Create a years since capture column
# sample.caps <- sample.caps %>%
#   dplyr::mutate(yrsincecap = Yr-captyr)
# 
# # Assign Adult as the reference level
# sample.caps$Age <- ifelse(sample.caps$Age == "J", 1, 
#                           ifelse(sample.caps$Age == "A", 0, NA))
# 
# # Dealing with scaling Age ad hoc
# # If the bird is an adult and the years since capture is >1 assign it as an adult
# # If not keep the Age as juvenile 
# sample.caps$Age <- ifelse(sample.caps$Age == 1 & sample.caps$yrsincecap >= 1, 0, sample.caps$Age)
# 
# # Filter data to include the 2 change points
# mover_only <- sample.caps %>%
#   dplyr::filter(name %in% c("cp_1", "cp_2")) %>%
#   dplyr::select(-yrsincecap) %>%
#   dplyr::rename("BirdID" = id)
# 
# # Pivot wider to get cp_1 and cp_2 in the same row
# mover_wide <- mover_only %>%
#   dplyr::filter(name %in% c("cp_1", "cp_2")) %>%
#   tidyr::pivot_wider(
#     id_cols = c(BandID, BirdID, Age, captyr, Yr, LPDV),
#     names_from = name,
#     values_from = c(mean, upper, lower),
#     names_sep = "_"
#   ) 
# 
# # Convert to Date using Yr as origin
# mover_only <- mover_wide %>%
#   dplyr::mutate(
#     mean_cp_1  = purrr::map_dbl(mean_cp_1, 1),
#     mean_cp_2  = purrr::map_dbl(mean_cp_2, 1),
#     upper_cp_1 = purrr::map_dbl(upper_cp_1, 1),
#     lower_cp_1 = purrr::map_dbl(lower_cp_1, 1),
#     upper_cp_2 = purrr::map_dbl(upper_cp_2, 1),
#     lower_cp_2 = purrr::map_dbl(lower_cp_2, 1),
#     
#     ChangePoint_1 = as.Date(mean_cp_1 - 1, origin = paste0(Yr, "-01-01")),
#     ChangePoint_2 = as.Date(mean_cp_2 - 1, origin = paste0(Yr, "-01-01")),
#     upper_cp_1 = as.Date(upper_cp_1 - 1, origin = paste0(Yr, "-01-01")),
#     lower_cp_1 = as.Date(lower_cp_1 - 1, origin = paste0(Yr, "-01-01")),
#     upper_cp_2 = as.Date(upper_cp_2 - 1, origin = paste0(Yr, "-01-01")),
#     lower_cp_2 = as.Date(lower_cp_2 - 1, origin = paste0(Yr, "-01-01"))
#   ) %>%
#   dplyr::select(
#     BandID, BirdID, Age, Yr, LPDV,
#     ChangePoint_1, ChangePoint_2
#   ) %>%
#   dplyr::rename(Year = Yr)
# 
# # Output data for movers
# write_csv(mover_only, "Sample/Dispersed Birds/birdlist.csv")
# 
# # Custom color palette
# custom_palette <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4")
# 
# # Convert ChangePoint_1 to day-of-year
# mover_only <- mover_only %>%
#   mutate(DayOfYear = as.numeric(format(ChangePoint_1, "%j")))
# 
# # Plot with boxplot + jitter + custom colors
# ggplot(mover_only, aes(x = WMU, y = DayOfYear, fill = WMU)) +
#   geom_boxplot(alpha = 0.7, color = "black", outlier.shape = NA) +
#   geom_jitter(width = 0.2, size = 2, alpha = 0.6, color = "black") +
#   scale_fill_manual(values = custom_palette) +
#   labs(x = "Wildlife Management Unit",
#     y = "Beginning of Movement (Day of Year)"
#   ) +
#   theme_light() +
#   theme(legend.position = "none",
#         axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
#         axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
#         axis.text.x = element_text(size = 10, face = "bold"),
#         axis.text.y = element_text(size = 10, face = "bold"))
# 
# mean(mover_only$DayOfYear)
# table(mover_only$WMU)
# max(mover_only$ChangePoint_2)
# min(mover_only$ChangePoint_1)
# 
# 
# ################################################################################
# ## Now create dataset of all birds
# 
# mover_only <- mover_only %>%
#   dplyr::mutate(Status = "1")
# 
# # Read in model parameter csv
# params <- read_csv("Data management/Csvs/Piecewise Regression/Model Parameters/best_mod_params_Franny.csv")
# params
# 
# # Remove rows where id or name is NA
# df_clean <- subset(params, !is.na(id) & !is.na(name))
# 
# # Extract bandid and convert to numeric
# sample <- df_clean %>%
#   dplyr::mutate("BandID" = str_sub(id, 1,5)) %>%
#   dplyr::mutate(BandID = as.numeric(str_replace(BandID, "_", ""))) 
# 
# # Read in virus csv
# virus <- read_csv("Data management/Csvs/Raw/Disease/LPDV_REV/Pennsylvania/virus_raw.csv") %>%
#   dplyr::rename("BandID" = bandid)
# virus
# 
# # Merge virus and sample
# # If a bird mover assign that bird a value of 1 in the status
# sample.virus <- right_join(virus, sample) %>%
#   dplyr::select(-REV) %>%
#   na.omit()
# 
# # Read in capture data
# caps<- read_csv("Data management/Csvs/Raw/Captures/20250629_PAHenCaptures_2022_2023_2024.csv")
# caps
# 
# # Convert to numeric 
# caps$captyr <- lubridate::year(caps$CaptureDate)
# sample.virus <- sample.virus %>%
#   dplyr::mutate(Yr = stringr::str_split_fixed(id, "_", 2)[, 2])
# 
# # Merge caps and dat4analysis_hr together
# sample.caps <- right_join(caps, sample.virus) %>%
#   dplyr::select(BandID, 
#                 id,
#                 Age,
#                 WMU,
#                 captyr,
#                 Yr,
#                 name,
#                 mean,
#                 upper,
#                 lower,
#                 LPDV) %>%
#   dplyr::mutate(Yr = ifelse(Yr == 202, 2024, Yr))
# 
# # Convert columns to numeric for calculation
# sample.caps$captyr <- as.numeric(sample.caps$captyr)
# sample.caps$Yr <- as.numeric(sample.caps$Yr)
# 
# # Create a years since capture column
# sample.caps <- sample.caps %>%
#   dplyr::mutate(yrsincecap = Yr-captyr) %>%
#   na.omit()
# 
# # Assign Adult as the reference level
# sample.caps$Age <- ifelse(sample.caps$Age == "J", 1, 
#                           ifelse(sample.caps$Age == "A", 0, NA))
# 
# # Dealing with scaling Age ad hoc
# # If the bird is an adult and the years since capture is >1 assign it as an adult
# # If not keep the Age as juvenile 
# sample.caps$Age <- ifelse(sample.caps$Age == 1 & sample.caps$yrsincecap >= 1, 0, sample.caps$Age)
# 
# birdlist <- sample.caps %>%
#   dplyr::select(id, BandID, Age, Yr, LPDV) %>%
#   distinct()
# 
# # Get IDs with Status == 1 in mover_only
# ids_with_status1 <- mover_only %>%
#   dplyr::filter(Status == 1) %>%
#   dplyr::pull(BirdID)
# 
# # Create Status column in birdlist
# birdlist <- birdlist %>%
#   dplyr::mutate(Status = ifelse(id %in% ids_with_status1, 1, 0)) %>%
#   dplyr::rename(BirdID = id)
# 
# # Resident Stratification
# birdlist_zero <- birdlist %>%
#   filter(Status == 0) %>%
#   mutate(
#     # Assign year based on Yr column
#     Year = Yr,
#     
#     # Dynamically create ChangePoints based on the Yr
#     ChangePoint_1 = make_date(Yr, 2, 7),
#     ChangePoint_2 = make_date(Yr, 4, 28),
#     
#     # Day of year for ChangePoint_1
#     DayOfYear = yday(ChangePoint_1)
#   ) %>%
#   # Rename and reorder columns to match mover_only
#   dplyr::select(
#     BandID,
#     BirdID,
#     Age,
#     Year,
#     LPDV,
#     ChangePoint_1,
#     ChangePoint_2,
#     DayOfYear,
#     Status
#   )
# # Change to character to merge 
# birdlist_zero$Status <- as.character(birdlist_zero$Status)
# 
# # Combine with mover_only
# combined_data <- bind_rows(mover_only, birdlist_zero)
# 
# # Add in WMUs post hoc for residents
# caps.wmu <- caps %>%
#   dplyr::select(BandID, WMU)
# 
# # Merge datasets
# # Rename columns
# combined_data.wmu <- left_join(combined_data, caps.wmu, by = "BandID") %>%
#   dplyr::select(-WMU.x) %>%
#   dplyr::rename(WMU = WMU.y) %>%
#   arrange(BirdID) %>%
#   distinct(.keep_all = T)
# 
# # Write csv with the whole sample
# write.csv(combined_data.wmu, "Sample/Complete Sample/PA_Sample.csv", row.names = FALSE)
# 
# ################################################################################
# ###############################################################################X



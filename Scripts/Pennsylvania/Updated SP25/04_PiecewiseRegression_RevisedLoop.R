#'---
#' title: Seasonal Movements of Wild Turkeys in the Mid-Atlantic Region
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script uses piecewise regression to identify behavioral transitions
#' **Last Updated**: 6/3/25
#' **Key Changes**: This script follows Wolfson et al. (2025)'s workflow using piecewise regression


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
# Separate models containing 1, 2, 3, and 4 intercepts
one_int<-list(nsd_plot_data~1)
two_int<-list(nsd_plot_data~1,
              ~1)
three_int<-list(nsd_plot_data~1,
                ~1,
                ~1)
four_int<-list(nsd_plot_data~1,
               ~1,
               ~1,
               ~1)

int_mods<-list(one_int, two_int, three_int, four_int)
int_mod_vec<-c("one_int", "two_int", "three_int", "four_int")

# Create objects to store the results
# This helps with the automation process
res<-data.frame(id=NA, year=NA, name=NA, mean=NA, lower=NA, upper=NA, Rhat=NA, n.eff=NA)
write_csv(res, 
          here("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Model Parameters/best_mod_params.csv"))

# Compare models in csv
model_comparison<-data.frame(id=NA, year=NA, 
                             one_int_loo=NA, 
                             two_int_loo=NA,
                             three_int_loo=NA,
                             four_int_loo=NA)
write_csv(model_comparison, 
          here("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Model Comparison/model_comparisons.csv"))



# Create a folder to save plots if it doesn't exist
if (!dir.exists("Data Management/Csvs/Hen_NSD_Plots")) {
  dir.create("Data Management/Csvs/Hen_NSD_Data_Plots")
}

# fit mcp models
out_mods<-list()

for (file in csv_files) {
  
  # Read the data
  df <- read_csv(file)
  
  # Use the filename (without extension) as a unique identifier
  file_id <- unique(df$BirdID)[1]
  cat("Working on BirdID", print(file_id))
  
  # Fit MCP models in parallel
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
  
  # Check model convergence (Rhat) and compute Leave one out cross validation
  # Loop from Wolfson et al. (2024)
  loo_list <- vector("list", length(out_mods))
  
  for (k in seq_along(out_mods)) {
    if (!is.null(out_mods[[k]])) {
      mod_summary <- as.data.frame(summary(out_mods[[k]]))
      out_mods[[k]]$rhat_fail <- any(mod_summary$Rhat > 1.1)
      
      if (!out_mods[[k]]$rhat_fail) {
        out_mods[[k]]$loo <- loo(out_mods[[k]])
        loo_list[[k]] <- out_mods[[k]]$loo$estimates["elpd_loo", "Estimate"]
      } else {
        cat("Skipped model", int_mod_vec[[k]], "\n",
            file = here("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Model Parameters/skipped_mods.txt"),
            append = TRUE)
        loo_list[[k]] <- -9999
      }
    } else {
      loo_list[[k]] <- -9999
    }
  }
  
  # Save model comparison
  loo_vec <- unlist(loo_list)
  
  # Ensure loo_vec is a named vector with 4 elements
  if (length(loo_vec) < 4) {
    loo_vec <- rep(-9999, 4)
  }
  names(loo_vec) <- paste0(int_mod_vec, "_loo")
  
  # Make one-row data frame
  mods <- as.data.frame(as.list(c(name = file_id, loo_vec)))
  
  # Save best model parameters
  best_mod_index <- which.max(loo_vec)
  best_mod <- out_mods[[best_mod_index]]
  params <- as.data.frame(summary(best_mod))
  params <- cbind.data.frame(name = file_id, params)
  write_csv(params, here("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Model Parameters/best_mod_params.csv"), 
            append = TRUE)
  
  # Save plot
  p <- plot(best_mod, q_fit = TRUE) +
    labs(y = "displacement in km",
         x = "Date",
         title = glue::glue("BirdID: {file_id} | Best model: {length(best_mod$model)} intercepts"))
  
  
  ggsave(plot = p,
         filename = here(glue::glue("Data Management/Csvs/Hen_NSD_Data_Plots/Piecewise Regression/Piecewise Regression Plots/{file_id}_.png")),
         width = 8, height = 5)
  # keep track of progress 
  cat("Finished hen", print(file_id))
}

################################################################################
###############################################################################X

#'---
#' title: Seasonal Movements of Wild Turkeys in the Mid-Atlantic Region
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script uses piecewise regression to identify behavioral transitions
#' **Last Updated**: 5/30/25


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
            "ggplot2")

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

# Define the model syntax for m3 (three intercepts)
model_m3 <- list(nsd_plot_data ~ 1, ~ 1, ~ 1)

# Create a folder to save plots if it doesn't exist
if (!dir.exists("Data Management/Csvs/Hen_NSD_Plots")) {
  dir.create("Data Management/Csvs/Hen_NSD_Data_Plots")
}

# Loop through each file
for (file in csv_files) {
  
  # Read the data
  df <- read_csv(file)
  
  # Fit the model
  model_fit <- mcp(model = model_m3, data = df[, c("days_numeric", "nsd_plot_data")],
                   par_x = "days_numeric", 
                   iter = 15000, # Number of MCMC iterations
                   adapt = 5000  # Burn in
                   )
  
  # Create the plot
  p <- plot(model_fit, q_predict = TRUE, q_fit = TRUE) +
    xlab("Julian Date") +
    ylab("Displacement from wintering area (in km)\n") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Generate a plot file name
  file_name <- basename(file) # Get file name only, not path
  plot_file_name <- paste0("Data Management/Csvs/Hen_NSD_Data/Plots/", tools::file_path_sans_ext(file_name), "_m3_plot.png")
  
  # Save the plot
  ggsave(plot_file_name, plot = p, width = 10, height = 6)
}

################################################################################
###############################################################################X
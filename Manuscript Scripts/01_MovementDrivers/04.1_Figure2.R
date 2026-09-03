#---
#' title: Seasonal Movements of Female Wild Turkeys in Pennsylvania
#' author: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script creates figure 2 in the seasonal movements manuscript
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
csv_files <- list.files(path = "Data management/Csvs/Hen_NSD_Data/Individual/", pattern = ".csv", full.names = TRUE)

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
          here("Data management/Csvs/Piecewise Regression/Model Parameters/best_mod_params_test.csv"))

# Compare models in csv
model_comparison<-data.frame(BirdID=NA,  
                             three_int=NA,
                             loo=NA)
write_csv(model_comparison, 
          here("Data management/Csvs/Piecewise Regression/Model Parameters/Three Intercepts/model_comparisons_test.csv"))


# Create a folder to save plots if it doesn't exist
if (!dir.exists("Data management/Csvs/Hen_NSD_Plots/Test")) {
  dir.create("Data management/Csvs/Hen_NSD_Data_Plots/Test")
}

# fit mcp models
out_mods<-list()

# Create a list to store all final plots for all birds
all_final_plots <- list()
skipped_birds <- c()

csv_files <- csv_files[grepl("9061_2023", csv_files)]


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
  
  # LOO comparison
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
  write_csv(params, here("Data management/Csvs/Piecewise Regression/Model Parameters/best_mod_params_Franny_Test.csv"), append = TRUE)
  
  # Save model comparison
  write_csv(as.data.frame(mods), here("Data management/Csvs/Piecewise Regression/Model Parameters/Three Intercepts/model_comparisons_Franny_Test.csv"), append = TRUE)
  
  # Generate plots with LOO annotations
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
  
  # Add BirdID as a custom title grob
  title_grob <- textGrob(glue::glue("BirdID: {file_id}"),
                         gp = gpar(fontsize = 16, fontface = "bold"))
  
  final_plot <- arrangeGrob(
    title_grob,
    arrangeGrob(grobs = combined_list, ncol = 1),
    ncol = 1,
    heights = c(0.1, 0.9)
  )
  
  # Add final plot to list for combined PDF
  all_final_plots <- c(all_final_plots, list(final_plot))
  
  cat("Finished hen", file_id, "\n")
}

################################################################################
## Clean, ggplot-only output (no title/subtitle/LOO text)

plot_list <- final_plot

for (k in seq_along(out_mods)) {
  if (!is.null(out_mods[[k]])) {
    p_k <- plot(out_mods[[k]], q_fit = TRUE) +
      labs(x = "Calendar day",
           y = "Displacement (km)") +
      theme_light(base_size = 12) +
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85")
      )
    
    # store as ggplot object
    plot_list[[k]] <- p_k
  }
}

# Select only the best model plot
final_plot <- plot_list[[best_mod_index]]
plot(final_plot)

################################################################################
###############################################################################X



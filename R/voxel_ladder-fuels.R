# ==============================================================================
#
# TLS ladder fuel calculation using voxel methods
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 6 Dec 2020
# Last commit: 6 Dec 2020
#
# Status: Development
#
# ==============================================================================
#
# Description:
#
# Computes canopy height model and canopy cover for TLS and ALS data at varying
# resolutions using the number of voxels between 1 and 4 m divided by total number
# of voxels below 4 m.
#
# ==============================================================================
#
# User inputs:
#
# tls_las_folder = Folder location for TLS .las files
# tls_las_files = list.files function to get tls files to be processed. Can be modified
#   with a pattern to restrict search
# resolutions = Vector of resolutions (in meters) to use for chm and canopy cover
# out_file = output .csv file name
#
# ==============================================================================
#
# Package dependences:
#
# sp, raster, lidR, tidyverse, glue
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)

# ================================= User inputs ================================

# tls_las_folder <- 'data/las'
tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot'
tls_las_files <- list.files(tls_las_folder, pattern = 'tls', full.names = TRUE)

resolutions <- c(seq(0.01,0.05,0.01), seq(0.1,0.5,0.1), 1)

# out_file <- 'data/voxel_ladder-fuels.csv'
out_file <- 'D:/Analyses/output/voxel_ladder-fuels.csv'

# ============ Compute voxel based ladder fuels metrics for TLS data ===========

combined_metrics <- tibble(
  campaign = numeric(),
  plot = numeric(),
  resolution_cm = numeric(),
  voxel_ladder_fuel = numeric()
)

for (tls_file in tls_las_files) {
  
  campaign <- str_extract(tls_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(tls_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing campaign ', campaign, ' plot ', plot)
  
  tls_las <- tls_file %>%
    readLAS(select = '')

  for (res in resolutions) {
    
    message('processing resolution ', res * 100, ' cm')
    
    plot_metric <- tls_las %>%
      voxel_metrics( ~ length(Z), res) %>%
      filter(Z <= 4) %>%
      summarize(
        voxel_ladder_fuel = sum(Z > 1)/n()
      ) %>%
      add_column(campaign, plot, resolution_cm = res*100)
    
    combined_metrics <- combined_metrics %>%
      add_row(plot_metric)
  }
}

 write.csv(combined_metrics, out_file)

# ==============================================================================
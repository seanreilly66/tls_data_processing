# ==============================================================================
#
# TLS ladder fuel calculation using density and voxel methods
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
# Computes many TLS ladder fuel metrics up to 10m using density and voxel method 
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

#tls_las_folder <- 'data/las'
tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot'
tls_las_files <- list.files(tls_las_folder, pattern = 'tls', full.names = TRUE)

res <- 0.2

# out_file <- 'data/voxel_ladder-fuels.csv'
out_file <- 'D:/Analyses/output/c1_ladder-fuels_metrics_201223.csv'

# ============ Compute density based ladder fuels metrics for TLS data ===========


tls_combine_density <- tibble(
  campaign = numeric(),
  plot = numeric(),
  tls_density_ladder_fuel_1to2 = numeric(),
  tls_density_ladder_fuel_1to3 = numeric(),
  tls_density_ladder_fuel_2to3 = numeric(),
  tls_density_ladder_fuel_3to4 = numeric(),
  tls_density_ladder_fuel_2to4 = numeric(),
  tls_density_ladder_fuel_1to4 = numeric(),
  tls_density_ladder_fuel_1to5 = numeric(),
  tls_density_ladder_fuel_2to5 = numeric(),
  tls_density_ladder_fuel_3to5 = numeric(),
  tls_density_ladder_fuel_4to5 = numeric(),
  tls_density_ladder_fuel_1to6 = numeric(),
  tls_density_ladder_fuel_2to6 = numeric(),
  tls_density_ladder_fuel_3to6 = numeric(),
  tls_density_ladder_fuel_4to6 = numeric(),
  tls_density_ladder_fuel_5to6 = numeric(),
  tls_density_ladder_fuel_1to7 = numeric(),
  tls_density_ladder_fuel_2to7 = numeric(),
  tls_density_ladder_fuel_3to7 = numeric(),
  tls_density_ladder_fuel_4to7 = numeric(),
  tls_density_ladder_fuel_5to7 = numeric(),
  tls_density_ladder_fuel_6to7 = numeric(),
  tls_density_ladder_fuel_1to8 = numeric(),
  tls_density_ladder_fuel_2to8 = numeric(),
  tls_density_ladder_fuel_3to8 = numeric(),
  tls_density_ladder_fuel_4to8 = numeric(),
  tls_density_ladder_fuel_5to8 = numeric(),
  tls_density_ladder_fuel_6to8 = numeric(),
  tls_density_ladder_fuel_7to8 = numeric()
)

for (tls_density_file in tls_las_files) {
  
  campaign <- str_extract(tls_density_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(tls_density_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing density TLS campaign ', campaign, ' plot ', plot)
  
  tls_density_las <- tls_density_file %>%
    readLAS(select = '')
  
  tls_density_metric <- as_tibble(tls_density_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      tls_density_ladder_fuel_1to2 = sum(Z > 1 & Z <= 2) / sum(Z <= 2),
      tls_density_ladder_fuel_1to3 = sum(Z > 1 & Z <= 3) / sum(Z <= 3),
      tls_density_ladder_fuel_2to3 = sum(Z > 2 & Z <= 3) / sum(Z <= 3),
      tls_density_ladder_fuel_3to4 = sum(Z > 3 & Z <= 4) / sum(Z <= 4),
      tls_density_ladder_fuel_2to4 = sum(Z > 2 & Z <= 4) / sum(Z <= 4),
      tls_density_ladder_fuel_1to4 = sum(Z > 1 & Z <= 4) / sum(Z <= 4),
      tls_density_ladder_fuel_1to5 = sum(Z > 1 & Z <= 5) / sum(Z <= 5),
      tls_density_ladder_fuel_2to5 = sum(Z > 2 & Z <= 5) / sum(Z <= 5),
      tls_density_ladder_fuel_3to5 = sum(Z > 3 & Z <= 5) / sum(Z <= 5),
      tls_density_ladder_fuel_4to5 = sum(Z > 4 & Z <= 5) / sum(Z <= 5),
      tls_density_ladder_fuel_1to6 = sum(Z > 1 & Z <= 6) / sum(Z <= 6),
      tls_density_ladder_fuel_2to6 = sum(Z > 2 & Z <= 6) / sum(Z <= 6),
      tls_density_ladder_fuel_3to6 = sum(Z > 3 & Z <= 6) / sum(Z <= 6),
      tls_density_ladder_fuel_4to6 = sum(Z > 4 & Z <= 6) / sum(Z <= 6),
      tls_density_ladder_fuel_5to6 = sum(Z > 5 & Z <= 6) / sum(Z <= 6),
      tls_density_ladder_fuel_1to7 = sum(Z > 1 & Z <= 7) / sum(Z <= 7),
      tls_density_ladder_fuel_2to7 = sum(Z > 2 & Z <= 7) / sum(Z <= 7),
      tls_density_ladder_fuel_3to7 = sum(Z > 3 & Z <= 7) / sum(Z <= 7),
      tls_density_ladder_fuel_4to7 = sum(Z > 4 & Z <= 7) / sum(Z <= 7),
      tls_density_ladder_fuel_5to7 = sum(Z > 5 & Z <= 7) / sum(Z <= 7),
      tls_density_ladder_fuel_6to7 = sum(Z > 6 & Z <= 7) / sum(Z <= 7),
      tls_density_ladder_fuel_1to8 = sum(Z > 1 & Z <= 8) / sum(Z <= 8),
      tls_density_ladder_fuel_2to8 = sum(Z > 2 & Z <= 8) / sum(Z <= 8),
      tls_density_ladder_fuel_3to8 = sum(Z > 3 & Z <= 8) / sum(Z <= 8),
      tls_density_ladder_fuel_4to8 = sum(Z > 4 & Z <= 8) / sum(Z <= 8),
      tls_density_ladder_fuel_5to8 = sum(Z > 5 & Z <= 8) / sum(Z <= 8),
      tls_density_ladder_fuel_6to8 = sum(Z > 6 & Z <= 8) / sum(Z <= 8),
      tls_density_ladder_fuel_7to8 = sum(Z > 7 & Z <= 8) / sum(Z <= 8)
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  tls_combine_density <- tls_combine_density %>%
    add_row(tls_density_metric)
  
}

# ============ Compute voxel based ladder fuels metrics for TLS data ===========

tls_combine_voxel <- tibble(
  campaign = numeric(),
  plot = numeric(),
  tls_voxel_ladder_fuel_1to2 = numeric(),
  tls_voxel_ladder_fuel_1to3 = numeric(),
  tls_voxel_ladder_fuel_2to3 = numeric(),
  tls_voxel_ladder_fuel_3to4 = numeric(),
  tls_voxel_ladder_fuel_2to4 = numeric(),
  tls_voxel_ladder_fuel_1to4 = numeric(),
  tls_voxel_ladder_fuel_1to5 = numeric(),
  tls_voxel_ladder_fuel_2to5 = numeric(),
  tls_voxel_ladder_fuel_3to5 = numeric(),
  tls_voxel_ladder_fuel_4to5 = numeric(),
  tls_voxel_ladder_fuel_1to6 = numeric(),
  tls_voxel_ladder_fuel_2to6 = numeric(),
  tls_voxel_ladder_fuel_3to6 = numeric(),
  tls_voxel_ladder_fuel_4to6 = numeric(),
  tls_voxel_ladder_fuel_5to6 = numeric(),
  tls_voxel_ladder_fuel_1to7 = numeric(),
  tls_voxel_ladder_fuel_2to7 = numeric(),
  tls_voxel_ladder_fuel_3to7 = numeric(),
  tls_voxel_ladder_fuel_4to7 = numeric(),
  tls_voxel_ladder_fuel_5to7 = numeric(),
  tls_voxel_ladder_fuel_6to7 = numeric(),
  tls_voxel_ladder_fuel_1to8 = numeric(),
  tls_voxel_ladder_fuel_2to8 = numeric(),
  tls_voxel_ladder_fuel_3to8 = numeric(),
  tls_voxel_ladder_fuel_4to8 = numeric(),
  tls_voxel_ladder_fuel_5to8 = numeric(),
  tls_voxel_ladder_fuel_6to8 = numeric(),
  tls_voxel_ladder_fuel_7to8 = numeric()
)

for (tls_voxel_file in tls_las_files) {
  
  campaign <- str_extract(tls_voxel_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(tls_voxel_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing voxel campaign ', campaign, ' plot ', plot)
  
  tls_voxel_las <- tls_voxel_file %>%
    readLAS(select = '')
  
  tls_voxel_metric <- tls_voxel_las %>%
    voxel_metrics(~ length(Z), res) %>%
    summarize(
      tls_voxel_ladder_fuel_1to2 = sum(Z > 1 & Z <= 2) / sum(Z <= 2),
      tls_voxel_ladder_fuel_1to3 = sum(Z > 1 & Z <= 3) / sum(Z <= 3),
      tls_voxel_ladder_fuel_2to3 = sum(Z > 2 & Z <= 3) / sum(Z <= 3),
      tls_voxel_ladder_fuel_3to4 = sum(Z > 3 & Z <= 4) / sum(Z <= 4),
      tls_voxel_ladder_fuel_2to4 = sum(Z > 2 & Z <= 4) / sum(Z <= 4),
      tls_voxel_ladder_fuel_1to4 = sum(Z > 1 & Z <= 4) / sum(Z <= 4),
      tls_voxel_ladder_fuel_1to5 = sum(Z > 1 & Z <= 5) / sum(Z <= 5),
      tls_voxel_ladder_fuel_2to5 = sum(Z > 2 & Z <= 5) / sum(Z <= 5),
      tls_voxel_ladder_fuel_3to5 = sum(Z > 3 & Z <= 5) / sum(Z <= 5),
      tls_voxel_ladder_fuel_4to5 = sum(Z > 4 & Z <= 5) / sum(Z <= 5),
      tls_voxel_ladder_fuel_1to6 = sum(Z > 1 & Z <= 6) / sum(Z <= 6),
      tls_voxel_ladder_fuel_2to6 = sum(Z > 2 & Z <= 6) / sum(Z <= 6),
      tls_voxel_ladder_fuel_3to6 = sum(Z > 3 & Z <= 6) / sum(Z <= 6),
      tls_voxel_ladder_fuel_4to6 = sum(Z > 4 & Z <= 6) / sum(Z <= 6),
      tls_voxel_ladder_fuel_5to6 = sum(Z > 5 & Z <= 6) / sum(Z <= 6),
      tls_voxel_ladder_fuel_1to7 = sum(Z > 1 & Z <= 7) / sum(Z <= 7),
      tls_voxel_ladder_fuel_2to7 = sum(Z > 2 & Z <= 7) / sum(Z <= 7),
      tls_voxel_ladder_fuel_3to7 = sum(Z > 3 & Z <= 7) / sum(Z <= 7),
      tls_voxel_ladder_fuel_4to7 = sum(Z > 4 & Z <= 7) / sum(Z <= 7),
      tls_voxel_ladder_fuel_5to7 = sum(Z > 5 & Z <= 7) / sum(Z <= 7),
      tls_voxel_ladder_fuel_6to7 = sum(Z > 6 & Z <= 7) / sum(Z <= 7),
      tls_voxel_ladder_fuel_1to8 = sum(Z > 1 & Z <= 8) / sum(Z <= 8),
      tls_voxel_ladder_fuel_2to8 = sum(Z > 2 & Z <= 8) / sum(Z <= 8),
      tls_voxel_ladder_fuel_3to8 = sum(Z > 3 & Z <= 8) / sum(Z <= 8),
      tls_voxel_ladder_fuel_4to8 = sum(Z > 4 & Z <= 8) / sum(Z <= 8),
      tls_voxel_ladder_fuel_5to8 = sum(Z > 5 & Z <= 8) / sum(Z <= 8),
      tls_voxel_ladder_fuel_6to8 = sum(Z > 6 & Z <= 8) / sum(Z <= 8),
      tls_voxel_ladder_fuel_7to8 = sum(Z > 7 & Z <= 8) / sum(Z <= 8)
      ) %>%
    add_column(campaign, plot, .before = 1)
  
  tls_combine_voxel <- tls_combine_voxel %>%
    add_row(tls_voxel_metric)
  
}

combined_metrics <- full_join(tls_combine_density, tls_combine_voxel, by = c('campaign','plot'))

write.csv(combined_metrics, out_file)


# ==============================================================================



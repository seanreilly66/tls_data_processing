# ==============================================================================
#
# TLS, UAV, ALS, ZEB ladder fuel calculation using density 
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
# Computes  TLS ladder fuel metrics up to 8m using density
# 
# "tls_ladder_fuel_1to2" "tls_ladder_fuel_7to8" 
# "tls_ladder_fuel_1to5" "tls_ladder_fuel_1to3"
# 'tls_ladder_fuel_1to4
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
# tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot'
# tls_las_files <- list.files(tls_las_folder, pattern = 'tls', full.names = TRUE)

tls_las <- readLAS('D:/c6 - Saddle Mtn/c6_tls_fixed/c6_tls_p11_DEMnorm_cut2plot_200205.las', select='')
# ============ Compute density based ladder fuels metrics for TLS data ===========


tls_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  tls_ladder_fuel_1to2 = numeric(),
  tls_ladder_fuel_1to3 = numeric(),
  tls_ladder_fuel_1to4 = numeric(),
  tls_ladder_fuel_1to5 = numeric(),
  tls_ladder_fuel_7to8 = numeric()
)

for (tls_file in tls_las_files) {
  
  campaign <- str_extract(tls_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(tls_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing TLS campaign ', campaign, ' plot ', plot)
  
  tls_las <- tls_file %>%
    readLAS(select = '')
  
  tls_metric <- as_tibble(tls_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      tls_ladder_fuel_1to2 = sum(Z > 1 & Z <= 2) / sum(Z <= 2),
      tls_ladder_fuel_1to3 = sum(Z > 1 & Z <= 3) / sum(Z <= 3),
      tls_ladder_fuel_2to3 = sum(Z > 2 & Z <= 3) / sum(Z <= 3),
      tls_ladder_fuel_1to4 = sum(Z > 1 & Z <= 4) / sum(Z <= 4),
      tls_ladder_fuel_2to4 = sum(Z > 2 & Z <= 4) / sum(Z <= 4),
      tls_ladder_fuel_3to4 = sum(Z > 3 & Z <= 4) / sum(Z <= 4),
      tls_ladder_fuel_1to5 = sum(Z > 1 & Z <= 5) / sum(Z <= 5),
      tls_ladder_fuel_2to5 = sum(Z > 2 & Z <= 5) / sum(Z <= 5),
      tls_ladder_fuel_3to5 = sum(Z > 3 & Z <= 5) / sum(Z <= 5),
      tls_ladder_fuel_4to5 = sum(Z > 4 & Z <= 5) / sum(Z <= 5),
      tls_ladder_fuel_1to6 = sum(Z > 1 & Z <= 6) / sum(Z <= 6),
      tls_ladder_fuel_2to6 = sum(Z > 2 & Z <= 6) / sum(Z <= 6),
      tls_ladder_fuel_3to6 = sum(Z > 3 & Z <= 6) / sum(Z <= 6),
      tls_ladder_fuel_4to6 = sum(Z > 4 & Z <= 6) / sum(Z <= 6),
      tls_ladder_fuel_5to6 = sum(Z > 5 & Z <= 6) / sum(Z <= 6),
      tls_ladder_fuel_1to7 = sum(Z > 1 & Z <= 7) / sum(Z <= 7),
      tls_ladder_fuel_2to7 = sum(Z > 2 & Z <= 7) / sum(Z <= 7),
      tls_ladder_fuel_3to7 = sum(Z > 3 & Z <= 7) / sum(Z <= 7),
      tls_ladder_fuel_4to7 = sum(Z > 4 & Z <= 7) / sum(Z <= 7),
      tls_ladder_fuel_5to7 = sum(Z > 5 & Z <= 7) / sum(Z <= 7),
      tls_ladder_fuel_6to7 = sum(Z > 6 & Z <= 7) / sum(Z <= 7),
      tls_ladder_fuel_1to8 = sum(Z > 1 & Z <= 8) / sum(Z <= 8),
      tls_ladder_fuel_2to8 = sum(Z > 2 & Z <= 8) / sum(Z <= 8),
      tls_ladder_fuel_3to8 = sum(Z > 3 & Z <= 8) / sum(Z <= 8),
      tls_ladder_fuel_4to8 = sum(Z > 4 & Z <= 8) / sum(Z <= 8),
      tls_ladder_fuel_5to8 = sum(Z > 5 & Z <= 8) / sum(Z <= 8),
      tls_ladder_fuel_6to8 = sum(Z > 6 & Z <= 8) / sum(Z <= 8),
      tls_ladder_fuel_7to8 = sum(Z > 7 & Z <= 8) / sum(Z <= 8)
    )
  
  %>%
    add_column(campaign, plot, .before = 1)
  
  tls_combine <- tls_combine %>%
    add_row(tls_metric)
  
}

write_csv(tls_metric,'D:/Analyses/c6_tls_p11_ladder-fuel_metrics_FIXED.csv')

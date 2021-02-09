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
tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot'
tls_las_files <- list.files(tls_las_folder, pattern = 'tls', full.names = TRUE)

als_las_folder <- 'D:/c1 - Pepperwood/c1_ALS_normplot'
als_las_files <- list.files(als_las_folder, pattern = 'als', full.names = TRUE)

uav_las_folder <- 'D:/c6 - Saddle Mtn/c6_uas'
uav_las_files <- list.files(uav_las_folder, pattern = 'c6', full.names = TRUE)

zeb_las_folder <- 'D:/c1 - Pepperwood/c1_zeb_cut2plot'
zeb_las_files <- list.files(zeb_las_folder, pattern = 'zeb', full.names = TRUE)

banner_data <- read_csv('D:/Analyses/Ladder fuels-airtable.csv')


# out_file <- 'data/voxel_ladder-fuels.csv'
out_file <- 'D:/Analyses/output/c6_ladder-fuels_metrics_uas_210129_2.csv'

# ============ Compute density based ladder fuels metrics for TLS data ===========


tls_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  tls_ladder_fuel_1to2 = numeric(),
  tls_ladder_fuel_1to3 = numeric(),
  tls_ladder_fuel_1to4 = numeric(),
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
      tls_ladder_fuel_1to4 = sum(Z > 1 & Z <= 4) / sum(Z <= 4),
      tls_ladder_fuel_7to8 = sum(Z > 7 & Z <= 8) / sum(Z <= 8)
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  tls_combine <- tls_combine %>%
    add_row(tls_metric)
  
}

# ============ Compute density based ladder fuels metrics for ALS data ===========


als_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  als_ladder_fuel_1to2 = numeric(),
  als_ladder_fuel_1to3 = numeric(),
  als_ladder_fuel_1to4 = numeric(),
  als_ladder_fuel_1to5 = numeric(),
  als_ladder_fuel_7to8 = numeric()
)

for (als_file in als_las_files) {
  
  campaign <- str_extract(als_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(als_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing als campaign ', campaign, ' plot ', plot)
  
  als_las <- als_file %>%
    readLAS(select = '')
  
  als_metric <- as_tibble(als_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      als_ladder_fuel_1to2 = sum(Z > 1 & Z <= 2) / sum(Z <= 2),
      als_ladder_fuel_1to3 = sum(Z > 1 & Z <= 3) / sum(Z <= 3),
      als_ladder_fuel_1to4 = sum(Z > 1 & Z <= 4) / sum(Z <= 4),
      als_ladder_fuel_7to8 = sum(Z > 7 & Z <= 8) / sum(Z <= 8)
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  als_combine <- als_combine %>%
    add_row(als_metric)
  
}
# ============ Compute density based ladder fuels metrics for UAV data ===========


uav_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  uav_ladder_fuel_1to2 = numeric(),
  uav_ladder_fuel_1to3 = numeric(),
  uav_ladder_fuel_1to4 = numeric(),
  uav_ladder_fuel_1to5 = numeric(),
  uav_ladder_fuel_7to8 = numeric()
)

for (uav_file in uav_las_files) {
  
  campaign <- str_extract(uav_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(uav_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing uav campaign ', campaign, ' plot ', plot)
  
  uav_las <- uav_file %>%
    readLAS(select = '')
  
  uav_metric <- as_tibble(uav_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      uav_ladder_fuel_1to2 = sum(uav_las$Z > 1 & uav_las$Z <= 2) / sum(uav_las$Z <= 2),
      uav_ladder_fuel_1to3 = sum(uav_las$Z > 1 & uav_las$Z <= 3) / sum(uav_las$Z <= 3),
      uav_ladder_fuel_1to4 = sum(uav_las$Z > 1 & uav_las$Z <= 4) / sum(uav_las$Z <= 4),
      uav_ladder_fuel_7to8 = sum(uav_las$Z > 7 & uav_las$Z <= 8) / sum(uav_las$Z <= 8)
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  uav_combine <- uav_combine %>%
    add_row(uav_metric)
  
}

# ============ Compute density based ladder fuels metrics for ZEB data ===========


zeb_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  zeb_ladder_fuel_1to2 = numeric(),
  zeb_ladder_fuel_1to3 = numeric(),
  zeb_ladder_fuel_1to4 = numeric(),
  zeb_ladder_fuel_1to5 = numeric(),
  zeb_ladder_fuel_7to8 = numeric()
)

for (zeb_file in zeb_las_files) {
  
  campaign <- str_extract(zeb_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(zeb_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing zeb campaign ', campaign, ' plot ', plot)
  
  zeb_las <- zeb_file %>%
    readLAS(select = '')
  
  zeb_metric <- as_tibble(zeb_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      zeb_ladder_fuel_1to2 = sum(Z > 1 & Z <= 2) / sum(Z <= 2),
      zeb_ladder_fuel_1to3 = sum(Z > 1 & Z <= 3) / sum(Z <= 3),
      zeb_ladder_fuel_1to4 = sum(Z > 1 & Z <= 4) / sum(Z <= 4),
      zeb_ladder_fuel_7to8 = sum(Z > 7 & Z <= 8) / sum(Z <= 8)
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  zeb_combine <- zeb_combine %>%
    add_row(zeb_metric)
  
}
# ================================= banner data ================================

banner_data$Plot <- stringr::str_replace(banner_data$Plot, 'p', '') 


banner_summary <- banner_data %>%
  group_by(Plot) %>%
  summarize(
    trad_ladder_fuel_1to2 = mean(trad_ladder_fuel_1to2, na.rm = TRUE),
    trad_ladder_fuel_1to3 = mean(trad_ladder_fuel_1to3, na.rm = TRUE),
    trad_ladder_fuel_1to4 = mean(trad_ladder_fuel_1to4, na.rm = TRUE)
  ) %>%
  rename(plot=Plot) %>%
  mutate_at('plot', as.numeric)

# ==============================================================================

combined_metrics <- tls_combine %>%
  full_join(als_combine, by = c('campaign','plot')) %>%
  full_join(zeb_combine, by = c('campaign','plot')) %>%
  full_join(uav_combine, by = c('campaign','plot')) %>%
  full_join(banner_summary, by = 'plot')


write.csv(combined_metrics, out_file)


# ==============================================================================

write.csv(uav_combine, out_file)

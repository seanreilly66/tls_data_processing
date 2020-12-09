# ==============================================================================
#
# TLS and ALS point cloud based canopy height calculation
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 15 Aug 2020
# Last commit: 21 Sept 2020
#
# Status: Complete
#
# ==============================================================================
#
# Description:
#
# Computes ladder fuel metrics from TLS and ALS from Kramer
# et al (2016)
#
# ==============================================================================
# 
# User inputs:
#
# tls_las_folder <- Folder location for TLS .las files
# tls_las_pattern <- File name pattern for TLS files containing {c} campaign number 
#   and {p} plot glue placeholders
# 
# tls_las_folder <- Folder location for ALS .las files
# tls_las_pattern <- File name pattern for ALS files containing {c} campaign number 
#   and {p} plot glue placeholders  
# 
# campaign = Vector of campaign numbers to be iterated over
# plot = Vector of plot numbers to be iterated over
# 
# out_file <- Output filename containing .csv extension
# 
# ==============================================================================
# 
# 
# Package dependences: 
#
# sp, raster, lidR, tidyverse, glue
# 
# ==============================================================================
#
# Known problems:
#
# Documentation incomplete
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)

# ================================= User inputs ================================

tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot'
tls_las_files <- list.files(tls_las_folder)

als_las_folder <- 'D:/c1 - Pepperwood/c1_ALS_normplot'

out_file <- 'D:/Analyses/output/ladderfuel_metrics_201208.csv'

# ===== Function for ladder fuel, density and standard metrics calculation =====

z_metrics <- function(z) {
  
  n_0to2 = sum(z < 2 & z > 0)
  n_1to2 = sum(z < 2 & z > 1)
  ladder_fuel_a = n_1to2/n_0to2
  
  n_0to3 = sum(z < 3 & z > 0)
  n_2to3 = sum(z < 3 & z > 2)
  ladder_fuel_b = n_2to3/n_0to3
  
  n_0to4 = sum(z < 4 & z > 0)
  n_3to4 = sum(z < 4 & z > 3)
  ladder_fuel_c = n_3to4/n_0to4
  
  n_2to4 = sum(z < 4 & z > 2)
  ladder_fuel_d = n_2to4/n_0to4
  
  n_1to4 = sum(z < 4 & z > 1)
  ladder_fuel_e = n_1to4/n_0to4
  
  n_0to8 = sum(z < 8 & z > 0)
  n_1to8 = sum(z < 8 & z > 1)
  ladder_fuel_f = n_1to8/n_0to8


  metrics <- tibble(
    ladder_fuel_1to2 = ladder_fuel_a,
    ladder_fuel_2to3 = ladder_fuel_b,
    ladder_fuel_3to4 = ladder_fuel_c,
    ladder_fuel_2to4 = ladder_fuel_d,
    ladder_fuel_1to4 = ladder_fuel_e,
    ladder_fuel_1to8 = ladder_fuel_f)

  return(metrics)
  
}

# ================== Compute grid metrics for TLS and ALS data =================

combined_metrics <- matrix(nrow = 0, ncol = 14)

colnames(combined_metrics) <- c(
  'campaign', 'plot', 'tls_ladder_fuel_1to2','tls_ladder_fuel_2to3',
    'tls_ladder_fuel_3to4', 'tls_ladder_fuel_2to4','tls_ladder_fuel_1to4',
    'tls_ladder_fuel_1to8','als_ladder_fuel_1to2','als_ladder_fuel_2to3',
  'als_ladder_fuel_3to4', 'als_ladder_fuel_2to4','als_ladder_fuel_1to4',
  'als_ladder_fuel_1to8')

combined_metrics <- as_tibble(combined_metrics)

for (file_name in tls_las_files) {

 campaign <- str_extract(file_name, '(?<=c)[:digit:]') %>%
   as.numeric()
 plot <- str_extract(file_name, '(?<=p)[:digit:]+') %>%
   as.numeric()
 
 message('processing campaign ', campaign, ' plot ', plot)
 
 als_file <- list.files(als_las_folder, 
                        pattern = glue('c{campaign}_als_p{plot}'),
                        full.names = TRUE)
 
 tls_file <- list.files(tls_las_folder, 
                        pattern = glue('c{campaign}_tls_plot_p{plot}'), 
                        full.names = TRUE)
  
 tls_z <- tls_file %>%
   readLAS(select = '') %>%
   .$Z 
 
 tls_metrics <- z_metrics(tls_z)
 
 colnames(tls_metrics) <- paste0('tls_', names(tls_metrics))
 
 if (length(als_file) == 0) {
   
   plot_metrics <- tibble(
     campaign, plot, tls_metrics
   )
   
 } else {
   
   als_z <- als_file %>%
     readLAS(select = '') %>%
     .$Z 
   
   als_metrics <- z_metrics(als_z)
   
   colnames(als_metrics) <- paste0('als_', names(als_metrics))
   
   plot_metrics <- tibble(
      campaign, plot, tls_metrics, als_metrics
    )

 }
 
 combined_metrics <- combined_metrics %>%
   add_row(plot_metrics)
  
}
    
write.csv(combined_metrics, out_file)

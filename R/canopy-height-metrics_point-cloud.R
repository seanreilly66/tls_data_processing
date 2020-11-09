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
# Computes canopy height point cloud based metrics from TLS and ALS from Garcia
# et al (2011)
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

out_file <- 'D:/Analyses/output/canopy_metrics.csv'

# ===== Function for ladder fuel, density and standard metrics calculation =====

z_metrics <- function(z) {
  
  n_0to4 = sum(z < 4 & z > 0)
  n_1to4 = sum(z < 4 & z > 1)
  ladder_fuel = n_1to4/n_0to4

  full_mean <- mean(z, na.rm = TRUE)
  full_median <- median(z, na.rm = TRUE)
  full_p95 <- quantile(z, 0.95, na.rm = TRUE)
  full_p99 <- quantile(z, 0.99, na.rm = TRUE)

  z <- subset(z, z > full_p99)

  top_mean <- mean(z, na.rm = TRUE)
  top_median <- median(z, na.rm = TRUE)
  top_p95 <- quantile(z, 0.95, na.rm = TRUE)
  top_p99 <- quantile(z, 0.99, na.rm = TRUE)

  max_height <- max(z, na.rm = TRUE)

  metrics <- tibble(
    ladder_fuel = ladder_fuel,
    full_mean = full_mean,
    full_median = full_median,
    full_p95 = full_p95,
    full_p99 = full_p99,
    top_mean = top_mean,
    top_median = top_median,
    top_p95 = top_p95,
    top_p99 = top_p99,
    max_height = max_height)

  return(metrics)
  
}

# ================== Compute grid metrics for TLS and ALS data =================

combined_metrics <- matrix(nrow = 0, ncol = 22)

colnames(combined_metrics) <- c(
  'campaign', 'plot', 'tls_ladder_fuel', 'tls_full_mean', 'tls_full_median',
  'tls_full_p95', 'tls_full_p99', 'tls_top_mean', 'tls_top_median', 'tls_top_p95',
  'tls_top_p99', 'tls_max_height', 'als_ladder_fuel', 'als_full_mean', 'als_full_median',
  'als_full_p95', 'als_full_p99', 'als_top_mean', 'als_top_median', 'als_top_p95',
  'als_top_p99', 'als_max_height')

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

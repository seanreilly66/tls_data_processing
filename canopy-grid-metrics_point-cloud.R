# ==============================================================================
#
# TLS and ALS point cloud grid metric canopy height and cover calculation
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 15 Aug 2020
# Last commit: 6 Nov 2020
#
# Status: Complete
#
# ==============================================================================
#
# Description:
#
# Computes canopy height model and canopy cover for TLS and ALS data at varying
# resolutions using the method from Garcia et al (2011)
#
# ==============================================================================
#
# User inputs:
#
# tls_las_folder = Folder location for TLS .las files
# tls_las_files = list.files function to get tls files to be processed. Can be modified
#   with a pattern to restrict search
# als_las_folder = Folder location for ALS .las files
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

tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot'
tls_las_files <- list.files(tls_las_folder)

als_las_folder <- 'D:/c1 - Pepperwood/c1_ALS_normplot'

resolutions <- 1:2

out_file <- 'D:/Analyses/output/canopy_grid_metrics.csv'

# ============= Function for cover and max height (CHM) calculation ============

z_metrics <- function(z) {
  
  chm <- max(z, na.rm = TRUE)
  cover <- sum(z > 1.4)
  
  metrics <- list(cover = cover,
                  chm = chm)
  
  return(metrics)
  
}

# =================== Function for summarizing grid metrics ====================

summary_metrics <- function(ras) {
  
  ras <- ras %>%
    as.data.frame()
  
  chm_max_height <- max(ras$chm, na.rm = TRUE)
  chm_mean <- mean(ras$chm, na.rm = TRUE)
  chm_median <- median(ras$chm, na.rm = TRUE)
  chm_p95 <- quantile(ras$chm, 0.95, na.rm = TRUE)
  chm_p99 <- quantile(ras$chm, 0.99, na.rm = TRUE)
  
  cover <- sum(ras$cover > 0, na.rm = TRUE) / nrow(ras)
  
  metrics <- tibble(chm_max_height, chm_mean, chm_median, chm_p95, chm_p99, cover)
  
}

# ================== Compute grid metrics for TLS and ALS data =================

combined_metrics <- matrix(nrow = 0, ncol = 15)

colnames(combined_metrics) <- c(
  'campaign',
  'plot',
  'resolution',
  'tls_chm_max_height',
  'tls_chm_mean',
  'tls_chm_median',
  'tls_chm_p95',
  'tls_chm_p99',
  'tls_cover',
  'als_chm_max_height',
  'als_chm_mean',
  'als_chm_median',
  'als_chm_p95',
  'als_chm_p99',
  'als_cover'
)

combined_metrics <- as_tibble(combined_metrics)


for (file_name in tls_las_files) {
  
  campaign <- str_extract(file_name, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(file_name, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing campaign ', campaign, ' plot ', plot)
  
  als_file <- list.files(
    als_las_folder,
    pattern = glue('c{campaign}_als_p{plot}'),
    full.names = TRUE
  )
  
  tls_file <- list.files(
    tls_las_folder,
    pattern = glue('c{campaign}_tls_plot_p{plot}'),
    full.names = TRUE
  )
  
  tls_file <- tls_file %>%
    readLAS(select = '')
  
  if (length(als_file) == 0) {
    als_file = NULL
  } else {
    als_file <- als_file %>%
      readLAS(select = '')
  }
  
  for (res in resolutions) {
    
    message('processing resolution ', res * 100, 'cm')
    
    tls_metrics <- tls_file %>%
      grid_metrics( ~ z_metrics(Z), res) %>%
      summary_metrics()
    
    colnames(tls_metrics) <-
      paste0('tls_', names(tls_metrics))
    
    plot_metrics <- tibble(campaign, plot, resolution_cm = res*100, tls_metrics) 
    
    if (!is.null(als_file)) {
      
      als_metrics <- als_file %>%
        grid_metrics( ~ z_metrics(Z), res) %>%
        summary_metrics()
      
      colnames(als_metrics) <-
        paste0('als_', names(als_metrics))
      
      plot_metrics <- plot_metrics %>%
        add_column(als_metrics)
      
    }
    
    combined_metrics <- combined_metrics %>%
      add_row(plot_metrics)
  }
}

write.csv(combined_metrics, out_file)

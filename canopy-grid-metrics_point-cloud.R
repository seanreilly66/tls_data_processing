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
tls_las_files <- list.files(tls_las_folder)[1:2]

als_las_folder <- 'D:/c1 - Pepperwood/c1_ALS_normplot'

resolutions <- 1:2

out_file <- 'D:/Analyses/output/canopy_grid_metrics.csv'

# ============= Function for cover and max height (CHM) calculation ============ 

z_metrics <- function(z) {
  
  chm <- max(z, na.rm = TRUE)
  
  cover <- sum(z > 1.4)
  
  metrics <- list(
    cover = cover,
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
  
  cover <- sum(ras$cover > 0, na.rm = TRUE)/nrow(ras)
  
  metrics <- tibble(
    chm_max_height, chm_mean, chm_median, chm_p95, chm_p99, cover
  )
  
}

# ================== Compute grid metrics for TLS and ALS data =================

combined_metrics <- matrix(nrow = 0, ncol = 20)

colnames(combined_metrics) <- c(
  'campaign', 'plot')

combined_metrics <- as_tibble(combined_metrics)

file_name <- tls_las_files[1]

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

tls_file <- tls_file %>%
  readLAS(select = '') 

als_file <- als_file %>%
  readLAS(select = '')
  
for (res in resolutions) {
  
  message('processing resolution ', res*100, 'cm')
  
  plot_metric <- tibble(campaign, plot, res, tls_metrics)
    add_column(res)
  
  tls_metrics <- tls_file%>%
    grid_metrics(~z_metrics(Z), res) %>%
    summary_metrics()
  
  colnames(tls_metrics) <- paste0('tls_', names(tls_metrics))
  
  plot_metric <- tibble(campaign, plot, res, tls_metrics)
 
  als_metrics <- als_file%>%
    grid_metrics(~z_metrics(Z), res) %>%
    summary_metrics()
  
  colnames(als_metrics) <- paste0('als_', res*100, 'cm_', names(als_metrics))
  
  plot_metric <- plot_metric %>%
    als_metrics
  
  combined_metrics <- combined_metrics %>%
    add_column(als_metrics)
    
}

for (file_name in tls_las_files[-1]) {

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
  
 tls_file <- tls_file %>%
   readLAS(select = '') 
 
 if (length(als_file) == 0) {
   
   als_file = NULL
   
 } else {
   
   als_file <- als_file %>%
     readLAS(select = '')
   
 }
 
 plot_metrics <- tibble(campaign, plot)
 
 for (res in resolutions) {
   
   message('processing resolution ', res*100, 'cm')
 
   tls_metrics <- tls_file%>%
     grid_metrics(~z_metrics(Z), res) %>%
     summary_metrics()
 
   colnames(tls_metrics) <- paste0('tls_', res*100, 'cm_', names(tls_metrics))
   
   plot_metrics <- plot_metrics %>%
     add_column(tls_metrics)
 
   if (!is.null(als_file)) {
     
     als_metrics <- als_file%>%
       grid_metrics(~z_metrics(Z), res) %>%
       summary_metrics()
     
     colnames(als_metrics) <- paste0('als_', res*100, 'cm_', names(als_metrics))
     
     plot_metrics <- plot_metrics %>%
       add_column(als_metrics)
     
   }
   
 }
 
 combined_metrics <- combined_metrics %>%
   add_row(plot_metrics)
  
}
    
# write.csv(combined_metrics, out_file)

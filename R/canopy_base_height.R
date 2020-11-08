# ==============================================================================
#
# TLS and ALS point cloud canopy base height
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 6 Nov 2020
# Last commit: 6 Nov 2020
#
# Status: Under development
#
# ==============================================================================
#
# Description:
#
# Computes canopy base height from voxels using the method from Garcia et al (2011)
#
# ==============================================================================
#
# User inputs: - INCOMPLETE -
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
# Current state:
#
# Playing with rayshader to make voxel visualizations
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)

library(VoxR)

# ================================= User inputs ================================

tls_las_folder <- 'data'
tls_las_files <- list.files(tls_las_folder, pattern = 'tls')

als_las_folder <- 'data'

resolution <- 0.3

# out_file <- 'data/cbh.csv'

# ======================== Initiate file name for loop ========================= 
# Current state: Randomly picks one of the TLS files and does not loop

file_name <- tls_las_files[ceiling(runif(1, min = 0, max = length(tls_las_files)))]

# for (file_name in tls_las_files) {

# ================================ Read in files =============================== 

campaign <- str_extract(file_name, '(?<=c)[:digit:]') %>%
  as.numeric()
plot <- str_extract(file_name, '(?<=p)[:digit:]+') %>%
  as.numeric()

message('processing campaign ', campaign, ' plot ', plot)

als_file <- list.files(
  als_las_folder,
  pattern = glue('c{campaign}.+als.+p{plot}'),
  full.names = TRUE
)

tls_file <- list.files(
  tls_las_folder,
  pattern = glue('c{campaign}.+tls.+p{plot}'),
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

# =========================== Generate 0.3 m voxels ============================ 


tls_voxel <- tls_file %>%
  voxel_metrics(~length(Z), res = resolution)

tls_voxel <- tls_voxel %>%
  transmute(x = X - min(X, na.rm = TRUE),
            y = Y - min(Y, na.rm = TRUE),
            z = Z - min(Z, na.rm = TRUE)
  )

plot_voxels(data = tls_voxel, res = resolution, lcol = 'black', lwd = 0.5, fcol = 'grey95')

als_voxel <- als_file %>%
  voxel_metrics(~length(Z), res = resolution)

als_voxel <- als_voxel %>%
  transmute(x = X - min(X, na.rm = TRUE),
            y = Y - min(Y, na.rm = TRUE),
            z = Z - min(Z, na.rm = TRUE)
  )

plot_voxels(data = als_voxel, res = resolution, lcol = 'black', lwd = 0.5, fcol = 'grey95')

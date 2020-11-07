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

library(rayshader)
library(plot3D)

# ================================= User inputs ================================

tls_las_folder <- 'data'
tls_las_files <- list.files(tls_las_folder, pattern = 'tls')

# als_las_folder <- 'data'

resolution <- 0.3

# out_file <- 'data/cbh.csv'


# ======================== Initiate file name for loop ========================= 

file_name <- tls_las_files[1]
rm(tls_las_files)

# for (file_name in tls_las_files) {

# ================================ Read in files =============================== 

campaign <- str_extract(file_name, '(?<=c)[:digit:]') %>%
  as.numeric()
plot <- str_extract(file_name, '(?<=p)[:digit:]+') %>%
  as.numeric()

message('processing campaign ', campaign, ' plot ', plot)

# als_file <- list.files(
#   als_las_folder,
#   pattern = glue('c{campaign}_als_p{plot}'),
#   full.names = TRUE
# )

tls_file <- list.files(
  tls_las_folder,
  pattern = glue('c{campaign}_tls_p{plot}'),  # MODIFIED - NO LONGER CONTAINS PLOT error
  full.names = TRUE
)

tls_file <- tls_file %>%
  readLAS(select = '')

# if (length(als_file) == 0) {
#   als_file = NULL
# } else {
#   als_file <- als_file %>%
#     readLAS(select = '')
# }

# =========================== Generate 0.3 m voxels ============================ 

tls_voxel <- tls_file %>%
  voxel_metrics(~length(Z), resolution)



# ==============================================================================
# This is where it goes off the rails
# ==============================================================================

rm(campaign, file_name, plot, resolution, tls_las_folder)

voxel <- tls_voxel %>%
  transmute(
    x = X - min(X, na.rm = TRUE),
    y = Y - min(Y, na.rm = TRUE),
    z = Z,
    occupied = V1 > 0,
    n = V1
  )

voxel %>%
  group_by(occupied) %>%
  summarize(
    max = max(n, na.rm = TRUE),
    min = min(n, na.rm = TRUE)
  )

voxel3D(x = voxel$X,
        y = voxel$Y,
        z = voxel$Z,
        colvar = voxel[,1:3])



par(mfrow = c(2, 2), mar  = c(2, 2, 2, 2))

# fast but needs high resolution grid
xyz <- mesh(voxel$x, voxel$y, voxel$z)
F <- with(xyz, log(x^2 + y^2 + z^2 + 
                     10*(x^2 + y^2) * (y^2 + z^2) ^2))

voxel3D(x, y, z, F, level = 2, pch = ".", cex = 5)


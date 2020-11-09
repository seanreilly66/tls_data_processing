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
# resolution = Vector of resolutions (in meters) to use for chm and canopy cover
# rolling_window_size = Size (in meters) of window to apply for rolling mean smoothing
#   of VPP. Should be a multiple of resolution
# out_file = output .csv file name
#
# ==============================================================================
#
# Package dependences:
#
# sp, raster, lidR, tidyverse, glue, ggplot2, VoxR
#
# ==============================================================================
#
# Current state:
#
# Working through processing of single file visualizations
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)
library(ggplot2)
library(VoxR)

# ================================= User inputs ================================

tls_las_folder <- 'data'
tls_las_files <- list.files(tls_las_folder, pattern = 'tls')

als_las_folder <- 'data'

resolution <- 0.3
rolling_window_size = 0.9

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

rm(campaign, file_name, plot, tls_las_files, tls_las_folder, als_las_folder)

# =========================== Generate 0.3 m voxels ============================

tls_voxel <- tls_file %>%
  voxel_metrics(~length(Z), res = resolution)

tls_voxel <- tls_voxel %>%
  transmute(x = X - min(X, na.rm = TRUE),
            y = Y - min(Y, na.rm = TRUE),
            z = Z)

plot_voxels(data = tls_voxel, res = resolution, lcol = 'black', lwd = 0.5, fcol = 'grey95')

als_voxel <- als_file %>%
  voxel_metrics(~length(Z), res = resolution)

als_voxel <- als_voxel %>%
  transmute(x = X - min(X, na.rm = TRUE),
            y = Y - min(Y, na.rm = TRUE),
            z = Z)

plot_voxels(data = als_voxel, res = resolution, lcol = 'black', lwd = 0.5, fcol = 'grey95')

rm(tls_file, als_file)

# ============================== ggplot theme set =============================== 

theme_set(
  theme(text = element_text(family = 'serif', face = 'plain'),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        line = element_line(size = 1),
        axis.line = element_line(),
        panel.background = element_rect(color = 'white'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        legend.spacing = unit(0, "cm"),
        legend.margin = margin(0,5,0,5)
  )
)

# ===================== Plot vertical structure of voxels ======================

n_cell_tls = max(tls_voxel$x, na.rm = TRUE) * max(tls_voxel$y, na.rm = TRUE) / resolution^2

tls_vpp <- tls_voxel %>%
  group_by(z) %>%
  summarize(
    n_voxel = n(),
    p_voxel = n()/n_cell_tls
  )

ggplot() +
  geom_histogram(
    data = tls_voxel,
    mapping = aes(y = z),
    binwidth = resolution) +
  geom_point(
    data = tls_vpp,
    mapping = aes(
      x = n_voxel,
      y = z))

ggplot(
  data = tls_vpp,
  mapping = aes(
    x = p_voxel, 
    y = z
    )) +
  geom_point()


n_cell_als = max(als_voxel$x, na.rm = TRUE) * max(als_voxel$y, na.rm = TRUE) / resolution^2

als_vpp <- als_voxel %>%
  group_by(z) %>%
  summarize(
    n_voxel = n(),
    p_voxel = n()/n_cell_als
  )

ggplot() +
  geom_histogram(
    data = als_voxel,
    mapping = aes(y = z),
    binwidth = resolution) +
  geom_point(
    data = als_vpp,
    mapping = aes(
      x = n_voxel,
      y = z))

ggplot(
  data = als_vpp,
  mapping = aes(
    x = p_voxel, 
    y = z
  )) +
  geom_point()

# =================== Rolling average to smooth distribution =================== 

tls_vpp <- tls_vpp %>%
  mutate(p_smooth = data.table::frollmean(
    x = p_voxel,
    n = rolling_window_size/resolution,
    align = 'center'
  ))

ggplot(
  data = tls_vpp %>%
    arrange(z),
  mapping = aes( 
    y = z
  )) +
  geom_vline(
    mapping = aes(xintercept = 0.05),
    linetype = 'dashed',
    color = 'grey80',
    size = 1
  ) +
  geom_vline(
    mapping = aes(xintercept = 0.1),
    linetype = 'dashed',
    color = 'grey80',
    size = 1
  ) +
  geom_point(
    mapping = aes(x = p_voxel),
    color = 'firebrick',
    alpha = 0.7,
    size = 2
  ) +
  geom_point(
    mapping = aes(x = p_smooth),
    size = 2
  )+
  geom_path(
    mapping = aes(x = p_smooth),
    size = 1
  )


als_vpp <- als_vpp %>%
  mutate(p_smooth = data.table::frollmean(
    x = p_voxel,
    n = rolling_window_size/resolution,
    align = 'center'
  ))

ggplot(
  data = als_vpp %>%
    arrange(z),
  mapping = aes( 
    y = z
  )) +
  geom_vline(
    mapping = aes(xintercept = 0.05),
    linetype = 'dashed',
    color = 'grey80',
    size = 1
  ) +
  geom_vline(
    mapping = aes(xintercept = 0.1),
    linetype = 'dashed',
    color = 'grey80',
    size = 1
  ) +
  geom_point(
    mapping = aes(x = p_voxel),
    color = 'firebrick',
    alpha = 0.7,
    size = 2
  ) +
  geom_point(
    mapping = aes(x = p_smooth),
    size = 2
  )+
  geom_path(
    mapping = aes(x = p_smooth),
    size = 1
  )
# ==============================================================================
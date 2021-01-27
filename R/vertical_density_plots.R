# ==============================================================================
#
# TLS and ALS point cloud canopy base height
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 26 Jan 2021
#
# Status: Under development
#
# ==============================================================================
#
# Description:
#
# Vertical density plots of TLS, ALS, UAS, and Zeb
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
# vpp_fig_output = folder for vpp graphs to be written to
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
library(ggpubr)

# ================================= User inputs ================================

# tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot/p1301'
tls_las_folder <- 'data/las/tls'

# als_las_folder <- 'D:/c1 - Pepperwood/c1_ALS_normplot/p1301'
als_las_folder <- 'data/las/als'

uas_las_folder <-  'D:/c1 - Pepperwood/c1_ALS_normplot/p1301'
  
zeb_las_folder <-  'D:/c1 - Pepperwood/c1_ALS_normplot/p1301'

vpp_fig_output <- 'D:/Analyses/p1301 figures'

high_plot <- 1301

medium_plot <- 1303
  
low_plot <- 1304

eight_plot <- 1306

window_size <- 5

# ============================== ggplot theme set ==============================

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(size = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'white'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5)
  )
)

# ======================== Generate high plot =========================

z <- tibble(
  Z = as.numeric(),
  n = as.numeric(),
  p = as.numeric(),
  p_smooth = as.numeric(),
  method = as.character(),
  plot = as.character()
)

transect <- tibble(
  X = as.numeric(),
  Z = as.numeric(),
  method = as.character(),
  plot = as.character()
)


for (plot_type in c('high_plot', 'medium_plot', 'low_plot', 'eight_plot')) {
  
  if (plot_type == 'high_plot') {
    plot = high_plot
  } else if (plot_type == 'medium_plot') {
    plot = medium_plot
  } else if (plot_type == 'low_plot') {
    plot = low_plot
  } else if (plot_type == 'eight_plot') {
    plot = eight_plot
  }
  
  # TLS
  tls <- list.files(tls_las_folder,
               pattern = glue('p{plot}'),
               full.names = TRUE) %>%
    readLAS(select = '')
  
  p_min <- as.numeric(tls@bbox[, 1])
  p_max <- as.numeric(tls@bbox[, 2])
  
  tls_transect <- clip_transect(
    tls,
    p1 = p_min,
    p2 = p_max,
    width = 3
  )
  
  tls_transect <- tls_transect@data %>%
    select(X, Z) %>%
    add_column(method = 'TLS-Riegl',
               plot = plot_type)
  
  tls_z <- tls@data %>%
    mutate(Z = round(Z, digits = 1)) %>%
    group_by(Z) %>%
    summarize(n = n()) %>%
    filter(Z > 0.5) %>%
    mutate(p = n/sum(n, na.rm = TRUE)*100) %>%
    add_column(method = 'TLS-Riegl',
               plot = plot_type) %>%
    mutate(p_smooth = data.table::frollmean(
      x = p,
      n = window_size,
      align = 'center'))
  
  rm(tls)
  
  
  # ZEB
  # zeb <- list.files(zeb_las_folder,
  #              pattern = glue('p{plot}'),
  #              full.names = TRUE) %>%
  #   readLAS(select = '')
  # 
  # zeb_transect <- clip_transect(
  #   zeb,
  #   p1 = p_min,
  #   p2 = p_max,
  #   width = 3
  # )
  # 
  # zeb_transect <- zeb_transect@data %>%
  #   select(X, Z) %>%
  #   add_column(method = 'TLS-ZEB')
  # 
  # zeb_z <- zeb@data %>%
  #   mutate(Z = round(Z, digits = 1)) %>%
  #   group_by(Z) %>%
  #   summarize(n = n()) %>%
  #   filter(Z > 0.5) %>%
  #   mutate(p = n/sum(n, na.rm = TRUE)*100) %>%
  #   add_column(method = 'TLS-ZEB') %>%
  #   mutate(p_smooth = data.table::frollmean(
  #     x = p,
  #     n = window_size,
  #     align = 'center'))
  # 
  # rm(zeb)
  
  # UAS
  # uas <- list.files(uas_las_folder,
  #              pattern = glue('p{plot}'),
  #              full.names = TRUE) %>%
  #   readLAS(select = '')
  # 
  # uas_transect <- clip_transect(
  #   uas,
  #   p1 = p_min,
  #   p2 = p_max,
  #   width = 3
  # )
  # 
  # uas_transect <- uas_transect@data %>%
  #   select(X, Z) %>%
  #   add_column(method = 'UAS',
  #              plot = plot_type)
  # 
  # uas_z <- uas@data %>%
  #   mutate(Z = round(Z, digits = 1)) %>%
  #   group_by(Z) %>%
  #   summarize(n = n()) %>%
  #   filter(Z > 0.5) %>%
  #   mutate(p = n/sum(n, na.rm = TRUE)*100) %>%
  #   add_column(method = 'UAS',
  #              plot = plot_type) %>%
  #   mutate(p_smooth = data.table::frollmean(
  #     x = p,
  #     n = window_size,
  #     align = 'center'))
  # 
  # rm(uas)
  
  # ALS
  als <- list.files(als_las_folder,
               pattern = glue('p{plot}'),
               full.names = TRUE) %>%
    readLAS(select = '')
  
  als_min <-  as.numeric(als@bbox[, 1])
  
  als_transect <- clip_transect(
    als,
    p1 = p_min,
    p2 = p_max,
    width = 3
  )
  
  als_transect <- als_transect@data %>%
    select(X, Z) %>%
    add_column(method = 'ALS',
               plot = plot_type)
  
  als_z <- als@data %>%
    mutate(Z = round(Z, digits = 1)) %>%
    group_by(Z) %>%
    summarize(n = n()) %>%
    filter(Z > 0.5) %>%
    mutate(p = n/sum(n, na.rm = TRUE)*100) %>%
    add_column(method = 'ALS',
               plot = plot_type) %>%
    mutate(p_smooth = data.table::frollmean(
      x = p,
      n = window_size,
      align = 'center'))
  
  rm(als)

  # transect <- transect %>%
  #   add_row(tls_transect, zeb_transect, uas_transect, als_transect)
  # z <- z %>%
  #   add_row(tls_z, zeb_z, uas_z, als_z)
  
  transect <- transect %>%
    add_row(tls_transect) %>%
    add_row(als_transect)
  z <- z %>%
    add_row(tls_z) %>%
    add_row(als_z)
  
}




# Plot data

transect_plot <-
  ggplot(data = transect,
         mapping = aes(x = X, y = Z, color = method, size = method)) +
  geom_point() +
  coord_equal() +
  scale_size_manual(values = c(2, 0.5)) +
  

z_plot <-
  ggplot(data = z %>% 
           filter(plot == 'high_plot') %>%
           arrange(Z),
         mapping = aes(x = p_smooth, y = Z, color = method)) +
  geom_path()

z_plot


tls_transect <-
  ggplot(data = transect %>%
           filter(method == 'TLS-Riegl') %>%
           filter(plot == 'high_plot') %>%
           sample_n(1000000),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.5) +
  coord_equal()

tls_transect


# ==============================================================================

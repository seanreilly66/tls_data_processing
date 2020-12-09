# ==============================================================================
#
# TLS ladder fuel calculation using voxel methods
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
# Computes canopy height model and canopy cover for TLS and ALS data at varying
# resolutions using the number of voxels between 1 and 4 m divided by total number
# of voxels below 4 m.
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

# tls_las_folder <- 'data/las'
tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot'
tls_las_files <- list.files(tls_las_folder, pattern = 'tls', full.names = TRUE)

als_las_folder <- 'D:/c1 - Pepperwood/c1_ALS_normplot'
als_las_files <- list.files(als_las_folder, pattern = 'als', full.names = TRUE)

res <- 0.1

# out_file <- 'data/voxel_ladder-fuels.csv'
out_file <- 'D:/Analyses/output/voxel_ladder-fuels_201208.csv'

trad_file <- 'D:/Analyses/brie_thesis_parameters.csv'


fig_output <- 'D:/Analyses/figures'

# ============ Compute voxel based ladder fuels metrics for TLS data ===========

combined_metrics <- tibble(
  campaign = numeric(),
  plot = numeric(),
  tls_voxel_ladder_fuel_1to2 = numeric(),
  tls_voxel_ladder_fuel_2to3 = numeric(),
  tls_voxel_ladder_fuel_3to4 = numeric(),
  tls_voxel_ladder_fuel_2to4 = numeric(),
  tls_voxel_ladder_fuel_1to4 = numeric(),
  tls_voxel_ladder_fuel_1to8 = numeric(),
  als_voxel_ladder_fuel_1to2 = numeric(),
  als_voxel_ladder_fuel_2to3 = numeric(),
  als_voxel_ladder_fuel_3to4 = numeric(),
  als_voxel_ladder_fuel_2to4 = numeric(),
  als_voxel_ladder_fuel_1to4 = numeric(),
  als_voxel_ladder_fuel_1to8 = numeric()
  
)


for (tls_file in tls_las_files) {
  
  campaign <- str_extract(tls_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(tls_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing TLS campaign ', campaign, ' plot ', plot)
  
  tls_las <- tls_file %>%
    readLAS(select = '')

  tls_plot_metric_a <- tls_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 2) %>%
    summarize(
      tls_voxel_ladder_fuel_1to2 = sum(Z > 1)/n()
    ) %>%
    add_column(campaign, plot)
  
  tls_plot_metric_b <- tls_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 3) %>%
    summarize(
      tls_voxel_ladder_fuel_2to3 = sum(Z > 2)/n()
    ) 
  
  tls_plot_metric_c <- tls_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 4) %>%
    summarize(
      tls_voxel_ladder_fuel_3to4 = sum(Z > 3)/n()
    )
  
  tls_plot_metric_d <- tls_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 4) %>%
    summarize(
      tls_voxel_ladder_fuel_2to4 = sum(Z > 2)/n()
    ) 
    
  tls_plot_metric_e <- tls_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 4) %>%
    summarize(
      tls_voxel_ladder_fuel_1to4 = sum(Z > 1)/n()
    )
  
  tls_plot_metric_f <- tls_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 8) %>%
    summarize(
      tls_voxel_ladder_fuel_1to8 = sum(Z > 1)/n()
    ) 
  
  
  

  
  tls_metrics <- bind_cols(tls_plot_metric_a, 
            tls_plot_metric_b, 
            tls_plot_metric_c, 
            tls_plot_metric_d, 
            tls_plot_metric_e, 
            tls_plot_metric_f)
  
}

for (als_file in als_las_files) {
  
  campaign <- str_extract(als_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(als_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing ALS campaign ', campaign, ' plot ', plot)
  
  als_las <- als_file %>%
    readLAS(select = '')
  
  als_plot_metric_a <- als_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 2) %>%
    summarize(
      als_voxel_ladder_fuel_1to2 = sum(Z > 1)/n()
    ) %>%
    add_column(campaign, plot)
  
  als_plot_metric_b <- als_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 3) %>%
    summarize(
      als_voxel_ladder_fuel_2to3 = sum(Z > 2)/n()
    ) 
  
  als_plot_metric_c <- als_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 4) %>%
    summarize(
      als_voxel_ladder_fuel_3to4 = sum(Z > 3)/n()
    )
  
  als_plot_metric_d <- als_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 4) %>%
    summarize(
      als_voxel_ladder_fuel_2to4 = sum(Z > 2)/n()
    ) 
  
  als_plot_metric_e <- als_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 4) %>%
    summarize(
      als_voxel_ladder_fuel_1to4 = sum(Z > 1)/n()
    )
  
  als_plot_metric_f <- tls_las %>%
    voxel_metrics( ~ length(Z), res) %>%
    filter(Z <= 8) %>%
    summarize(
      als_voxel_ladder_fuel_1to8 = sum(Z > 1)/n()
    ) 
  
  
  
  
  
  als_metrics <- bind_cols(als_plot_metric_a, 
                           als_plot_metric_b,
                           als_plot_metric_c, 
                           als_plot_metric_d,
                           als_plot_metric_e,
                           als_plot_metric_f)
  
}
  
join <- left_join(tls_metrics, als_metrics, by=c('campaign','plot'))

combined_metrics <- combined_metrics%>%
  add_row(join)
  




  



 write.csv(combined_metrics, out_file)



 
 
# ==============================================================================
# 
# theme_set(
#   theme(text = element_text(family = 'serif', face = 'plain'),
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 14),
#         line = element_line(size = 1),
#         axis.line = element_line(),
#         panel.background = element_rect(color = 'white'),
#         legend.title = element_text(size = 16),
#         legend.text = element_text(size = 14),
#         legend.key = element_blank(),
#         legend.spacing = unit(0, "cm"),
#         legend.margin = margin(0,5,0,5),
#         title = element_text(size = 12.8)
#   )
# )
#  
# plot <- unique(combined_metrics$plot) 
# trad <- read_csv(trad_file) 
# 
# 
# for (i in plot) {
#   
#   subtrad <- subset(trad, plot==i)
#   ladder <- subtrad$trad_ladder_fuel
#   tls <- subtrad$tls_ladder_fuel
#   
#   figure <- ggplot(
#     data = subset(combined_metrics, plot %in% i) %>%
#       mutate_at('plot', as.factor),
#     mapping = aes(
#       x = resolution_cm,
#       y = voxel_ladder_fuel,
#       color = 'black'
#     )) +
#     geom_point() +
#     geom_line() + 
#     geom_hline(
#       mapping = aes(yintercept = ladder, 
#                     color = 'grey80'),
#       linetype = 'dashed',
#       size = 1
#     ) + 
#     geom_hline(
#       mapping = aes(yintercept = tls,
#                     color = 'firebrick'),
#       linetype = 'dashed',
#       size = 1
#     ) +
#     scale_color_identity(
#       name = glue('plot {i}'),
#       breaks = c('grey80', 'firebrick', 'black'),
#       labels = c('photo banner', 'TLS', 'tls voxels'),
#       guide = "legend"
#     )+
#     ylim(0,1)
#   
#   figure
#   
#   ggsave(
#     glue('{fig_output}/p{i}_voxel_ladder-fuels.png'),
#     width = 6.5,
#     height = 4.5,
#     units = 'in',
#     dpi = 700
#     
#   )
#   
# }
# 
# 

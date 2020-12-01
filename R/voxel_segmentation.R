# ==============================================================================
#
# Point cloud segmentation
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 30 Nov 2020
# Last commit: 30 Nov 2020
#
# Status: Under development
#
# ==============================================================================
#
# Description:
#
# Segments point cloud into tree and non-tree voxels for a single plot
#
# ==============================================================================
#
# User inputs: - INCOMPLETE -
#
# plot = plot number
# plot_file = .las plot file name. Currently set up to use a list.files function to
#   identify file using plot number
# tree_files = .las file names for all trees within a plot. Currently set up to use
#   a list.files function to identify all files by plot number
# res = Resolution (in meters) to use for voxels
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
# Working through processing of single plot
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)
library(VoxR)
library(ggpubr)

# ================================= User inputs ================================

plot <- 1301

plot_file <- list.files('data/las',
                        pattern = glue('tls_p{plot}'),
                        full.name = TRUE)

tree_files <- list.files('data/clipped_trees',
                         pattern = glue('tls_p{plot}') ,
                         full.name = TRUE)

res <- 0.3

fig_output <- glue('figures/c1_p{plot}_voxel-segmentation.png')

# ============================= Voxelize las files =============================

plot_vox <- readLAS(plot_file, select = '') %>%
  voxel_metrics(~ length(Z), res) %>%
  group_by(Z) %>%
  summarize(plot_n_voxel = n())

for (tree_file in tree_files) {
  
  tree_vox <- readLAS(tree_file, select = '') %>%
    voxel_metrics( ~ length(Z), res) %>%
    group_by(Z) %>%
    summarize(n_voxel = n()) %>%
    rename(!!str_extract(tree_file, 'tree[:digit:]+(?=_)') := n_voxel)
  
  plot_vox <- plot_vox %>%
    full_join(tree_vox, by = 'Z')
  
}

plot_vox <- plot_vox %>%
  rowwise() %>%
  mutate(tree_n_voxel = sum(c_across(starts_with('tree')), na.rm = TRUE),
         .before = 3) %>%
  mutate(p_tree_voxel = tree_n_voxel / plot_n_voxel,
         .before = 4)

rm(tree_vox, tree_file)

# ============================== ggplot theme set ===============================

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

# ======================= Generate plots of distribution =======================

max_x <- plot_vox %>%
  filter(Z > 1) %>%
  select(plot_n_voxel) %>%
  max(na.rm = TRUE)

distribution_plot <- ggplot(data = plot_vox %>%
                           arrange(Z),
                         mapping = aes(y = Z)) +
  geom_path(mapping = aes(x = plot_n_voxel,
                          color = 'firebrick'),
            size = 1.5) +
  geom_path(mapping = aes(x = tree_n_voxel,
                          color = 'black'),
            size = 1.5) +
  labs(y = 'Height (m)',
       x = 'N voxels') +
  scale_color_identity(
    name = NULL,
    breaks = c('firebrick', 'black'),
    labels = c('All voxels', 'Tree voxels'),
    guide = 'legend'
  ) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  coord_cartesian(xlim = c(0, max_x))

distribution_plot

percentage_plot <- ggplot(data = plot_vox %>%
                            arrange(Z),
                          mapping = aes(y = Z,
                                        x = p_tree_voxel)) +
  geom_path(size = 1.5) +
  labs(y = NULL,
       x = 'Percentage tree voxels') +
  theme(axis.text.y = element_blank())

percentage_plot

full_fig <- ggarrange(
  distribution_plot, percentage_plot,
  nrow = 1, ncol = 2, widths = c(1, 0.7)) %>%
  annotate_figure(
    top = text_grob(glue('Plot {plot}\n ')
                    , family = 'serif', size = 16))

full_fig

ggsave(
  fig_output,
  width = 9,
  height = 4.5,
  units = 'in',
  dpi = 700)

# ==============================================================================
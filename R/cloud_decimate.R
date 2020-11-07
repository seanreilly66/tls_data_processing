# ==============================================================================
#
# Decimated point cloud generation
# 
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
# Last commit: 6 Nov 2020
#
# ==============================================================================
#
# Reades in a decimated version of a point cloud and writes the smaller version
# to file. Uses a 10% random filter to randomly reduce point cloud by 10-fold
# ==============================================================================

library(lidR)
library(stringr)

# ==============================================================================

tls_las_file = 'foldername/filename.las'

# ==============================================================================

tls <- readLAS(tls_las_file, filter = '-keep_random_fraction 0.1')

writeLAS(tls, str_replace(tls_las_file, '.las', '_decimated.las'))

# ==============================================================================

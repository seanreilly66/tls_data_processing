# ===============================================================================
#
# Combine tree attributes
#
# ===============================================================================
#
# Last edit: 9 Nov 2020
# Last edit by: Sean Reilly
#
# ===============================================================================
#
# User inputs:
#
# setwd() = The working directory currently is expected to contain a series of files
#   for tree attributes, along with a biomass file. If you decide you want to break
#   these up into more folders, we can revisit this structure and modify it accordingly
#   (e.g., all the tree attribute stuff in one place and biomass in another)
#
# ta_file_pattern = String pattern to match to file names to identify which are to
#   be combined (Located in working directory)
# ta_combined_output = File name for output combined tree attributes data (Writes
#   to working directory)
#
# bm_file <- biomass .csv file name (Located in working directory)
# bm_output <- biomass and tree attributes combined data output .csv. (Writes to
#   working directory)
#
# ===============================================================================

library(tidyverse)

# ================================= User inputs =================================

# setwd('data/tree_attributes')
setwd('D:/Analyses/c1_tree attributes')

ta_file_pattern <- 'c1_tls'
ta_combined_output <- 'c1_tls_tree_attributes.csv'

bm_file <- 'c1_trad_LBAKcombined.csv'
bm_output <- 'c1_tls_trad_plotdata_with_tree_attributes_combined.csv'

# ======================== Combine files by column names ========================

ta_file_names <- list.files(pattern = ta_file_pattern,
                            full.names = 'TRUE')

ta_combined_df <- read_csv(ta_file_names[1]) %>%
  select(
    'Tree ID',
    X,
    Y,
    Z,
    'DBH',
    'Crown Diameter',
    'Crown Area',
    'Crown Volume',
    'Tree Height',
    CBH,
    Straightness
  ) %>%
  mutate_at('CBH', as.numeric) %>%
  add_column('ta_plot' = str_extract(ta_file_names[1], pattern = '(?<=p)[:digit:]+'))

for (ta_file in ta_file_names[-1]) {
  ta_single_df <- read_csv(ta_file) %>%
    select(
      'Tree ID',
      X,
      Y,
      Z,
      DBH,
      'Crown Diameter',
      'Crown Area',
      'Crown Volume',
      'Tree Height',
      CBH,
      Straightness
    ) %>%
    mutate_at('CBH', as.numeric)  %>%
    add_column('ta_plot' = str_extract(ta_file, pattern = '(?<=p)[:digit:]+'))
    
  ta_combined_df <- ta_combined_df %>%
    add_row(ta_single_df)
  
}

write.csv(ta_combined_df, ta_combined_output)

# ======================== Combine with biomass dataset =========================

bm_df <- read_csv(bm_file) %>%
  mutate_at('Tag', as.numeric)

ta_combined_df <-  ta_combined_df %>%
  rename(Tag = 'Crown Diameter') %>%
  mutate_at('Tag', as.numeric)

bm_df <- bm_df %>%
  full_join(ta_combined_df, by = 'Tag')

write.csv(bm_df, bm_output)

# ===============================================================================
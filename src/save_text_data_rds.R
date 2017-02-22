#----------------------------------------------------------------------------
#
# R script to read the raw-data files for
# later processing. The rds file is smaller than the original text files
# ands can be loaded in about 7 seconds.
#
# Input files: raw-data/final/en_US/en_US.[blogs|news|twitter]\.txt
# [deprecated] Output file: data/text_data.rds
#
# No output file is created as readr::read_lines is much faster than
# compared to readLines.
#
#----------------------------------------------------------------------------

source('src/globals.R')

library(tidyverse)
library(tidytext)
library(readr)

path = 'raw-data/final/en_US/'
files <- c('en_US.blogs.txt', 'en_US.news.txt', 'en_US.twitter.txt')
files_types <- c('blogs', 'news', 'twitter')

# files <- c('en_US.blogs.txt', 'en_US.news.txt', 'en_US.twitter.txt')
# 
# # read all data files
# n <- if_else(rep(use_small_data, 3), c(10000L, 15000L, 30000L), c(-1L, -1L, -1L))
# names(n) <- files
# text_data <-lapply(files, function(x) read_lines(paste0(path, '/', x), n_max = n[[x]], progress = interactive())) #, skipNul = TRUE))
# names(text_data) <- c('blogs', 'news', 'twitter')

# 
# read_folder <- function(infolder) {
#   data_frame(file = dir(infolder, full.names = TRUE)) %>%
#     mutate(text = map(file, read_lines)) %>%
#     transmute(id = basename(file), text) %>%
#     unnest(text)
# }

# make text data frame with these columns:
# source = blogs|news|twitter
# line = line number from the original file
# text = text lines from file

# n <- -1L
n <- if_else(rep(use_small_data, 3), c(10000L, 15000L, 30000L), c(-1L, -1L, -1L))
raw_text <- data_frame(file = dir(path, full.names = TRUE)) %>%
  mutate(source = gsub('(.*\\.)(.*)(\\..*)', '\\2', basename(file))) %>%
  unnest(text = map2(file, n, function(f, n) { read_lines(f, n_max = n, progress = interactive()) })) %>%
  select(source, text)

#   seq_along(text_data) %>%
#   lapply(
#     function(i) {
#       n <- length(text_data[[i]])
#       list(
#         source = rep(names(text_data)[i], n),
#         line = 1:n,
#         text = read_lines(paste0(path, '/', x), n_max = n[[x]], progress = interactive())
#       )
#     }
#   ) %>%
#   bind_rows()
# 
# # free memory
# rm(text_data)
# gc()

# save to RDS format for faster loading
# write_rds(text_data, paste0('data/text_data', ifelse(use_small_data, '_small', ''), '.rds'))
# cat(paste0('Removed writing rds. ', 'Previous data/text_data', ifelse(use_small_data, '_small', ''), '.txt file not created.'))

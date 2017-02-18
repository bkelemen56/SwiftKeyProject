#----------------------------------------------------------------------------
#
# R script to read the raw-data files and save into one .rds file for
# later processing. The rds file is smaller than the original text files
# ands can be loaded in about 7 seconds.
#
# Input files: raw-data/final/en_US/en_US.[blogs|news|twitter]\.txt
# Output file: data/text_data.rds
#
#----------------------------------------------------------------------------

path = 'raw-data/final/en_US'
files <- c('en_US.blogs.txt', 'en_US.news.txt', 'en_US.twitter.txt')

# read all data files
text_data <-lapply(files, function(x) readLines(paste0(path, '/', x), skipNul = TRUE))
names(text_data) <- c('blogs', 'news', 'twitter')

# save to RDS format for faster loading
saveRDS(text_data, 'data/text_data.rds')

#----------------------------------------------------------------------------
#
# Quiz #1
#
#----------------------------------------------------------------------------

source('util/general_util.R')
source('util/text_util.R')

text_file_blogs <- 'data/final/en_US/en_US.blogs.txt'
text_file_news <- 'data/final/en_US/en_US.news.txt'
text_file_twitter <- 'data/final/en_US/en_US.twitter.txt'

path = 'data/final/en_US/'
files <- c('en_US.blogs.txt', 'en_US.news.txt', 'en_US.twitter.txt')

text_data <- load_files(files, path)
name(text_data) <- c('blogs', 'news', 'twitter')
  
questions <- c(1:6)   # which questions to execute?

if (1 %in% questions) {
  cat("Question #1:\n")
  fi <- file.info(text_file_blogs)
  size <- fi$size[1]
  cat(paste('Size of', 'en_US.blogs.txt', fmt_int_or_decimal(size / 1024 / 1024, 2)), '\n')
  cat('\n')
}

if (2 %in% questions) {
  cat("Question #2:\n")
  n <- nlines(text_file_twitter)
  cat(paste('Number of lines in', 'en_US.twitter.txt', as.character(n)), '\n')
  cat('\n')
}

if (3 %in% questions) {
  cat('Question #3:\n')
  
  n1 <- max_len_lines(text_file_twitter)
  n2 <- max_len_lines(text_file_blogs)
  n3 <- max_len_lines(text_file_news)
  
  cat(paste('Max length lines in', 'twitter', 'text file', as.character(n1)), '\n')
  cat(paste('Max length lines in', 'blogs', 'text file', as.character(n2)), '\n')
  cat(paste('Max length lines in', 'news', 'text file', as.character(n3)), '\n')
  
  cat('\n')
}

if (4 %in% questions) {
  cat('Question #4:\n')
  
  lines <- load_file(text_file_twitter)
  
  love <- grep('love', lines)
  cat(paste("Number of lines with 'love' in", 'twitter', 'text file', as.character(length(love))), '\n')
  
  hate <- grep('hate', lines) 
  cat(paste("Number of lines with 'hate' in", 'twitter', 'text file', as.character(length(hate))), '\n')
  
  cat(paste("Ratio love:hate in", 'twitter', 'text file', fmt_int_or_decimal(length(love) / length(hate), 2)), '\n')
  
  cat('\n')
  
  # do cleanup
  love <- NULL
  hate <- NULL
  lines <- NULL
  gc()
}

if (5 %in% questions) {
  cat('Question #5:\n')
  
  lines <- load_file(text_file_twitter)
  
  i <- grep('biostats', lines)
  print(lines[i])
  
  cat('\n')
  
  # do cleanup
  lines <- NULL
  gc()
}

if (6 %in% questions) {
  cat('Question #6:\n')
  
  lines <- load_file(text_file_twitter)
  
  i <- grep('A computer once beat me at chess, but it was no match for me at kickboxing', lines)
  print(length(i))
  
  cat('\n')
  
  # do cleanup
  lines <- NULL
  gc()
}


# ---------------------------------------------------------------------
# make train, test and validation data sets
#
# the following combination are created:
# 1) small: only 1000/1500/3000 lines from blogs, news and twitter
# 2) 0.01-0.05: 1%-5% of each file
# ---------------------------------------------------------------------

library(readr)

# ---------------------------------------------------------------------
# Load documents
# ---------------------------------------------------------------------
file_types <- c('blogs', 'news', 'twitter')

make_small_dataset <- FALSE

# creates the small dataset
create_small_datasets <- function(raw_text, file_type) {
  cat('writing small sample documents\n')
  
  n <- c(1000L, 1500L, 3000L)
  names(n) <- file_types
  
  m <- 1
  nlines <- n[[file_type]]
    
  for (dataset in c('train', 'test', 'validate')) {
    out_file <- paste0('data/', file_type, '.', dataset, '.small.txt')
    writeLines(raw_text[m:(m+nlines-1)], out_file)
    
    m <- m + nlines
    nlines <- n[[file_type]] / 2
  }
}

# creates datasets as random % number of lines.
# % referes to the number of lines in train. test/validate get half each
create_pct_dataset <- function(raw_text, file_type, pct) {
  cat('writing ', pct, '% sample documents\n')
  
  n <- length(raw_text)
  pct_lines <- as.integer(n * pct)
  s <- sample(1:n, min(n, pct_lines * 2))  # need double for train (100%), test (50%) and validation (50%)
  
  m <- 1
  nlines <- pct_lines
  if (pct <= .5) {
    datasets <- c('train', 'test', 'validate')
  } else {
    datasets <- c('train')
  }
  for (dataset in datasets) {
    out_file <- paste0('data/', file_type, '.', dataset, '.', format(pct, decimal.mark = '_'), '.txt')
    writeLines(raw_text[s][m:(m+nlines-1)], out_file)
    
    m <- m + nlines
    nlines <- pct_lines / 2
  }
}

# main loop
for (file_type in file_types) {
  cat('\nprocessing', file_type, '\n')
  
  in_file <- paste0('raw-data/final/en_US/en_US.', file_type, '.txt')
  raw_text <- read_lines(in_file, progress = interactive())
  
  # create small datasets
  if (make_small_dataset) {
    create_small_datasets(raw_text, file_type)
  }
  
  # create pct datasets
  # options c(.10, .15, .20, .25)
  #for (pct in c(.50)) create_pct_dataset(raw_text, file_type, pct)
  #for (pct in c(.06, .07, .08, .09)) create_pct_dataset(raw_text, file_type, pct)
  for (pct in c(.75, 1.00)) create_pct_dataset(raw_text, file_type, pct)
}

cat('\nend program')

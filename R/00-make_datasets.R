# ---------------------------------------------------------------------
# make train, test and validation data sets
#
# the following combination are created:
# 1) small: only 1000/1500/3000 lines from blogs, news and twitter
# 2) 0.01-0.05: 1%-5% of each file
#
# v1.2 - create incrementally 1% files to the top training dataset
# ---------------------------------------------------------------------

library(readr)

file_types <- c('blogs', 'news', 'twitter')
datasets <- c('train', 'test', 'validate')

make_small_dataset <- FALSE

train_pct    <- .8        # use up to 80% for training
test_pct     <- .1        # use 10% for testing
validate_pct <- .1        # use 10% for validation

pct_inc      <-.01        # process incrementally in 1% chunks

# ---------------------------------------------------------------------
# create small dataset
# ---------------------------------------------------------------------

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

# ---------------------------------------------------------------------
# create a % dataset
# ---------------------------------------------------------------------

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

# ---------------------------------------------------------------------
# create a % dataset in smaller file chunks
#
# returns a list of one element x file_type with the number of chunks
# created
# ---------------------------------------------------------------------

create_datasets <- function(raw_text, file_type, 
                            train_pct = .8, test_pct = .1, validate_pct = .1, pct_inc = .01) {
  cat('writing datasets\n')
  
  pct_datasets <- list(train = train_pct, test = test_pct, validate = validate_pct)

  chunks <- list()
  n <- length(raw_text)
  s <- sample.int(n)       # random permutation of all text lines
  
  m <- 1
  for (dataset in datasets) {
    cat(paste0("  ", dataset))
    
    pct <- pct_datasets[[dataset]]        # % of (sampled) input file to read and write out
    n_chunks <- pct / pct_inc             # number of cycles to create smaller files
    n_lines <- as.integer(n * pct / n_chunks)    # number of lines in each smaller file
    
    chunks[[dataset]] <- n_chunks
      
    for (i in c(1:n_chunks)) {
      cat(".")
      out_file <- paste0('data/', file_type, 
                         '.', dataset, 
                         '-', format_fixed_width(i, 3),
                         '.txt')
      write_lines(raw_text[s][m:(m+n_lines-1)], out_file)
      m <- m + n_lines
    }
    cat("\n")
  }
  
  #cat(paste0('finished. n=', n, ' m=', m, '\n'))
  
  chunks
}

format_fixed_width <- function(i, width) {
  s <- paste(i)
  while (str_length(s) < width) s <- paste0("0", s)
  s
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
  #for (pct in c(.75, 1.00)) create_pct_dataset(raw_text, file_type, pct)
  
  chunks <- create_datasets(raw_text, 
                            file_type, 
                            train_pct = train_pct, 
                            test_pct = test_pct, 
                            validate_pct = validate_pct, 
                            pct_inc = pct_inc)
}

#print(chunks)

# concat all sub-files for each chunch into an "all.<dataset>-nnn.txt" file
cat("creating all-<dataset>-nnn.txt files:\n")

system2(
  "rm",
  args = c("data/all.*-*"),
  stdout = TRUE
)

for (dataset in datasets) {
  cat(paste0("  cat ", dataset, " files"))
  n_chunks <- chunks[[dataset]]
  for (i in (1:n_chunks)) {
    cat(".")
    system2(
        "cat",
        args = c(paste0("data/*.", dataset, "-", format_fixed_width(i, 3), ".*", 
                        " > ",
                        "data/all.", dataset, "-", format_fixed_width(i, 3), ".txt")),
        stdout = TRUE
    )
  }
  cat("\n")
}

cat('\nend program')

# ---------------------------------------------------------------------
# final model
# make train, test and validation data sets
#
# script to create train, test and validation datasets from all the
# documents provided (blogs, news and twitter feeds). the datasets
# are created by random sampling the raw data. the training dataset will
# use 80%, test 10% and validation 10%. Each dataset is further
# divided into 1% chucks to ease development and overall processing.
#
# a small dataset is also created to be used in development.
# ---------------------------------------------------------------------

source('R/globals.R')

library(readr)

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

file_types <- c('blogs', 'news', 'twitter')
datasets <- c('train', 'test', 'validate')

make_small_dataset <- FALSE

train_pct    <- .8        # use up to 80% for training
test_pct     <- .1        # use 10% for testing
validate_pct <- .1        # use 10% for validation

pct_inc      <-.01        # process incrementally in 1% chunks

set.seed(1234)

# ---------------------------------------------------------------------
# create dataset functions
# ---------------------------------------------------------------------

#' creates the small dataset.
#' 
#' \code{create_small_datasets} creates small datasets intended to test
#'   the programs while in development. these datasets should not be used
#'   for actual model validations as they don't represent a valid sample
#'   of the population.
#'
#' @param raw_text text read from the raw text files provided 
#' @param file_type should be "blogs", "news" or "twitter"
#' @return NONE 
#' @examples
#'   \code{create_small_datasets(raw_text, "blogs")} will write 
#'   \code{blogs.[train|test|validate].small.txt} to the 
#'   PATH_DATA folder.
#'   
create_small_datasets <- function(raw_text, file_type) {
  flog.info('creating small sample documents')
  
  n <- c(1000L, 1500L, 3000L)
  names(n) <- file_types
  
  m <- 1
  nlines <- n[[file_type]]
    
  for (dataset in c('train', 'test', 'validate')) {
    out_file <- paste0(PATH_DATA, file_type, '.', dataset, '.small.txt')
    writeLines(raw_text[m:(m+nlines-1)], out_file)
    
    m <- m + nlines
    nlines <- n[[file_type]] / 2
  }
}

#' creates valid datasets to train, test and validate the model.
#' 
#' \code{create_datasets} creates the train, test and validate datasets.
#'   each dataset is further divided into 1% chucks to ease development 
#'   and overall processing.
#'
#' @param raw_text text read from the raw text files provided 
#' @param file_type should be "blogs", "news" or "twitter"
#' @return list containning one element by dataset type with the number of 
#'   chunks created 
#' @examples
#'   \code{create_small_datasets(raw_text, "blogs")} will write 
#'   \code{blogs.[train|test|validate].small.txt} to the 
#'   PATH_DATA folder.
#'   
create_datasets <- function(raw_text, file_type, 
                            train_pct = .8, test_pct = .1, validate_pct = .1, pct_inc = .01) {
  flog.info(paste('creating train, test and validation datasets for', file_type))
  
  pct_datasets <- list(train = train_pct, test = test_pct, validate = validate_pct)

  chunks <- list()
  n <- length(raw_text)
  s <- sample.int(n)       # random permutation of all text lines
  
  m <- 1
  for (dataset in datasets) {
    pct <- pct_datasets[[dataset]]        # % of (sampled) input file to read and write out
    n_chunks <- pct / pct_inc             # number of cycles to create smaller files
    n_lines <- as.integer(n * pct / n_chunks)    # number of lines in each smaller file
    
    chunks[[dataset]] <- n_chunks
      
    flog.info(paste0("  ", dataset, " [", n_chunks, " chunks]"))
    
    for (i in c(1:n_chunks)) {
      #cat(".")
      out_file <- paste0(PATH_DATA, file_type, 
                         '.', dataset, 
                         '-', str_pad(i, 3, side = "left", "0"),
                         '.txt')
      write_lines(raw_text[s][m:(m+n_lines-1)], out_file)
      m <- m + n_lines
    }
    #cat("\n")
  }
  
  chunks
}

# ---------------------------------------------------------------------
# main
# ---------------------------------------------------------------------

# initializde the logger and start logging...
init_logger(threshold = DEBUG, filename = "make_datasets", timestamp = TRUE, tee = TRUE)
flog.info("start: make_datasets")

# main loop
for (file_type in file_types) {
  flog.info(paste('processing', file_type))
  
  in_file <- paste0(PATH_RAW_DATA, "en_US.", file_type, '.txt')
  raw_text <- read_lines(in_file, progress = FALSE)
  
  # create small datasets
  if (make_small_dataset) {
    create_small_datasets(raw_text, file_type)
  }
  
  # create pct datasets
  #if (FALSE)
  chunks <- create_datasets(raw_text, 
                            file_type, 
                            train_pct = train_pct, 
                            test_pct = test_pct, 
                            validate_pct = validate_pct, 
                            pct_inc = pct_inc)
}

# concat all sub-files for each chunch into an "all.<dataset>-nnn.txt" file
flog.info("creating all.[train|test|validate]-###.txt files")

flog.debug("deleting previous all.*-*.txt files")
system2(
  "rm",
  args = c(paste0(DIR_DATA, "all.*-*.txt")),
  stdout = TRUE
)

flog.debug("concatenating chunks into all.[train|test|validate]-???.txt files")
for (dataset in datasets) {
  flog.debug(paste0("  concatenating ", dataset, " files"))
  n_chunks <- chunks[[dataset]]
  for (i in (1:n_chunks)) {
    n_file <- str_pad(i, 3, side = "left", "0")
    system2(
      "cat",
      args = c(paste0(DIR_DATA, "*.", dataset, "-", n_file, ".txt"), 
              " > ",
              paste0(DIR_DATA, "all.", dataset, "-", n_file, ".txt")),
      stdout = TRUE
    )
  }
}

flog.debug("deleting [blogs|news|twitter].*.txt files")
for (file_type in file_types) {
  flog.debug(paste0("  deleting ", file_type, " files"))
  system2(
    "rm",
    args = c(paste0(DIR_DATA, file_type, ".*.txt")),
    stdout = TRUE
  )
}

flog.info("end: make_datasets")

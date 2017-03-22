# ---------------------------------------------------------------------
# model #5.0 - final model
# make model - part 1
#
# script to process the training dataset and train 80 separate models,
# each on 1% of the total data. these separate models will be combined
# later via the make_model_part_2 script into the final model. 
# ---------------------------------------------------------------------

source('R/globals.R')
source('R/model_util.R')

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

use_small_data <- FALSE
debug          <- TRUE          # to only test loops, no file processing
do_parallel    <- FALSE           # run in parallel (no console output) - only in production
n_prune        <- 5              # number of words to keep for same root

# ---------------------------------------------------------------------
# functions
# ---------------------------------------------------------------------

#' creates the model from one of the training files.
#' 
#' \code{process_file} reads one of the training files, cleans the 
#'   text data, calculates the model and saved it to cache. This
#'   function is intended to be called in a for loop to process all
#'   80 sections of the training dataset.
#'
#' @param i document number to read
#' @return NONE 
#' @examples
#'   \code{process_file(1)} will read \code{all.train.001.txt} from
#'   the PATH_DATA folder and create \code{model_XXX.001.cache} in
#'   the PATH_CACHE folder.
#'   
process_file <- function(i) {
  file_number <- str_pad(i, 3, side = "left", "0")
  doc_filename <- paste0("all.train-", file_number, ".txt")
  model_filename <- paste0(MODEL_ID, ".", file_number, ".cache")
  
  flog.info(paste("processing file", doc_filename))
  
  load_documents(doc_filename) %>% 
    clean_documents() %>%
    calc_model(n_prune = n_prune, do_mem_size_calc = TRUE) %>%
    save_model_to_cache(model_filename)

  flog.info(paste("completed processing file", doc_filename))
  
  invisible()
}

# ---------------------------------------------------------------------
# main
# ---------------------------------------------------------------------

# initializde the logger and start logging...
init_logger(threshold = DEBUG, filename = "make_model_part_1", timestamp = TRUE, tee = TRUE)
flog.info("start: make_model_part_1")

if (debug) {
  flog.info("start: debug computing")
  process_file(1)
  flog.info("end: debug computing")
  
} else if (do_parallel) {
  flog.info("start: parallel computing")
  library(doParallel)
  
  no_cores <- detectCores() - 1  
  cl <- makeCluster(no_cores, type = "FORK")  
  registerDoParallel(cl) # ?no_cores)  

  flog.info(paste("computing with", no_cores, "cores"))
  foreach(i = 1:80) %dopar% process_file(i)

  stopCluster(cl)
  flog.info("end: parallel computing")
  
} else {
  flog.info("start: sequential computing")
  for (i in c(1:80)) process_file(i)
  flog.info("end: sequential computing")
}

flog.info("end: make_model_part_1")

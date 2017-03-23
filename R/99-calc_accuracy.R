# ---------------------------------------------------------------------
# model #5.0 - final model
# make model - part 2
#
# script to process the training dataset and train 80 separate models,
# each on 1% of the total data. these separate models will be combined
# later via the make_model_part_2 script into the final model. 
# ---------------------------------------------------------------------

source('R/globals.R')
source('R/model_util.R')

library(doParallel)
library(foreach)

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

# model to validate
model_fname <- paste0(MODEL_ID, ".001-a.cache")

# how many lines to read from test file(s) to calculate accuracy?
# after processing most of the test files, i noticed that with only
# 200 lines we get the same accuracy estimate
n_lines_to_process <- 200

# parameters to predict word
use_unigram_model <- TRUE
discount_factor <- 0.5

# use_small_data <- FALSE
debug       <- FALSE           # to only test loops, no file processing
do_parallel <- TRUE           # run in parallel (no console output) - only in production
n_prune     <- 5              # number of words to keep for same root

# ---------------------------------------------------------------------
# functions
# ---------------------------------------------------------------------

#' calculates the accuracy of a model and a test file number.
#' 
#' \code{calc_accuracy} calculates the accuracy of a model and
#'   predict_words algorithm on a test file indicated.
#'
#' @param model model to validate.
#' @param i test file number to use for model accuracy.
#' @param ... parameters passed to "model_accuracy(...)" call.
#' @return list of accuracy results 
#' @examples
#'   \code{calc_accuracy(2, use_unigram_model = use_unigram_model)} will 
#'   calcualte the accuracy the model on \code{data/all.test-002.txt}
#'   file using unigrams in the katz backoff algorithm.
#'   
calc_accuracy <- function(model, i, ...) {
  flog.trace("start:calc_accuracy")
  
  test_fname <- paste0("all.test-", str_pad(i, 3, "left", "0"), ".txt")
  flog.info(paste("calculating accuracy on", test_fname))
  accuracy <- model_accuracy(model, test_fname, ...)
  log_accuracy(paste("accuracy on:", test_fname), accuracy)
  
  flog.trace("end:calc_accuracy")
  accuracy
}

#' calculates the accuracy of a model saved in cache.
#' 
#' \code{calc_model_accuracy} calculates the accuracy of a model 
#'   stored in the cache folder.
#'
#' @param model_fname model filename to validate.
#' @param ... parameters passed to "model_accuracy(...)" call.
#' @return list of accuracy results 
#' @examples
#'   \code{calc_model_accuracy(paste0(MODEL_ID, ".001-a.cache"), 
#'   use_unigram_model = use_unigram_model)} will calcualte the accuracy the 
#'   model on test files using unigrams in the katz backoff 
#'   algorithm.
#'   
calc_model_accuracy <- function(model_fname, ...) {
  flog.trace("start:calc_model_accuracy")
  flog.info("calculating accuracy of a model")
  
  # load model from cache
  flog.info(paste("using model", model_fname))
  model <- load_model_from_cache(model_fname)
  
  if (debug) {
    flog.info("start: debug computing")
    accuracy <- calc_accuracy(model, 1, n_lines_to_process = 100, ...)
    flog.info("end: debug computing")
    
  } else if (do_parallel) {
    flog.info("start: parallel computing")

    no_cores <- detectCores() - 1  
    cl <- makeCluster(no_cores, type = "FORK")  
    registerDoParallel(cl)
    
    flog.info(paste("computing with", no_cores, "cores"))
    # in parallel only compute max non_cores chunks 
    # (it takes too long otherwise)
    accuracy <- foreach(i = 1:(max(no_cores, 10))) %dopar% 
      calc_accuracy(model, i, n_lines_to_process = n_lines_to_process, ...)
    
    stopCluster(cl)
    flog.info("end: parallel computing")
    
  } else {
    # sequencial only use 1 chunk
    accuracy <- foreach(i = 1:1) %do% 
      calc_accuracy(model, i, n_lines_to_process = n_lines_to_process, ..)
  }
  
  log_accuracy("final accuracy:", reduce(accuracy, reduce_accuracy))
  
  flog.trace("end:calc_model_accuracy")
  invisible()
}

# ---------------------------------------------------------------------
# main
# ---------------------------------------------------------------------

# initialize the logger and start logging...
init_logger(threshold = DEBUG, filename = "calc_accuracy", timestamp = TRUE, tee = TRUE)
flog.info("start: calc_accuracy")

# we could iterate here over various models...
calc_model_accuracy(model_fname, 
                    discount_factor = discount_factor, 
                    use_unigram_model = use_unigram_model)

flog.info("end: calc_accuracy")

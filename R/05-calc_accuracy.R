# ---------------------------------------------------------------------
# model #5.0 - final model
# make model - part 2
#
# script to process the training dataset and train 80 separate models,
# each on 1% of the total data. these separate models will be combined
# later via the make_model_part_2 script into the final model. 
# ---------------------------------------------------------------------

source('R/globals.R')
source('R/bad_words.R')
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
max_model_level <- 4

# use_small_data <- FALSE
debug       <- FALSE           # to only test loops, no file processing
do_parallel <- TRUE           # run in parallel (no console output) - only in production
n_prune     <- 5              # number of words to keep for same root

# ---------------------------------------------------------------------
# main
# ---------------------------------------------------------------------

# initialize the logger and start logging...
init_logger(threshold = DEBUG, filename = "calc_accuracy", timestamp = TRUE, tee = TRUE)
flog.info("start: calc_accuracy")

cat('calculate best model accuracy:\n\n')

# load model from cache
postfix <- readline(prompt = "which models to test [a-g] /a-g/? ")
if (postfix == "") postfix <- "abcdefg"

flog.info(paste0("use_unigram_model = ", use_unigram_model))
flog.info(paste0("discount_factor = ", discount_factor))
flog.info(paste0("max_model_level = ", max_model_level))

best_accuracy <- list(accuracy = 0)
best_model <- NULL
results <- data.table(model_fname = character(), accuracy = double())

for (ch in tokenize_characters(postfix)[[1]]) {
  model_fname <- paste0(MODEL_ID, ".001-", ch, ".cache")
  accuracy <- calc_model_accuracy(model_fname, 
                                  n_lines_to_process = ifelse(debug, 10, n_lines_to_process),
                                  discount_factor = discount_factor, 
                                  use_unigram_model = use_unigram_model,
                                  max_model_level = max_model_level)

  if (accuracy$accuracy > best_accuracy$accuracy) {
    best_model <- model_fname
    best_accuracy <- accuracy
  }
  
  # save results
  results <- rbind(results, 
                   data.table(model_fname = model_fname, accuracy = accuracy$accuracy))
}

flog.info(paste0("best model = ", best_model))
log_accuracy("best accuracy:", best_accuracy)

flog.info("all accuracies:", results, capture = TRUE)

flog.info("end: calc_accuracy")

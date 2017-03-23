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
debug       <- TRUE           # to only test loops, no file processing
do_parallel <- TRUE           # run in parallel (no console output) - only in production
n_prune     <- 5              # number of words to keep for same root

# ---------------------------------------------------------------------
# main
# ---------------------------------------------------------------------

# initialize the logger and start logging...
init_logger(threshold = DEBUG, filename = "calc_accuracy", timestamp = TRUE, tee = TRUE)
flog.info("start: calc_accuracy")

# we could iterate here over various models...
calc_model_accuracy(model_fname, 
                    n_lines_to_process = ifelse(debug, 100, n_lines_to_process),
                    discount_factor = discount_factor, 
                    use_unigram_model = use_unigram_model)

flog.info("end: calc_accuracy")

# ---------------------------------------------------------------------
# model #4.1 - 
# 
# script to compute read model-4 1-gram and n-gram files and write out
# one cached file with the whole model
# ---------------------------------------------------------------------

library(data.table)
library(tidyverse)

# ---------------------------------------------------------------------
# combine models
#
# model_id            prefix for files to cache (e.g. "model-3")
# do_mem_size_calc    calcualte and print size of each model?
# ---------------------------------------------------------------------

model_fname <- function(model_id_key, ngram_type = NULL) {
  paste0(model_id_key, 
         ifelse(is.null(ngram_type) | ngram_type == "", "", paste0('.', ngram_type)), 
         '.cache')
}

load_from_cache <- function(model_id_key, ngram_type = NULL) {
  filename <- paste0('cache/', model_fname(model_id_key, ngram_type))
  if (file.exists(filename)) {
    model <- read_rds(filename)
  } else {
    model <- NULL
  }
  model
}

save_to_cache <- function(model, model_id_key, ngram_type = NULL) {
  filename <- paste0('cache/', model_fname(model_id_key, ngram_type))
  write_rds(model, filename)
}

combine_models <- function(model_id_old, model_id_new, use_small_data = TRUE, use_pct_data = 0.01) {

  # make key for cache file
  model_id_key <- function(model_id) {
    paste0(model_id, ".",
           ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_')))
  }
  
  model_id_old_key <- model_id_key(model_id_old)
  model_id_new_key <- model_id_key(model_id_new)
    
  cat('combining models [', model_id_old_key, "-->", model_id_new_key, ']\n')
  
  # combine models
  dt.1 <- load_from_cache(model_id_old_key, "1-gram")
  dt.n <- load_from_cache(model_id_old_key, "n-gram")
  models <- list(model.1 = dt.1, model.n = dt.n)
  
  save_to_cache(models, model_id_new_key)

  models
}

# main program

do_combine <- FALSE
if (do_combine) {
  # process all 4.0 models
  combine_models("model-4", "model-4-1", use_small_data = TRUE)
  
  for (pct in c(.01, .02, .03, .04, .05, .1, .15, .2, .25)) {
    combine_models("model-4", "model-4-1", use_small_data = FALSE, use_pct_data = pct)
  }
}


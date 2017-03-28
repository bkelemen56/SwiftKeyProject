# ---------------------------------------------------------------------
# model #4.1 - learn from 4-grams, trigrams and bigrams using katz backoff process
#
# model data storage has been optmized: n-grams stored as hashed values,
# words indexed, data.table for faster retrieval. All i stored in one
# file "model-4-1.xxxxx.cache"
#
# the interactive process reads a sentince and predicts each word.
# a list of words predicted are displayed.
#
# ---------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(FeatureHashing)

debugSource("~/R Workspace/data science class/10- capstone/SwiftKeyProject/src/model_4_4_util.R")

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

model_id <- "model-4-4"
model_fname <- paste0(model_id, ".001-f.cache")

n_prune <- 5
do_accuracy_calc <- FALSE

# discount factor for katz backoff model
discount_factor <- 0.5
use_unigram_model <- FALSE # TRUE will also use the unigram in the backoff
max_model_level <- 4       # start with this level for backoff

# accepts a complete sentence
predict_sentence <- function(s) {
  s <- char_tolower(s)
  tok <- quanteda::tokenize(s)[[1]]
  
  s <- paste(tok[1], tok[2], tok[3])
  m <- 4
  result <- s
  while (m <= (length(tok)+1)) {
    dt_words <- predict_words(model, s, discount_factor)

    if (m > length(tok)) {
      # final prediction
      next_word <- paste0('\n[', pretty_fmt(model[[1]], dt_words), ']')
    } else if (nrow(dt_words) == 0 || dt_words[1, word] != tok[m]) {
      # incorrect prediction
      next_word <- paste0('\n{', tok[m], ', !', pretty_fmt(model[[1]], dt_words), '}')
    } else {
      # correct prediction
      next_word <- paste0('\n', dt_words[1, word])
    }
    
    result <- paste(result, next_word)
    
    s <- paste(tok[m-2], tok[m-1], tok[m])
    m <- m+1 
  }
  
  result
}

print_predict <- function(word) {
  if (!is.null(word)) {
    cat('prediction: ', word)
  } else {
    cat("can't predict - no match")
  }
}

# ---------------------------------------------------------------------
# main interactive loop
# ---------------------------------------------------------------------

# load model from cache
model <- load_model_from_cache(model_fname)

# check accuracy?
if (do_accuracy_calc) {
  cat("accuracy calculation\n")
  
  accuracy_train <- model_accuracy(model, "train")
  accuracy_test <- model_accuracy(model, "test")
}

# predict interactively
cat('\nprediction:\n')
repeat {
  cat('\n')
  s <- readline(prompt = "enter three or more words: ")
  if (s == '') {
    break
  } else {
    # clean s
    s <- clean_text(s)
    prediction <- predict_sentence(s)
    print_predict(prediction)
  }
}

cat('\nend program')
